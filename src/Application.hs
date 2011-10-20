{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleContexts #-}


module Application where

import            Control.Applicative
import            Control.Monad
import            Control.Monad.Reader
import            Control.Monad.State
import            Data.Aeson as AE
import            Data.ByteString.Char8 (ByteString)
import qualified  Data.ByteString.Char8 as BS
import            Data.Lens.Template
import            Data.Map (Map)
import qualified  Data.Map as DM
import            Data.Maybe
import            Data.Text (Text)
import qualified  Data.Text as DT
import qualified  Data.Text.Encoding as DT
import            Database.HDBC.Sqlite3
import            JCU.Prolog
import            JCU.Templates
import            JCU.Types
import            Language.Prolog.NanoProlog.NanoProlog
import            Snap.Core
import            Snap.Snaplet
import            Snap.Snaplet.Auth
import            Snap.Snaplet.Auth.Backends.Hdbc
import            Snap.Snaplet.Hdbc
import            Snap.Snaplet.Session
import            Snap.Snaplet.Session.Backends.CookieSession
import            Snap.Util.FileServe
import            Text.Blaze
import            Text.Blaze.Renderer.Utf8 (renderHtml)
import            Text.Digestive
import            Text.Digestive.Blaze.Html5
import            Text.Digestive.Forms.Snap
import qualified  Text.Email.Validate as E


data App = App
  {  _authLens  :: Snaplet (AuthManager App)
  ,  _sessLens  :: Snaplet SessionManager
  ,  _dbLens    :: Snaplet (HdbcSnaplet Connection)
  }

makeLens ''App

type AppHandler = Handler App App

instance HasHdbc AppHandler Connection where
  getPool = with dbLens $ gets hdbcPool

jcu :: SnapletInit App App
jcu = makeSnaplet "jcu" "Prolog proof tree practice application" Nothing $ do
  addRoutes  [  ("/",           siteIndex)
             ,  ("/forbidden",  forbiddenH)
             ,  ("/login",   loginH)
             ,  ("/logout",  logoutH)
             ,  ("/signup",  signupH)
             ,  ("/rules/stored",  method GET   readStoredRulesH)
             ,  ("/rules/stored",  method POST  addStoredRuleH)
             ,  ("/rules/stored/:id",  method DELETE  deleteStoredRuleH)
             ,  ("/proof/check",   method POST  checkProofH)
             ,  ("/rules/unify",   method POST  unifyH)
             ,  ("/load-example",  method GET loadExampleH)
             ,  ("/check-syntax/:type",  method POST checkSyntaxH)
             ,  ("/subst/:sub/:for",     method POST substH)
             ,  ("", serveDirectory "resources/static")
             ]
  _sesslens'  <- nestSnaplet "session" sessLens $ initCookieSessionManager
                   "config/site_key.txt" "_session" Nothing
  let sqli = connectSqlite3 "resources/jcu.db"
  _dblens'    <- nestSnaplet "hdbc" dbLens $ hdbcInit sqli
  _authlens'  <- nestSnaplet "auth" authLens $ initHdbcAuthManager
                   defAuthSettings sessLens sqli defAuthTable defQueries
  return  $ App _authlens' _sesslens' _dblens'


restrict :: AppHandler b -> AppHandler b -> AppHandler b
restrict failH succH = do
  with sessLens touchSession
  authed <- with authLens $ isLoggedIn
  if authed
    then succH
    else failH

loginRedir :: AppHandler ()
loginRedir = redirect "/login"

forbiddenH :: AppHandler a
forbiddenH = do
  modifyResponse $ setResponseStatus 403 "Forbidden"
  writeBS "403 forbidden"
  r <- getResponse
  finishWith r

------------------------------------------------------------------------------
-- | Renders the front page of the sample site.
--
-- The 'ifTop' is required to limit this to the top of a route.
-- Otherwise, the way the route table is currently set up, this action
-- would be given every request.
siteIndex :: AppHandler ()
siteIndex = ifTop $ restrict loginRedir $ (blaze $ template index)

-- TODO: Remove
{- loginH :: AppHandler ()-}
{- loginH = loginHandler "password" (Just "remember") failedLogin redirHome-}
  {- where failedLogin _ = blaze $ template False login-}

loginH :: AppHandler ()
loginH = withSession sessLens $ do
  loggedIn <- with authLens isLoggedIn
  when loggedIn $ redirect "/"
  res <- eitherSnapForm registrationForm "registration-form"
  case res of
    Left form' -> do
      didFail <- with sessLens $ do
        failed <- getFromSession "login-failed"
        deleteFromSession "login-failed"
        commitSession
        return failed
      blaze $ template $ loginHTML (isJust didFail) form'
    Right (FormUser e p r) -> do
      loginRes <- with authLens $ loginByUsername (DT.encodeUtf8 e) (ClearText $ DT.encodeUtf8 p) r
      case loginRes of
        Left _   ->  do  with sessLens $ do
                           setInSession "login-failed" "1"
                           commitSession
                         redirect "/login"
        Right _  ->  redirect "/"

logoutH :: AppHandler ()
logoutH = do
  with authLens logout
  redirect "/"


------------------------------------------------------------------------------
-- | Renders the login page

redirIfLogin :: AppHandler () -> AppHandler ()
redirIfLogin = flip restrict redirHome

redirHome :: AppHandler ()
redirHome = redirect "/"

{- additionalUserFields :: User -> Document-}
{- additionalUserFields usr = [ "storedRules" =: storedRules usr ]-}


signupH :: AppHandler ()
signupH = do
  loggedIn <- with authLens isLoggedIn
  when loggedIn $ redirect "/"
  res <- eitherSnapForm registrationForm "registration-form"
  case res of
    Left form' ->
      blaze $ template (signupHTML form')
    Right (FormUser u p _) -> do
      _ <- with authLens $ createUser u (DT.encodeUtf8 p) -- TODO Could throw a DuplicateLogin!
      redirect "/"


------------------------------------------------------------------------------
-- | Functions for handling reading and saving per-person rules

readStoredRulesH :: AppHandler ()
readStoredRulesH = restrict forbiddenH $ do
  rules <- getRawRules
  modifyResponse $ setContentType "application/json"
  writeLBS $ encode rules

deleteStoredRuleH :: AppHandler ()
deleteStoredRuleH = restrict forbiddenH $ do
  mrid  <- getParam "id"
  case mrid of
    Nothing  -> return ()
    Just x   -> deleteRule x

-- TODO: Abstract over user thing
addStoredRuleH :: AppHandler ()
addStoredRuleH = restrict forbiddenH $ do
  rqrl <- readRequestBody 4096
  case mkRule rqrl of
    Left   err   -> error500H err
    Right  rule  -> do
      cau <- with authLens $ currentUser
      case cau of
        Nothing ->  redirect "/"
        Just x  ->  case userId x of
                      Nothing -> redirect "/"
                      Just y  -> insertRule y rule

loadExampleH :: AppHandler ()
loadExampleH = restrict forbiddenH $ do
  cau <- with authLens $ currentUser
  case cau of
    Nothing ->  redirect "/"
    Just x  ->  case userId x of
                  Nothing -> redirect "/"
                  Just y  -> do
                    deleteUserRules y
                    mapM (insertRule y) exampleData
                    redirHome

-- TODO: Abstract over the uid thing.. also refactor it; it's ugly
getRuleList :: AppHandler [Rule]
getRuleList = restrict forbiddenH $ do
  cau <- with authLens $ currentUser
  uid <- case cau of
           Nothing -> redirect "/"
           Just x  ->  case userId x of
                         Nothing -> redirect "/"
                         Just y  -> return y
  rules <- getStoredRules uid
  -- TODO: Error handling
  return $ map (fst . startParse pRule . BS.unpack . ruleStr) rules

getRawRules :: AppHandler [ByteString]
getRawRules = restrict forbiddenH $ do
  cau <- with authLens $ currentUser
  uid <- case cau of
           Nothing -> redirect "/"
           Just x  ->  case userId x of
                         Nothing -> redirect "/"
                         Just y  -> return y
  rules <- getStoredRules uid
  return $ map ruleStr rules


-- | Check the proof from the client. Since the checking could potentially
-- shoot into an inifinite recursion, a timeout is in place.
checkProofH :: AppHandler ()
checkProofH = restrict forbiddenH $ do
  setTimeout 15
  body <- readRequestBody 4096
  case mkProof body of
    Left   err    -> error500H err
    Right  proof  -> do  rules <- getRuleList
                         let prf = checkProof rules  proof
                         writeLBS $ encode prf

unifyH :: AppHandler ()
unifyH = restrict forbiddenH $ do
  setTimeout 10
  body <- readRequestBody 4096
  case mkDropReq body of
    Left   err                   -> error500H err
    Right  (DropReq prf lvl rl)  -> writeLBS $ encode (dropUnify prf lvl rl)

error500H :: ByteString -> AppHandler a
error500H msg = do
  modifyResponse $ setResponseStatus 500 "Internal server error"
  writeBS $ BS.append (BS.pack "500 internal server error: ") msg
  r <- getResponse
  finishWith r

checkSyntaxH :: AppHandler ()
checkSyntaxH = restrict forbiddenH $ do
  ptype  <- getParam "type"
  body   <- readRequestBody 4096
  let ret = parseCheck ptype body
  writeLBS $ encode ret

substH :: AppHandler ()
substH = restrict forbiddenH $ do
  body  <- readRequestBody 4096
  sub   <- getParam "sub"
  for   <- getParam "for"
  case mkProof body of
    Left   err    ->  error500H err
    Right  proof  ->  let  unjust  = BS.unpack . fromJust
                           stree   = subst (Env $ DM.fromList [(unjust for, Var $ unjust sub)]) proof
                      in   writeLBS $ encode stree



blaze :: Reader AuthState Html -> AppHandler ()
blaze htmlRdr = do
  modifyResponse $ addHeader "Content-Type" "text/html; charset=UTF-8"
  li   <- with authLens isLoggedIn
  eml  <- with authLens $ do
    cu <- currentUser
    return $ case cu of
      Nothing -> ""
      Just u  -> userLogin u
  let html = runReader htmlRdr (AuthState li eml)
  writeLBS $ renderHtml html

-------------------------------------------------------------------------------
-- Forms

data FormUser = FormUser
  {  email     :: Text
  ,  password  :: Text
  ,  remember  :: Bool }
  deriving Show

isEmail :: Monad m => Validator m Html Text
isEmail = check "Invalid email address" (E.isValid . DT.unpack)

longPwd :: Monad m => Validator m Html Text
longPwd  =  check "Password needs to be at least six characters long"
         $  \xs -> DT.length xs >= 6

isNonEmpty :: Monad m => Validator m Html Text
isNonEmpty = check "Field must not be empty" $ not . DT.null

loginForm :: Form AppHandler SnapInput Html BlazeFormHtml FormUser
loginForm = FormUser
  <$>  label  "Email address: "
       ++>  inputText Nothing `validate` isEmail
       <++  errors
  <*>  label  "Password: "
       ++>  inputPassword `validate` longPwd
       <++  errors
  <*>  label  "Remember me?"
       ++>  inputCheckBox True

-- TODO: Check if the email addresses are the same
-- TODO: Also send an email after registration
registrationForm :: Form AppHandler SnapInput Html BlazeFormHtml FormUser
registrationForm = (\e _ p _ -> FormUser e p False)
  <$>  label  "Email address: "
       ++>  inputText Nothing `validate` isEmail
       <++  errors
  <*>  label  "Email address (confirmation): "
       ++>  inputText Nothing `validate` isEmail
       <++  errors
  <*>  label  "Password: "
       ++>  inputPassword `validate` longPwd
       <++  errors
  <*>  label  "Password (confirmation): "
       ++>  inputPassword `validate` longPwd
       <++  errors


-------------------------------------------------------------------------------
-- Authentication and authorization


-------------------------------------------------------------------------------
-- Database interaction

voidM :: Monad m => m a -> m ()
voidM m = do
  _ <- m
  return ()

insertRule :: HasHdbc m c => UserId -> Rule -> m ()
insertRule uid rl = voidM $
  query'  "INSERT INTRO rules (uid, rule) VALUES (?, ?)"
          [toSql $ unUid uid, toSql $ show rl]

deleteRule :: HasHdbc m c => ByteString -> m ()
deleteRule rid = voidM $
  query' "DELETE FROM rules WHERE rid = ?" [toSql rid]

getStoredRules :: HasHdbc m c => UserId -> m [DBRule]
getStoredRules uid = do
  rs <- query  "SELECT (rid, rule_order, rule)* FROM rules WHERE uid = ?"
               [toSql uid]
  return $ map convRow rs
  where  convRow :: Map String SqlValue -> DBRule
         convRow mp =
           let  rdSql k = fromSql $ mp DM.! k
           in   DBRule  (rdSql "rid")
                        (rdSql "rule_order")
                        (rdSql "rule")

deleteUserRules :: HasHdbc m c => UserId -> m ()
deleteUserRules uid = voidM $
  query' "DELETE FROM rules WHERE uid = ?" [toSql uid]

