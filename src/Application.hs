{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleContexts #-}


module Application where

import            Control.Applicative
import            Control.Exception (SomeException)
import            Control.Monad
import            Control.Monad.CatchIO hiding (Handler)
import            Control.Monad.Reader
import            Control.Monad.State
import            Data.Aeson as AE
import            Data.ByteString.Char8 (ByteString)
import qualified  Data.ByteString.Char8 as BS
import            Data.Lens.Template
import            Data.ListLike (CharString(..))
import            Data.Map (Map)
import qualified  Data.Map as DM
import            Data.Maybe
import            Data.Pool
import            Data.String
import            Data.Text (Text)
import qualified  Data.Text as DT
import qualified  Data.Text.Encoding as DT
import qualified  Database.HDBC as HDBC
import            Database.HDBC.PostgreSQL
import            JCU.Prolog
import            JCU.Templates
import            JCU.Types
import            Language.Prolog.NanoProlog.NanoProlog
import            Language.Prolog.NanoProlog.Parser
import            Prelude hiding (catch)
import            Snap.Core
import            Snap.Snaplet
import            Snap.Snaplet.Auth
import            Snap.Snaplet.Auth.Backends.Hdbc
{-import            Snap.Snaplet.Hdbc-}
import            Snap.Snaplet.Session
import            Snap.Snaplet.Session.Backends.CookieSession
import            Snap.Util.FileServe
import            Text.Blaze
import qualified  Text.Blaze.Html5 as H
import            Text.Blaze.Renderer.Utf8 (renderHtml)
import            Text.Digestive
import            Text.Digestive.Blaze.Html5
import            Text.Digestive.Forms.Snap
import qualified  Text.Email.Validate as E
import qualified  Database.HDBC as HDBC

data App = App
  {  _authLens  :: Snaplet (AuthManager App)
  ,  _sessLens  :: Snaplet SessionManager
  ,  pgconn    :: Connection
  }

makeLens ''App

type AppHandler = Handler App App

{-instance HasHdbc (Handler b App) Connection IO where-}
   {-getHdbcState = with dbLens get-}

jcu :: SnapletInit App App
jcu = makeSnaplet "jcu" "Prolog proof tree practice application" Nothing $ do
  addRoutes  [  ("/", ifTop siteIndexH)
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
  let pgsql  = connectPostgreSQL' =<< readFile "config/connection_string.conf"
  pg <- liftIO $ pgsql
  -- pool <- liftIO $ createPool pgsql HDBC.disconnect 1 500 1
  {-_dblens'    <- nestSnaplet "hdbc" dbLens $ hdbcInit pgsql-}
  _authlens'  <- nestSnaplet "auth" authLens $ initHdbcAuthManager
                   defAuthSettings sessLens pgsql defAuthTable defQueries
  return  $ App _authlens' _sesslens' pg


------------------------------------------------------------------------------
-- | Handlers

restrict :: AppHandler b -> AppHandler b -> AppHandler b
restrict failH succH = do
  with sessLens touchSession
  authed <- with authLens isLoggedIn
  if authed
    then succH
    else failH

loginRedir :: AppHandler ()
loginRedir = redirect "/login"

forbiddenH :: AppHandler a
forbiddenH = do
  modifyResponse $ setResponseStatus 403 "Forbidden"
  writeBS "403 forbidden"
  finishWith =<< getResponse

siteIndexH :: AppHandler ()
siteIndexH = ifTop $ restrict loginRedir (blaze $ template index)

loginH :: AppHandler ()
loginH = withSession sessLens $ do
  loggedIn <- with authLens isLoggedIn
  when loggedIn $ redirect "/"
  res <- eitherSnapForm loginForm "login-form"
  case res of
    Left form' -> do
      didFail <- with sessLens $ do
        failed <- getFromSession "login-failed"
        deleteFromSession "login-failed"
        commitSession
        return failed
      blaze $ template $ loginHTML (isJust didFail) form'
    Right (FormUser e p r) -> do
      loginRes <- with authLens $
                    loginByUsername  (DT.encodeUtf8 e)
                                     (ClearText $ DT.encodeUtf8 p) r
      case loginRes of
        Left _   ->  do  with sessLens $ do
                           setInSession "login-failed" "1"
                           commitSession
                         redirect "/login"
        Right _  ->  redirect "/"

-- TODO: Also send an email after registration
signupH :: AppHandler ()
signupH = do
  loggedIn <- with authLens isLoggedIn
  when loggedIn $ redirect "/"
  res <- eitherSnapForm registrationForm "registration-form"
  case res of
    Left form' -> do
      exists <- with sessLens $ do
        failed <- getFromSession "username-exists"
        deleteFromSession "username-exists"
        commitSession
        return failed
      blaze $ template (signupHTML (isJust exists) form')
    Right (FormUser e p _) -> do
      _ <- with authLens (createUser e (DT.encodeUtf8 p)) `catch` hndlExcptn
      redirect "/"
  where  hndlExcptn :: SomeException -> AppHandler AuthUser
         hndlExcptn _ = do
           with sessLens $ do
             setInSession "username-exists" "1"
             commitSession
           redirect "/signup"

logoutH :: AppHandler ()
logoutH = do
  with authLens logout
  redirect "/"

readStoredRulesH :: AppHandler ()
readStoredRulesH = restrict forbiddenH $ do
  rules <- getStoredRules =<< getUserId
  modifyResponse $ setContentType "application/json"
  writeLBS $ encode rules

deleteStoredRuleH :: AppHandler ()
deleteStoredRuleH = restrict forbiddenH $ do
  mrid <- getParam "id"
  case mrid of
    Nothing  -> return ()
    Just x   -> deleteRule x -- TODO: Take user ID into account. we don't want people deleting other users's rules

addStoredRuleH :: AppHandler ()
addStoredRuleH = restrict forbiddenH $ do
  rqrl <- readRequestBody 4096
  case mkRule rqrl of
    Left   err  -> error500H err
    Right  rl   -> do
      uid  <- getUserId
      voidM $ insertRule uid rl

loadExampleH :: AppHandler ()
loadExampleH = restrict forbiddenH $ do
  uid <- getUserId
  deleteUserRules uid
  mapM_ (insertRule uid) exampleData
  -- commitSession
  -- redirect "/"


getUserId :: AppHandler UserId
getUserId = do
  cau <- with authLens currentUser
  case cau >>= userId of
    Nothing  -> redirect "/"
    Just x   -> return x

-- | Check the proof from the client. Since the checking could potentially
-- shoot into an inifinite recursion, a timeout is in place.
checkProofH :: AppHandler ()
checkProofH = restrict forbiddenH $ do
  setTimeout 15
  body <- readRequestBody 4096
  case mkProof body of
    Left   err    -> error500H err
    Right  proof  -> do
      rules <- getStoredRules =<< getUserId
      writeLBS $ encode (checkProof (map rule rules) proof)

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
  writeBS $ BS.append (fromString "500 internal server error: ") msg
  finishWith =<< getResponse

checkSyntaxH :: AppHandler ()
checkSyntaxH = restrict forbiddenH $ do
  ptype  <- getParam "type"
  body   <- readRequestBody 4096
  writeLBS $ encode (parseCheck ptype body)

substH :: AppHandler ()
substH = restrict forbiddenH $ do
  body  <- readRequestBody 4096
  sub   <- getParam "sub"
  for   <- getParam "for"
  case mkProof body of
    Left   err    -> error500H err
    Right  proof  ->
      case (sub, for) of
        (Just sub', Just for')  ->
          let  env = Env $ DM.fromList [(BS.unpack for', Var $ BS.unpack sub')]
          in   writeLBS $ encode (subst env proof)
        _                       -> writeLBS $ encode proof


-------------------------------------------------------------------------------
-- View rendering

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

identical :: Validator AppHandler Html (Text, Text)
identical = check "Field values must be identical" (uncurry (==))

loginForm :: Form AppHandler SnapInput Html BlazeFormHtml FormUser
loginForm = (\e p r _ -> FormUser e p r)
  <$>  mapViewHtml H.div (
       label  "Email address: "
       ++>    inputText Nothing `validate` isEmail
       <++    errors)
  <*>  mapViewHtml H.div (
       label  "Password: "
       ++>    inputPassword False `validate` longPwd
       <++    errors)
  <*>  mapViewHtml H.div (
       label  "Remember me?"
       ++>    inputCheckBox True)
  <*>  mapViewHtml H.div (
       submit "Login")

registrationForm :: Form AppHandler SnapInput Html BlazeFormHtml FormUser
registrationForm = (\ep pp _ -> FormUser (fst ep) (fst pp) False)
  <$>  ((,)
         <$>  mapViewHtml H.div (
              label  "Email address: "
              ++>    inputText Nothing `validate` isEmail
              <++    errors)
         <*>  mapViewHtml H.div (
              label  "Email address (confirmation): "
              ++>    inputText Nothing `validate` isEmail
              <++    errors))
       `validate`  identical
       <++         errors
  <*>  ((,)
         <$>  mapViewHtml H.div (
              label  "Password: "
              ++>    inputPassword False `validate` longPwd
              <++    errors)
         <*>  mapViewHtml H.div (
              label  "Password (confirmation): "
              ++>    inputPassword False `validate` longPwd
              <++    errors))
       `validate`  identical
       <++         errors
  <*>  mapViewHtml H.div (
       submit "Register")



-------------------------------------------------------------------------------
-- Database interaction

voidM :: Monad m => m a -> m ()
voidM m = do
  _ <- m
  return ()

-- TODO: This is just a workaround....
q :: String -> [HDBC.SqlValue] -> AppHandler ()
q qry vals = do
  c <- gets pgconn
  c' <- liftIO $ HDBC.clone c
  liftIO $ HDBC.withTransaction c' $ \conn' -> do
    stmt  <- HDBC.prepare conn' qry
    voidM $ HDBC.execute stmt vals
  return ()

insertRule :: UserId -> Rule -> AppHandler (Maybe Int)
insertRule uid rl = let sqlVals = [HDBC.toSql $ unUid uid, HDBC.toSql $ show rl] in do
  q  "INSERT INTO rules (uid, rule_order, rule) VALUES (?, 1, ?)" sqlVals
  c <- gets pgconn
  c' <- liftIO $ HDBC.clone c
  rws <- liftIO $ do
    stmt <- HDBC.prepare c' "SELECT rid FROM rules WHERE uid = ? AND rule = ? ORDER BY rid DESC"
    voidM $ HDBC.execute stmt sqlVals
    HDBC.fetchAllRowsMap' stmt
  return $ case rws of
             []     -> Nothing
             (x:_)  -> Just $ HDBC.fromSql $ x DM.! "rid"

deleteRule :: ByteString -> AppHandler ()
deleteRule rid = q "DELETE FROM rules WHERE rid = ?" [HDBC.toSql rid]

getStoredRules :: UserId -> AppHandler [DBRule]
getStoredRules uid = do
  c <- gets pgconn
  c' <- liftIO $ HDBC.clone c
  rws <- liftIO $ do
    stmt <- HDBC.prepare c' "SELECT rid, rule_order, rule FROM rules WHERE uid = ?"
    voidM $ HDBC.execute stmt [HDBC.toSql uid]
    HDBC.fetchAllRowsMap' stmt
  {-rs <- query  "SELECT rid, rule_order, rule FROM rules WHERE uid = ?"-}
               {-[toSql uid]-}
  return $ map convRow rws
  where  convRow :: Map String HDBC.SqlValue -> DBRule
         convRow mp =
           let  rdSql k = HDBC.fromSql $ mp DM.! k
           in   DBRule  (rdSql "rid")
                        (rdSql "rule_order")
                        (fst . startParse pRule $ CS (rdSql "rule"))

deleteUserRules :: UserId -> AppHandler ()
deleteUserRules uid = q "DELETE FROM rules WHERE uid = ?" [HDBC.toSql uid]

