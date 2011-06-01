{-# LANGUAGE OverloadedStrings  #-}

module JCU.Handlers where

import            Application (Application)
import            Data.Aeson (encode)
import            Data.ByteString as B (ByteString, length)
import            Data.ByteString.Char8 as B (unpack, pack)
import            Data.List as DL (delete)
import            Data.Map (Map, member, (!), fromList)
import            Data.Maybe (fromJust, fromMaybe)
import            JCU.Prolog
import            JCU.Types
import            Language.Prolog.NanoProlog.NanoProlog
import            Snap.Auth
import            Snap.Auth.Handlers
import            Snap.Extension.DB.MongoDB as MDB (u, save, merge, (=:), lookup, Document, MonadMongoDB, withDB')
import            Snap.Extension.Heist (render, MonadHeist)
import            Snap.Extension.Session.CookieSession (setSessionUserId, touchSession)
import            Snap.Types
import            Text.Email.Validate as E (isValid)

-- TODO: Add a consistent naming scheme and rename all functions here
--
--
-- | Access control related actions
restrict :: (MonadMongoDB m, MonadAuth m) => m b -> m b -> m b
restrict failH succH = do
  touchSession
  authed <- isLoggedIn
  if authed
    then succH
    else failH

loginRedir :: Application ()
loginRedir = redirect "/login"

forbiddenH :: Application a
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
siteIndex :: Application ()
siteIndex = ifTop $ restrict loginRedir $ render "index"

loginH :: Application ()
loginH = loginHandler "password" (Just "remember") failedLogin redirHome
  where  failedLogin :: MonadHeist n m => AuthFailure -> m ()
         failedLogin _ = render "login"

logoutH :: Application ()
logoutH = logoutHandler redirHome

------------------------------------------------------------------------------
-- | Renders the login page
newSessionH :: Application ()
newSessionH = redirIfLogin (render "login")

redirIfLogin :: Application () -> Application ()
redirIfLogin = flip restrict redirHome

newSignupH :: Application ()
newSignupH = redirIfLogin (render "signup")

redirHome :: Application ()
redirHome = redirect "/"

additionalUserFields :: User -> Document
additionalUserFields usr = [ "storedRules" =: storedRules usr ]

type FormValidator = [(ByteString, ByteString -> Bool)]

-- TODO: See what the Digestive Functors can do for form validation... it is
-- much better suited for validation than this...
formValidator :: FormValidator
formValidator =  [  ("email",     E.isValid . unpack)
                 ,  ("password",  (>= 6) . B.length) ]

valForm :: Ord k => Map k [ByteString] -> (k, ByteString -> Bool) -> Bool
valForm params (fld, val)  | fld `member` params  = val $ head (params ! fld)
                           | otherwise            = False

-- TODO: Look at digestive-functors for form validation
signupH :: Application ()
signupH = do
  parms <- getParams
  let validated = and [ valForm parms p | p <- formValidator]
  if validated
    then  do  email  <- getParam "email"
              pwd    <- getParam "password"
              let usr = makeUser email pwd
              au     <- saveAuthUser (authUser usr, additionalUserFields usr)
              case au of
                Nothing   -> newSignupH
                Just au'  -> do  setSessionUserId $ userId au'
                                 redirect "/"
    else  redirect "/signup" -- TODO: Better handling of invalid forms

makeUser :: Maybe ByteString -> Maybe ByteString -> User
makeUser email pwd = User (emptyAuthUser  {  userPassword  = fmap ClearText pwd
                                          ,  userEmail     = email }) []

------------------------------------------------------------------------------
-- | Functions for handling reading and saving per-person rules

readStoredRulesH :: Application ()
readStoredRulesH = restrict forbiddenH $ do
  rules <- getRawRules
  modifyResponse $ setContentType "application/json"
  writeLBS $ encode rules

deleteStoredRuleH :: Application ()
deleteStoredRuleH = restrict forbiddenH $ do
  rule  <- getParam "id"
  rls   <- getRawRules
  case rule of
    Nothing  -> return ()
    Just x   -> putRawRules (DL.delete x rls)

addStoredRuleH :: Application ()
addStoredRuleH = restrict forbiddenH $ do
  rqrl  <- getRequestBody
  rls   <- getRawRules
  case mkRule rqrl of
    Left   err   -> error500H err
    Right  rule  -> putRawRules (rls ++ [pack . show $ rule])

loadExampleH :: Application ()
loadExampleH = restrict forbiddenH $ do
  putRules exampleData
  redirHome

putRules :: [Rule] -> Application ()
putRules = putRawRules . map (pack . show)

putRawRules :: [ByteString] -> Application ()
putRawRules rls = restrict forbiddenH $ do
  cau  <- currentAuthUser
  doc  <- rulesToDoc rls (snd . fromJust $ cau)
  tbl  <- fmap u authUserTable
  withDB' $ save tbl doc

rulesToDoc :: (MonadMongoDB m) => [ByteString] -> Document -> m Document
rulesToDoc rls d = do
  let tsc = ["storedRules" =: rls]
  return $ tsc `MDB.merge` d

getRules :: Application [Rule]
getRules = restrict forbiddenH $ do
  cau <- currentAuthUser
  let rawRules = getStoredRules . snd . fromJust $ cau
  -- TODO: Error handling
  return $ map (fst . startParse pRule . unpack) (fromMaybe [] rawRules)

getRawRules :: Application [ByteString]
getRawRules = restrict forbiddenH $ do
  cau <- currentAuthUser
  let rules = getStoredRules . snd . fromJust $ cau
  return $ fromMaybe [] rules

getStoredRules :: Document -> Maybe [ByteString]
getStoredRules = MDB.lookup "storedRules"

-- | Check the proof from the client. Since the checking could potentially
-- shoot into an inifinite recursion, a timeout is in place.
checkProofH :: Application ()
checkProofH = restrict forbiddenH $ do
  setTimeout 15
  body <- getRequestBody
  case mkProof body of
    Left   err    -> error500H err
    Right  proof  -> do  rules <- getRules
                         let prf = checkProof rules  proof
                         writeLBS $ encode prf

unifyH :: Application ()
unifyH = restrict forbiddenH $ do
  setTimeout 10
  body <- getRequestBody
  case mkDropReq body of
    Left   err                   -> error500H err
    Right  (DropReq prf lvl rl)  -> writeLBS $ encode (dropUnify prf lvl rl)

error500H :: String -> Application a
error500H msg = do
  modifyResponse $ setResponseStatus 500 "Internal server error"
  writeBS . B.pack $ "500 internal server error: " ++ msg
  r <- getResponse
  finishWith r

checkSyntaxH :: Application ()
checkSyntaxH = restrict forbiddenH $ do
  ptype  <- getParam "type"
  body   <- getRequestBody
  let ret = parseCheck ptype body
  writeLBS $ encode ret

substH :: Application ()
substH = restrict forbiddenH $ do
  body  <- getRequestBody
  sub   <- getParam "sub"
  for   <- getParam "for"
  case mkProof body of
    Left   err    ->  error500H err
    Right  proof  ->  let  unjust  = B.unpack . fromJust
                           stree   = subst (fromList [(unjust for, Var $ unjust sub)]) proof
                      in   writeLBS $ encode stree
