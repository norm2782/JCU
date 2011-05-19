{-# LANGUAGE OverloadedStrings  #-}

module JCU.Handlers where

import            Application (Application)
import            Data.Aeson (encode, fromJSON, json)
import            Data.Aeson.Types as AE (Result(..), Value(..))
import            Data.Attoparsec.Lazy as L (Result(..), parse)
import            Data.ByteString as B (ByteString, length)
import            Data.ByteString.Char8 as B (unpack, pack)
import qualified  Data.ByteString.Lazy.Char8 as L (ByteString)
import            Data.Map (Map, member, (!))
import            Data.Maybe (fromJust)
{- import            Database.MongoDB as DB hiding (unpack, Value(..))-}
import            Debug.Trace (trace) -- TODO: Remove
import            JCU.Parser()
import            JCU.Prolog
import            JCU.Types
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

forbiddenH :: Application ()
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

checkH :: Application ()
checkH = render "check"

loginH :: Application ()
loginH = loginHandler "password" (Just "remember") failedLogin redirHome

logoutH :: Application ()
logoutH = logoutHandler redirHome

------------------------------------------------------------------------------
-- | Renders the login page
newSessionH :: Application ()
newSessionH = render "login"

failedLogin :: MonadHeist n m => AuthFailure -> m ()
failedLogin ExternalIdFailure = render "signup"
failedLogin PasswordFailure   = render "login"

newSignupH :: Application ()
newSignupH = render "signup"

redirHome :: Application ()
redirHome = redirect "/"

additionalUserFields :: User -> Document
additionalUserFields usr = [ "storedRules"  =: storedRules usr ]

type FormValidator = [(ByteString, FormField)]
data FormField = FormField  {  isRequired    :: Bool
                            ,  fldValidator  :: ByteString -> Bool }

-- TODO: Add support for multiple parameters with the same name
-- TODO: Add support for returning validation errors.
-- TODO: See what the Digestive Functors can do for form validation... it is
-- much better suited for validation than this...
formValidator :: FormValidator
formValidator =  [  ("email",     FormField True (E.isValid . unpack))
                 ,  ("password",  FormField True (\xs -> B.length xs >= 6)) ]

valForm :: Ord k => Map k [ByteString] -> (k, FormField) -> Bool
valForm parms (fld, FormField req val)  | fld `member` parms  = val $ head (parms ! fld)
                                        | otherwise           = not req

-- TODO: Look at digestive-functors for form validation
signupH :: Application ()
signupH = do
  parms  <- getParams
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
makeUser email pwd = User (emptyAuthUser  { userPassword  = fmap ClearText pwd
                                          , userEmail     = email }) []

------------------------------------------------------------------------------
-- | Functions for handling reading and saving per-person rules

readStoredRulesH :: Application ()
readStoredRulesH = restrict forbiddenH $ do
  rules <- getRules
  modifyResponse $ setContentType "application/json"
  writeLBS $ encode rules

updateStoredRulesH :: Application ()
updateStoredRulesH = restrict forbiddenH $ do undefined

deleteStoredRuleH :: Application ()
deleteStoredRuleH = restrict forbiddenH $ do
  rule <- getParam "id"
  trace ("deleteStoredRuleH: " ++ show rule) (return ())

addStoredRuleH :: Application ()
addStoredRuleH = restrict forbiddenH $ do
  rule <- getRequestBody
  trace ("addStoredRuleH: " ++ show rule)
        (writeLBS "")

-- TODO: Eventually remove populateH
populateH :: Application ()
populateH = restrict forbiddenH $ do
  putRules testStoredRules
  writeLBS "populateH"

putRules :: [Rule] -> Application ()
putRules rls = do
  cau  <- currentAuthUser
  doc  <- rulesToDoc rls (snd . fromJust $ cau)
  tbl  <- fmap u authUserTable
  withDB' $ save tbl doc

rulesToDoc :: (MonadMongoDB m) => [Rule] -> Document -> m Document
rulesToDoc rls d = do
  let tsc = ["storedRules" =: map (pack . show) rls]
  return $ tsc `MDB.merge` d

getRules :: Application [ByteString]
getRules = do
  cau <- currentAuthUser
  let rules = docToLst . snd . fromJust $ cau
  return $ case rules of
             Nothing  -> []
             Just xs  -> xs

docToLst :: Document -> Maybe [ByteString]
docToLst d = do
  sr <- MDB.lookup "storedRules" d
  return sr

-- | Check the proof from the client. Since the checking could potentially
-- shoot into an inifinite recursion, a timeout is in place.
checkProofH :: Application ()
checkProofH = restrict forbiddenH $ do
  setTimeout 15
  proof <- mkRules =<< getRequestBody
  writeLBS $ encode (checkProof testStoredRules proof) -- TODO: Grab rules from User

unifyH :: Application ()
unifyH = restrict forbiddenH $ do
  (DropReq tm rl) <- mkDropReq =<< getRequestBody
  writeLBS $ encode (getRhss tm rl)

mkDropReq :: L.ByteString -> Application DropReq
mkDropReq = parseJSON fromJSON

parseJSON :: (Value -> AE.Result a) -> L.ByteString -> Application a
parseJSON f raw =
  case L.parse json raw of
    (Done _ r)  ->
      case f r of
        (Success a)  -> return a
        _            -> error500H
    _           -> error500H

mkRules :: L.ByteString -> Application Proof
mkRules = parseJSON fromJSON

error500H :: Application a
error500H = do
  modifyResponse $ setResponseStatus 500 "Internal server error"
  writeBS "500 internal server error"
  r <- getResponse
  finishWith r
