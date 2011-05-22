{-# LANGUAGE OverloadedStrings  #-}

module JCU.Handlers where

import            Application (Application)
import            Data.Aeson (encode, fromJSON, json)
import            Data.Aeson.Types as AE (Result(..), Value(..))
import            Data.Attoparsec.Lazy as L (Result(..), parse)
import            Data.ByteString as B (ByteString, length)
import            Data.ByteString.Char8 as B (unpack, pack)
import qualified  Data.ByteString.Lazy.Char8 as L (ByteString)
import            Data.List as DL (delete)
import            Data.Map (Map, member, (!))
import            Data.Maybe (fromJust, fromMaybe)
import            JCU.Prolog
import            JCU.Types
import            Language.Prolog.NanoProlog
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

logoutH :: Application ()
logoutH = logoutHandler redirHome

------------------------------------------------------------------------------
-- | Renders the login page
newSessionH :: Application ()
newSessionH = redirIfLogin (render "login")

redirIfLogin :: Application () -> Application ()
redirIfLogin = flip restrict redirHome

failedLogin :: MonadHeist n m => AuthFailure -> m ()
failedLogin ExternalIdFailure  = render "signup"
failedLogin PasswordFailure    = render "login"

newSignupH :: Application ()
newSignupH = redirIfLogin (render "signup")

redirHome :: Application ()
redirHome = redirect "/"

additionalUserFields :: User -> Document
additionalUserFields usr = [ "storedRules"  =: storedRules usr ]

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
  rule  <- mkRule rqrl
  putRawRules (rls ++ [pack . show $ rule])

mkRule :: L.ByteString -> Application Rule
mkRule = parseJSON fromJSON

loadExampleH :: Application ()
loadExampleH = restrict forbiddenH $ do
  putRules exampleData
  redirHome

cnst :: LowerCase -> Term
cnst s = Fun s []

exampleData :: [Rule]
exampleData =
  -- Dutch Royal family
  [  Fun "ma"     [cnst "mien",  cnst "juul"]  :<-: []
  ,  Fun "ma"     [cnst "juul",  cnst "bea"]   :<-: []
  ,  Fun "ma"     [cnst "bea",   cnst "alex"]  :<-: []
  ,  Fun "ma"     [cnst "bea",   cnst "con"]   :<-: []
  ,  Fun "ma"     [cnst "bea",   cnst "fri"]   :<-: []
  ,  Fun "ma"     [cnst "max",   cnst "ale"]   :<-: []
  ,  Fun "ma"     [cnst "max",   cnst "ama"]   :<-: []
  ,  Fun "ma"     [cnst "max",   cnst "ari"]   :<-: []
  ,  Fun "pa"     [cnst "alex",  cnst "ale"]   :<-: []
  ,  Fun "pa"     [cnst "alex",  cnst "ama"]   :<-: []
  ,  Fun "pa"     [cnst "alex",  cnst "ari"]   :<-: []
  ,  Fun "ouder"  [Var "X",  Var "Y"] :<-:  [  Fun "pa"     [Var "X",  Var "Y"] ]
  ,  Fun "ouder"  [Var "X",  Var "Y"] :<-:  [  Fun "ma"     [Var "X",  Var "Y"] ]
  ,  Fun "voor"   [Var "X",  Var "Y"] :<-:  [  Fun "ouder"  [Var "X",  Var "Y"] ]
  ,  Fun "voor"   [Var "X",  Var "Y"] :<-:  [  Fun "ouder"  [Var "X",  Var "Z"]
                                            ,  Fun "voor"   [Var "Z",  Var "Y"] ]
  ,  Fun "oma"    [Var "X",  Var "Z"] :<-:  [  Fun "ma"     [Var "X",  Var "Y"]
                                            ,  Fun "ouder"  [Var "Y",  Var "Z"] ]
  -- List
  ,  Fun "append" [cnst "nil", Var "X", Var "Y"] :<-: []
  ,  Fun "append" [  Fun "cons" [Var "A", Var "X"]
                  ,  Var "Y", Fun "cons" [Var "A", Var "Z"]] :<-: [Fun "append" [Var "X", Var "Y", Var "Z"]]

  -- List lookup
  ,  Fun "elem" [Var "X", Fun "cons" [Var "X", Var "Y"]] :<-: []
  ,  Fun "elem" [Var "X", Fun "cons" [Var "Z", Var "Y"]] :<-: [Fun "elem" [Var "X", Var "Y"]]

  -- Natural numbers
  ,  Fun "plus" [cnst "zero", Var "X", Var "X"] :<-: []
  ,  Fun "plus" [Fun "succ" [Var "X"], Var "Y", Fun "succ" [Var "Z"]] :<-: [Fun "plus" [Var "X", Var "Y", Var "Z"]]
  ]

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
  proof  <- mkProof =<< getRequestBody
  rules  <- getRules
  let prf = checkProof rules proof
  writeLBS $ encode prf

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

mkProof :: L.ByteString -> Application Proof
mkProof = parseJSON fromJSON

error500H :: Application a
error500H = do
  modifyResponse $ setResponseStatus 500 "Internal server error"
  writeBS "500 internal server error"
  r <- getResponse
  finishWith r
