{-# LANGUAGE OverloadedStrings  #-}

module JCU.Web.Handlers where

import            Application (Application)
import            Data.Aeson (encode, fromJSON, json)
import            Data.Aeson.Types as AE (Result(..))
import            Data.Attoparsec.Lazy as L (Result(..), parse)
import            Data.ByteString as B (ByteString, length)
import            Data.ByteString.Char8 as B (unpack)
import qualified  Data.ByteString.Lazy.Char8 as L (ByteString)
import            Data.List as DL (length)
import            Data.Map (Map, member, (!))
import            Debug.Trace (trace) -- TODO: Remove
import            JCU.Prolog.Types
import            JCU.Prolog.Prolog (solve, unify)
import            JCU.Web.Types
import            Snap.Auth
import            Snap.Auth.Handlers
import            Snap.Extension.DB.MongoDB ((=:), Document, MonadMongoDB)
import            Snap.Extension.Heist (render, MonadHeist)
import            Snap.Extension.Session.CookieSession (setSessionUserId)
import            Snap.Types
import            Text.Email.Validate as E (isValid)

-- TODO: Add a consistent naming scheme and rename all functions here
--
--
-- | Access control related actions
restrict :: (MonadMongoDB m, MonadAuth m) => m b -> m b -> m b
restrict failH succH = do
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
additionalUserFields u = [ "storedRules"  =: storedRules u ]

type FormValidator = [(ByteString, FormField)]
data FormField = FormField  {  isRequired    :: Bool
                            ,  fldValidator  :: ByteString -> Bool }

-- TODO: Add support for multiple parameters with the same name
-- TODO: Add support for returning validation errors.
-- TODO: See what the Digestive Functors can do for form validation... it is
-- much better suited for validation than this...
formValidator :: FormValidator
formValidator =  [  ("email",     FormField True (E.isValid . unpack))
                 ,  ("password",  FormField True (\xs -> B.length xs > 6)) ]

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
              let u = makeUser email pwd
              au     <- saveAuthUser (authUser u, additionalUserFields u)
              case au of
                Nothing   -> newSignupH
                Just au'  -> do  setSessionUserId $ userId au'
                                 redirect "/"
    else  redirect "/signup" -- TODO: Better handling of invalid forms

makeUser :: Maybe ByteString -> Maybe ByteString -> User
makeUser email pwd = User (emptyAuthUser  { userPassword  = fmap ClearText pwd
                                          , userEmail     = email }) [] []

------------------------------------------------------------------------------
-- | Functions for handling reading and saving per-person rules

readStoredRulesH :: Application ()
readStoredRulesH = do-- TODO restrict forbiddenH $ do
  modifyResponse $ setContentType "application/json"
  trace ("readStoredRulesH: " ++ show testStoredRules)
        (writeLBS $ encode testStoredRules)

updateStoredRulesH :: Application ()
updateStoredRulesH = undefined -- TODO restrict forbiddenH $ do

deleteStoredRuleH :: Application ()
deleteStoredRuleH = do-- TODO restrict forbiddenH $ do
  rule <- getParam "id"
  trace ("deleteStoredRuleH: " ++ show rule) (return ())

deleteInUseRuleH :: Application ()
deleteInUseRuleH = do-- TODO restrict forbiddenH $ do
  rule <- getParam "id"
  trace ("deleteInUseRuleH: " ++ show rule) (return ())

hintRulesH :: Application ()
hintRulesH = do -- TODO restrict forbiddenH $ do
  rules <- mkRules =<< getRequestBody
  trace ("hintRulesH: " ++ show rules)
        (writeLBS $ encode True)

addStoredRuleH :: Application ()
addStoredRuleH = do-- TODO restrict forbiddenH $ do
  rule <- getRequestBody
  trace ("addStoredRuleH: " ++ show rule)
        (writeLBS "{}")

checkRulesH :: Application ()
checkRulesH = do-- TODO restrict forbiddenH $ do
  rules <- mkRules =<< getRequestBody
  let (t :<-: _:_)  = rules
  let solutions     = solve testStoredRules [t] [] 0
  let checked       = checkRules rules solutions
  trace ("checkRulesH: " ++ show rules)
        (writeLBS $ encode checked)

checkRules :: [Rule] -> [EnvTrace] -> [Bool]
checkRules rules = foldr (comp . checkRules') []
  where checkRules' (env, trcs) = [ any (cmpRuleTrace env r) trcs
                                  | r <- rules ]

comp :: [Bool] -> [Bool] -> [Bool]
comp lst highest | l lst > l highest = lst
                 | otherwise         = highest
  where l = DL.length . takeWhile (== True)

-- TODO: Is this right? Also, do we need r == u?
-- TODO: Take the conclusion into account
-- TODO: Send data from client from text fields, not from collection...
cmpRuleTrace :: Env -> Rule -> Trace -> Bool
cmpRuleTrace env r@(t :<-: _) (Trace g u _ _) =
  trace ("env: " ++ show env ++ "\n" ++
         "rterm: " ++ show r ++ "\n" ++
         "traceg: " ++ show g ++ "\n" ++
         "traceu: " ++ show u ++ "\n")
        unify (t, g) (Just env) /= Nothing || r == u

mkRules :: L.ByteString -> Application [Rule]
mkRules raw =
  case L.parse json raw of
    (Done _ r)  ->
      case fromJSON r :: AE.Result [Rule] of
        (Success a)  -> return a
        _            -> do500
    _           -> do500
  where do500 = do
          modifyResponse $ setResponseStatus 500 "Internal server error"
          writeBS "500 internal server error"
          r <- getResponse
          finishWith r

readInUseRulesH :: Application ()
readInUseRulesH =  do-- TODO restrict forbiddenH $ do
  modifyResponse $ setContentType "application/json"
  writeLBS $ encode testInUseRules

updateInUseRulesH :: Application ()
updateInUseRulesH = do-- TODO restrict forbiddenH $ do
  models <- getRequestBody
  let dmods = models -- fromJSON models
  trace ("updateInUseRulesH: " ++ show dmods) (return ())
  return ()

testStoredRules :: [Rule]
testStoredRules =  [ Fun "ma"    [Var "mien", Var "juul"] :<-: []
                   , Fun "ma"    [Var "juul", Var "bea"]  :<-: []
                   , Fun "ma"    [Var "bea",  Var "alex"] :<-: []
                   , Fun "ma"    [Var "bea",  Var "cons"] :<-: []
                   , Fun "ma"    [Var "max",  Var "ale"]  :<-: []
                   , Fun "ma"    [Var "max",  Var "ama"]  :<-: []
                   , Fun "ma"    [Var "max",  Var "ari"]  :<-: []
                   , Fun "oma"   [Var "X",    Var "Z"]    :<-: [ Fun "ma"    [Var "X", Var "Y"]
                                                               , Fun "ouder" [Var "Y", Var "Z"] ]
                   , Fun "pa"    [Var "alex", Var "ale"]  :<-: []
                   , Fun "pa"    [Var "alex", Var "ama"]  :<-: []
                   , Fun "pa"    [Var "alex", Var "ari"]  :<-: []
                   , Fun "ouder" [Var "X",    Var "Y"]    :<-: [ Fun "pa"    [Var "X", Var "Y"] ]
                   , Fun "ouder" [Var "X",    Var "Y"]    :<-: [ Fun "ma"    [Var "X", Var "Y"] ]
                   , Fun "voor"  [Var "X",    Var "Y"]    :<-: [ Fun "ouder" [Var "X", Var "Y"] ]
                   , Fun "voor"  [Var "X",    Var "Y"]    :<-: [ Fun "ouder" [Var "X", Var "Z"]
                                                               , Fun "voor"  [Var "Z", Var "Y"] ] ]

testInUseRules :: [Rule]
testInUseRules = [ Fun "ouder" [Var "X",    Var "ama"] :<-: []
                 , Fun "ouder" [Var "X",    Var "Y"]   :<-: [ Fun "pa" [Var "X", Var "Y"] ]
                 , Fun "pa"    [Var "X",    Var "ama"] :<-: []
                 , Fun "pa"    [Var "alex", Var "ama"] :<-: []
                 {- , Fun "alex"  []                      :<-: []-}
                 ]


{-
alex
------------ pa(alex, ama).
pa(X, ama).
------------ ouder(X, Y) :- pa(X, Y).
ouder(X, ama).


max
------------ ma(max, ama).
ma(X, ama).
------------ ouder(X, Y) :- ma(X, Y).
ouder(X, ama).
-}
