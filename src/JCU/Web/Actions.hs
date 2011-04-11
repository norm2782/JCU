{-# LANGUAGE OverloadedStrings  #-}

module JCU.Web.Actions where

import            Application (Application)
import            Data.Aeson (encode)
import            Data.Maybe (fromMaybe)
import Debug.Trace
import            JCU.Prolog.Prolog
import            JCU.Web.Types
import            Snap.Auth
import            Snap.Auth.Handlers
import            Snap.Extension.DB.MongoDB ((=:), Document, MonadMongoDB)
import            Snap.Extension.Heist (render)
import            Snap.Extension.Session.CookieSession (setSessionUserId)
import            Snap.Types


--
-- | Access control related actions
restrict :: (MonadMongoDB m, MonadAuth m) => m b -> m b
restrict action = do
  authed <- isLoggedIn
  case authed of
    False -> redirect "/login"
    True  -> action

forbiddenH :: Application ()
forbiddenH = do 
  modifyResponse $ setResponseStatus 403 "Forbidden"
  render "forbidden"

------------------------------------------------------------------------------
-- | Renders the front page of the sample site.
--
-- The 'ifTop' is required to limit this to the top of a route.
-- Otherwise, the way the route table is currently set up, this action
-- would be given every request.
siteIndex :: Application ()
siteIndex = restrict $ ifTop $ render "index"

checkH :: Application ()
checkH = render "check"

loginH = loginHandler "password" Nothing failedLogin redirHome

logoutH = logoutHandler redirHome

------------------------------------------------------------------------------
-- | Renders the login page
newSessionH :: Application ()
newSessionH = render "login"

failedLogin ExternalIdFailure = render "signup"
failedLogin PasswordFailure   = render "login"

newSignupH :: Application ()
newSignupH = render "signup"

redirHome :: Application ()
redirHome = redirect "/"

additionalUserFields :: User -> Document
additionalUserFields u = [ "storedRules"  =: storedRules u ]

signupH :: Application ()
signupH = do
  email  <- getParam "email"
  pwd    <- getParam "password"
  let u = makeUser email pwd
  au     <- saveAuthUser (authUser u, additionalUserFields u)
  case au of
    Nothing   -> newSignupH
    Just au'  -> do  setSessionUserId $ userId au'
                     redirect "/"

makeUser email pwd = User (emptyAuthUser  { userPassword  = fmap ClearText pwd
                                          , userEmail     = email }) ""

------------------------------------------------------------------------------
-- | Functions for handling reading and saving per-person rules

readStoredRulesH :: Application ()
readStoredRulesH = restrict $ undefined

updateStoredRulesH :: Application ()
updateStoredRulesH = restrict $ undefined

hintRulesH :: Application ()
hintRulesH = restrict $ undefined

checkRulesH :: Application ()
checkRulesH = restrict $ do
  rules <- getParam "rules"
  -- TODO: Grab the rules, parse them to something useful and then verify that the rules so far make sense and return a Bool
  writeLBS $ encode True
