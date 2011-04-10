{-# LANGUAGE OverloadedStrings  #-}

module JCU.Web.Actions where

import            Snap.Extension.Heist (render)
import            Snap.Types
import            Snap.Auth
import            Snap.Auth.Handlers
import            Application (Application)
import            Snap.Extension.DB.MongoDB ((=:), Document)
import            Snap.Extension.Session.CookieSession (setSessionUserId)
import            JCU.Prolog.Prolog
import            Data.Aeson (encode)
import            JCU.Web.Types

------------------------------------------------------------------------------
-- | Renders the front page of the sample site.
--
-- The 'ifTop' is required to limit this to the top of a route.
-- Otherwise, the way the route table is currently set up, this action
-- would be given every request.
siteIndex :: Application ()
siteIndex = ifTop $ render "index"


checkH :: Application ()
checkH = render "check"

loginH = loginHandler "password" Nothing newSessionH redirHome

logoutH = logoutHandler redirHome

------------------------------------------------------------------------------
-- | Renders the login page
newSessionH :: Application ()
newSessionH = render "login"

newSignupH :: Application ()
newSignupH = render "signup"

redirHome :: Application ()
redirHome = redirect "/"

additionalUserFields :: User -> Document
additionalUserFields u = [ "storedRules"  =: storedRules u ]

signupH :: Application ()
signupH = do
  ps  <- getParams
  let u = makeUser ps
  au  <- saveAuthUser (authUser u, additionalUserFields u)
  case au of
    Nothing   -> newSignupH
    Just au'  -> do  setSessionUserId $ userId au'
                     redirect "/"

makeUser ps = User emptyAuthUser ""

------------------------------------------------------------------------------
-- | Functions for handling reading and saving per-person rules

readStoredRulesH :: Application ()
readStoredRulesH = undefined

updateStoredRulesH :: Application ()
updateStoredRulesH = undefined

hintRulesH :: Application ()
hintRulesH = undefined

checkRulesH :: Application ()
checkRulesH = do
  rules <- getParam "rules"
  -- TODO: Grab the rules, parse them to something useful and then verify that the rules so far make sense and return a Bool
  writeLBS $ encode True
