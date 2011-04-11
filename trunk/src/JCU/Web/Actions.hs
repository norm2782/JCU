{-# LANGUAGE OverloadedStrings  #-}

module JCU.Web.Actions where

import            Application (Application)
import            Data.Aeson (encode)
import            Data.ByteString as B (ByteString, length)
import            Data.Map
import            Data.Maybe (fromMaybe)
import            JCU.Prolog.Prolog
import            JCU.Web.Types
import            Snap.Auth
import            Snap.Auth.Handlers
import            Snap.Extension.DB.MongoDB ((=:), Document, MonadMongoDB)
import            Snap.Extension.Heist (render)
import            Snap.Extension.Session.CookieSession (setSessionUserId)
import            Snap.Types
import            Text.Email.Validate as E (isValid)

-- TODO: Add a consistent naming scheme and rename all functions here
--
--
-- | Access control related actions
restrict :: (MonadMongoDB m, MonadAuth m) => m b -> m b -> m b
restrict fail succ = do
  authed <- isLoggedIn
  case authed of
    False  -> fail
    True   -> succ

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
siteIndex = restrict loginRedir $ ifTop $ render "index"

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

type FormValidator = [(ByteString, FormField)]
data FormField = FormField  {  isRequired    :: Bool
                            ,  fldValidator  :: (ByteString -> Bool) }

-- TODO: Add support for multiple parameters with the same name
-- TODO: Add support for returning validation errors.
formValidator :: FormValidator
formValidator =  [  ("email",     FormField True (\xs -> True)) -- TODO: Use E.isValid, but first find out how the bytestring mess works
                 ,  ("password",  FormField True (\xs -> B.length xs > 6)) ]

valForm parms (fld, (FormField req val))  | fld `member` parms  = val $ head (parms ! fld)
                                          | otherwise           = not req

-- TODO: Add form validation
signupH :: Application ()
signupH = do
  parms  <- getParams
  let isValid = and [ valForm parms p | p <- formValidator] -- TODO: Explicit conversion to list is ugly. Though, do we really need a map in the first place?
  if isValid
    then  do  email  <- getParam "email"
              pwd    <- getParam "password"
              let u = makeUser email pwd
              au     <- saveAuthUser (authUser u, additionalUserFields u)
              case au of
                Nothing   -> newSignupH
                Just au'  -> do  setSessionUserId $ userId au'
                                 redirect "/"
    else  redirect "/signup" -- TODO: Better handling of invalid forms

makeUser email pwd = User (emptyAuthUser  { userPassword  = fmap ClearText pwd
                                          , userEmail     = email }) ""

------------------------------------------------------------------------------
-- | Functions for handling reading and saving per-person rules

readStoredRulesH :: Application ()
readStoredRulesH = restrict forbiddenH $ undefined

updateStoredRulesH :: Application ()
updateStoredRulesH = restrict forbiddenH $ undefined

hintRulesH :: Application ()
hintRulesH = restrict forbiddenH $ undefined

checkRulesH :: Application ()
checkRulesH = restrict forbiddenH $ do
  rules <- getParam "rules"
  -- TODO: Grab the rules, parse them to something useful and then verify that the rules so far make sense and return a Bool
  writeLBS $ encode True