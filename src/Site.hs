{-# LANGUAGE OverloadedStrings  #-}

{-|

This is where all the routes and handlers are defined for your site. The
'site' function combines everything together and is exported by this module.

-}

module Site
  ( site
  ) where

import            Control.Applicative
import            Data.Maybe
import qualified  Data.Text.Encoding as T
import            Snap.Extension.Heist
import            Snap.Extension.Timer
import            Snap.Util.FileServe
import            Snap.Types
import            Text.Templating.Heist
import            Snap.Auth
import            Snap.Auth.Handlers
import            Application
import            Snap.Extension.DB.MongoDB
import            Snap.Extension (SnapExtend)
import            Snap.Extension.Session.CookieSession
import            Data.ByteString
import            JCU.Prolog
import            Data.Aeson (encode)

-- TODO: Get prolog field out of user? Though it can't hurt too much;
-- there's not a lot of data we want to store anyway.
data User = User  {  authUser     :: AuthUser
                  ,  storedRules  :: ByteString }

------------------------------------------------------------------------------
-- | Renders the front page of the sample site.
--
-- The 'ifTop' is required to limit this to the top of a route.
-- Otherwise, the way the route table is currently set up, this action
-- would be given every request.
siteIndex :: Application ()
siteIndex = ifTop $ render "index"


------------------------------------------------------------------------------
-- | Renders the echo page.
echo :: Application ()
echo = do
    message <- decodedParam "stuff"
    heistLocal (bindString "message" (T.decodeUtf8 message)) $ render "echo"


decodedParam :: MonadSnap m => ByteString -> m ByteString
decodedParam p = fromMaybe "" <$> getParam p



------------------------------------------------------------------------------
-- | Renders the login page
newSessionH :: Application ()
newSessionH = render "login"

newSignupH :: Application ()
newSignupH = render "signup"

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

readStoredRulesH = undefined
updateStoredRulesH = undefined
hintRulesH = undefined

checkRulesH = do
  rules <- getParam "rules"
  -- TODO: Grab the rules, parse them to something useful and then verify that the rules so far make sense.
  writeLBS $ encode True

------------------------------------------------------------------------------
-- | The main entry point handler.
site :: Application ()
site =  route  [  ("/",        siteIndex)
               ,  ("/login",   method GET newSessionH)
               ,  ("/login",   method POST  $  loginHandler "password"
                                               Nothing newSessionH redirHome)
               ,  ("/logout",  logoutHandler redirHome)
               ,  ("/signup",  method GET   $ newSignupH)
               ,  ("/signup",  method POST  $ signupH)
               ,  ("/rules/stored",  method GET   $ readStoredRulesH)
               ,  ("/rules/stored",  method POST  $ updateStoredRulesH)
               ,  ("/rules/hint",    method POST  $ hintRulesH)
               ,  ("/rules/check",   method POST  $ checkRulesH)
               ]
        <|> serveDirectory "resources/static"
