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
import            Data.ByteString.Char8 (ByteString, pack)

-- TODO: Get prolog field out of user? Though it can't hurt too much;
-- there's not a lot of data we want to store anyway.
data User = User
  {  authUser   :: AuthUser
  ,  fldProlog  :: ByteString
  }

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
  where
    decodedParam p = fromMaybe "" <$> getParam p



------------------------------------------------------------------------------
-- | Renders the login page
newSessionH :: Application ()
newSessionH = render "login"

newSignupH :: Application ()
newSignupH = render "signup"

redirHome = redirect "/"

additionalUserFields :: User -> Document
additionalUserFields u = [ "prolog" =: fldProlog u ]

signupH :: Application ()
signupH = do
  ps <- getParams
  let u = makeUser ps
  au <- saveAuthUser (authUser u, additionalUserFields u)
  case au of
    Nothing   -> newSignupH
    Just au'  -> do  setSessionUserId $ userId au'
                     redirect "/"

makeUser ps = User (emptyAuthUser  { userPassword  = Just $ ClearText $ pack "foo"
                                   , userEmail     = Just $ pack "foo@bar.com"
                                   }) (pack "")
------------------------------------------------------------------------------
-- | Functions for handling reading and saving per-person rules

readRules = undefined
updateRules = undefined

------------------------------------------------------------------------------
-- | The main entry point handler.
site :: Application ()
site =  route  [ ("/",        siteIndex)
               , ("/login",   method GET newSessionH)
               , ("/login",   method POST $ loginHandler "password" Nothing newSessionH redirHome)
               , ("/logout",  logoutHandler redirHome)
               , ("/signup",  method GET $ newSignupH)
               , ("/signup",  method POST $ signupH)
               , ("/rules",   method GET $ readRules)
               , ("/rules",   method PUT $ updateRules)
               , (""
               ]
        <|> serveDirectory "resources/static"
