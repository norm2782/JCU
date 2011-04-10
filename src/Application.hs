{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}

{-

This module defines our application's monad and any application-specific
information it requires.

-}

module Application
  (  Application
  ,  applicationInitializer
  )  where

import            Snap.Auth
import            Snap.Extension
import            Snap.Extension.DB.MongoDB
import            Snap.Extension.Heist.Impl
import            Snap.Extension.Timer.Impl
import            Snap.Extension.Session.CookieSession

------------------------------------------------------------------------------
-- | 'Application' is our application's monad. It uses 'SnapExtend' from
-- 'Snap.Extension' to provide us with an extended 'MonadSnap' making use of
-- the Heist and Timer Snap extensions.
type Application = SnapExtend ApplicationState


------------------------------------------------------------------------------
-- | 'ApplicationState' is a record which contains the state needed by the Snap
-- extensions we're using.  We're using Heist so we can easily render Heist
-- templates, and Timer simply to illustrate the config loading differences
-- between development and production modes.
data ApplicationState = ApplicationState
    {  templateState  :: HeistState Application
    ,  timerState     :: TimerState
    ,  appSessionSt   :: CookieSessionState
    ,  mongoDBState   :: MongoDBState
    }

-- Instantiate your app as a MonadSession
instance HasCookieSessionState ApplicationState where
  getCookieSessionState = appSessionSt


instance MonadAuth Application

instance HasMongoDBState ApplicationState where
  getMongoDBState       = mongoDBState
  setMongoDBState s pa  = pa { mongoDBState = s }

------------------------------------------------------------------------------
instance HasHeistState Application ApplicationState where
    getHeistState      = templateState
    setHeistState s a  = a { templateState = s }


------------------------------------------------------------------------------
instance HasTimerState ApplicationState where
    getTimerState      = timerState
    setTimerState s a  = a { timerState = s }


------------------------------------------------------------------------------
-- | The 'Initializer' for ApplicationState. For more on 'Initializer's, see
-- the documentation from the snap package. Briefly, this is used to
-- generate the 'ApplicationState' needed for our application and will
-- automatically generate reload\/cleanup actions for us which we don't need
-- to worry about.
applicationInitializer :: Initializer ApplicationState
applicationInitializer = do
    heist  <- heistInitializer "resources/templates"
    timer  <- timerInitializer
    cs     <- cookieSessionStateInitializer $ defCookieSessionState
                { csKeyPath    = "config/site-key.txt"
                , csCookieName = "jcu-session" }
    mng    <- mongoDBInitializer (host "127.0.0.1") 27017 "jcu"
    return $ ApplicationState heist timer cs mng
