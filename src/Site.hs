{-# LANGUAGE OverloadedStrings  #-}

{-|

This is where all the routes and handlers are defined for your site. The
'site' function combines everything together and is exported by this module.

-}

module Site
  ( site
  ) where

import            Application
import            Control.Applicative
import            JCU.Web.Actions
import            Snap.Extension.Heist
import            Snap.Util.FileServe
import            Snap.Types

------------------------------------------------------------------------------
-- | The main entry point handler.
site :: Application ()
site =  route  [  ("/",        siteIndex)
               ,  ("/login",   method GET newSessionH)
               ,  ("/login",   method POST  $ loginH)
               ,  ("/logout",  logoutH)
               ,  ("/signup",  method GET   $ newSignupH)
               ,  ("/signup",  method POST  $ signupH)
               ,  ("/rules/stored",  method GET   $ readStoredRulesH)
               ,  ("/rules/stored",  method POST  $ updateStoredRulesH)
               ,  ("/rules/hint",    method POST  $ hintRulesH)
               ,  ("/rules/check",   method POST  $ checkRulesH)
               ,  ("/rules/check",   method GET   $ checkH) -- TODO: Remove after done testing
               ]
        <|> serveDirectory "resources/static"
