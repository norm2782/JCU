{-# LANGUAGE OverloadedStrings  #-}

{-|

This is where all the routes and handlers are defined for your site. The
'site' function combines everything together and is exported by this module.

-}

module Router
  ( router
  ) where

import            Application
import            Control.Applicative
import            JCU.Handlers
import            Snap.Util.FileServe
import            Snap.Types

------------------------------------------------------------------------------
-- | The main entry point handler.
router :: Application ()
router =  route  [  ("/",           siteIndex)
                 ,  ("/forbidden",  forbiddenH)
                 ,  ("/login",   method GET   newSessionH)
                 ,  ("/login",   method POST  loginH)
                 ,  ("/logout",  logoutH)
                 ,  ("/signup",  method GET   newSignupH)
                 ,  ("/signup",  method POST  signupH)
                 ,  ("/rules/stored",  method GET   readStoredRulesH)
                 ,  ("/rules/stored",  method PUT   updateStoredRulesH)
                 ,  ("/rules/stored",  method POST   addStoredRuleH) -- TODO: For adding individual rule. See if this is the desired approach
                 ,  ("/rules/stored/:id",  method DELETE  deleteStoredRuleH)
                 ,  ("/proof/check",   method POST  checkProofH)
                 ,  ("/rules/check",   method GET   checkH) -- TODO: Remove after done testing
                 ,  ("/rules/unify",   method POST  unifyH)
                 ,  ("/populate", method GET populateH) -- TODO: Remove
                 ]
          <|> serveDirectory "resources/static"
