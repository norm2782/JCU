{-# LANGUAGE OverloadedStrings #-}

module JCU.Templates where

import            Control.Monad
import            Control.Monad.Reader
import            Data.Text (Text)
import            Snap.Types
import            Text.Blaze.Html5 hiding (header,footer)
import qualified  Text.Blaze.Html5 as H
import qualified  Text.Blaze.Html5.Attributes as A
import            Text.Blaze.Internal (HtmlM(..))
import            Text.Blaze.Renderer.Utf8
import            Text.Digestive
import            Text.Digestive.Blaze.Html5
import            Text.Digestive.Forms.Snap


-------------------------------------------------------------------------------
-- View

data AuthState = AuthState {
     loggedInST :: Bool
  ,  emailST    :: Text
}


-- replaces the layout.tpl file
template :: Reader AuthState Html -> Reader AuthState Html
template content = do
  h <- header
  d <- doc content
  return $ H.docTypeHtml $ (h >> d)

doc :: Reader AuthState Html -> Reader AuthState Html
doc content = do
  c <- content
  loggedIn <- asks loggedInST
  return $
    H.body $
      H.div ! A.id "doc" $ do
        H.div ! A.id "hd" $ do
          H.span ! A.id "header" $ do
            H.img ! A.src jcuLogo64 ! A.alt "JCU logo"
            H.text "Module Functioneel en Logisch Programmeren"
          when loggedIn $ do
            H.span ! A.id "logout" $ H.a ! A.href "/logout" $ H.text "Logout"
        H.div ! A.id "bd" $ c
        H.div ! A.id "ft" $ do
          H.img ! A.src "/img/uulogo.png" ! A.id "uulogo" ! A.alt "UU Logo"
  where
    jcuLogo64  = "img/jculogo-64.png"

header :: Reader AuthState Html
header = do
  loggedIn <- asks loggedInST
  return $ H.head $ do
    H.title "JCU: Module Functioneel en Logische Programmeren"
    H.link ! A.rel "stylesheet" ! A.type_ "text/css" ! A.href cssBase
    H.link ! A.rel "stylesheet" ! A.type_ "text/css" ! A.href cssFonts
    H.link ! A.rel "stylesheet" ! A.type_ "text/css" ! A.href cssGrids
    H.link ! A.rel "stylesheet" ! A.type_ "text/css" ! A.media "screen" ! A.href mainCss
    H.link ! A.rel "icon" ! A.type_ "image/png" ! A.href jcuLogo16
    when loggedIn $ do
      H.script ! A.src "brunch/build/web/js/app.js" $ H.text ""
      H.script $ H.text "require('main');"
  where
    cssBase    = "http://yui.yahooapis.com/3.3.0/build/cssbase/base-min.css"
    cssFonts   = "http://yui.yahooapis.com/3.3.0/build/cssfonts/fonts-min.css"
    cssGrids   = "http://yui.yahooapis.com/3.3.0/build/cssgrids/grids-min.css"
    mainCss    = "brunch/build/web/css/main.css"
    jcuLogo16  = "img/jculogo-16.png"


-- Replaces the signup.tpl file
signupHTML :: FormHtml (HtmlM a) -> Reader AuthState Html
signupHTML form = return $
  H.div ! A.id "home-view" $ do
    H.h1 $ H.text "Please sign up"
    let (formHtml', enctype) = renderFormHtml form
    H.form  ! A.enctype (toValue $ show enctype) ! A.method "post"
            ! A.action "/signup" $ do
      _ <- formHtml'
      H.input ! A.type_ "submit" ! A.value "Signup"

-- Replaces the login.tpl file
loginHTML :: Bool -> FormHtml (HtmlM a) -> Reader AuthState Html
loginHTML loginFailed form = return $
  H.div ! A.id "home-view" $ do
    H.h1 $ H.text "Please log in"
    when loginFailed $ H.h2 "Incorrect login credentials"
    let (formHtml', enctype) = renderFormHtml form
    H.form  ! A.enctype (toValue $ show enctype) ! A.method "post"
            ! A.action "/login" $ do
      _ <- formHtml'
      H.input ! A.type_ "submit" ! A.value "Login"

index :: Reader AuthState Html
index = return $
  H.div $ H.text "JCU: Wiskunde D. The application is either loading, or something went wrong."

