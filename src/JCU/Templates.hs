{-# LANGUAGE OverloadedStrings #-}

module JCU.Templates where

import            Control.Monad
import            Application
import            Snap.Types
import            Text.Blaze.Html5 hiding (header,footer)
import qualified  Text.Blaze.Html5 as H
import qualified  Text.Blaze.Html5.Attributes as A
import            Text.Blaze.Renderer.Utf8

-- replaces the layout.tpl file
template :: Bool -> Html -> Html
template loadApp content = docTypeHtml $
  do header loadApp
     doc loadApp content

doc :: Bool -> Html -> Html
doc loadApp content =
  H.body $
    H.div ! A.id "doc" $ do
      H.div ! A.id "hd" $ do
        H.span ! A.id "header" $ do
          img ! A.src jcuLogo64 ! A.alt "JCU logo"
          text "Module Functioneel en Logisch Programmeren"
        when loadApp $ do
          H.span ! A.id "logout" $ H.a ! A.href "/logout" $ text "Logout"
      H.div ! A.id "bd" $ content
      H.div ! A.id "ft" $ do
        img ! A.src "/img/uulogo.png" ! A.id "uulogo" ! A.alt "UU Logo"
  where
    jcuLogo64  = "img/jculogo-64.png"

header :: Bool -> Html
header loadApp =
  H.head $ do
       H.title "JCU: Module Functioneel en Logische Programmeren"
       link ! A.rel "stylesheet" ! A.type_ "text/css" ! A.href cssBase
       link ! A.rel "stylesheet" ! A.type_ "text/css" ! A.href cssFonts
       link ! A.rel "stylesheet" ! A.type_ "text/css" ! A.href cssGrids
       link ! A.rel "stylesheet" ! A.type_ "text/css" ! A.media "screen" ! A.href mainCss
       link ! A.rel "icon" ! A.type_ "image/png" ! A.href jcuLogo16
       when loadApp $ do
         H.script ! A.src "brunch/build/web/js/app.js" $ text ""
         H.script $ text "require('main');"
  where
    cssBase    = "http://yui.yahooapis.com/3.3.0/build/cssbase/base-min.css"
    cssFonts   = "http://yui.yahooapis.com/3.3.0/build/cssfonts/fonts-min.css"
    cssGrids   = "http://yui.yahooapis.com/3.3.0/build/cssgrids/grids-min.css"
    mainCss    = "brunch/build/web/css/main.css"
    jcuLogo16  = "img/jculogo-16.png"


-- Replaces the signup.tpl file
signup :: Html
signup =
  H.div ! A.id "home-view" $ do
    h1 $ text "Please sign up"
    H.form ! A.method "post" ! A.action "/signup" $ do
      table $ do
        H.tr $ do
          H.td (text "Email address")
          H.td (input ! A.type_ "text" ! A.name "email")
        H.tr $ do
          H.td (text "Password")
          H.td (input ! A.type_ "password" ! A.name "password")
        H.tr $ do
          H.td (return ())
          H.td (input ! A.type_ "submit" ! A.value "Signup")

-- Replaces the login.tpl file
login :: Html
login =
  H.div ! A.id "home-view" $ do
    h1 $ text "Please log in"
    H.form ! A.method "post" ! A.action "/login" $ do
      table $ do
        H.tr $ do
          H.td (text "Email")
          H.td (input ! A.type_ "text" ! A.name "email")
        H.tr $ do
          H.td (text "Password")
          H.td (input ! A.type_ "password" ! A.name "password")
        H.tr$ do
          H.td $ return ()
          H.td $ do
            H.input ! A.type_ "submit" ! A.value "Login"
            H.input ! A.type_ "checkbox" ! A.value "1" ! A.name "remember"
            text "Remember me?"

index :: Html
index = H.div $ text "JCU: Wiskunde D. The application is either loading, or something went wrong."

blaze :: Html -> Application ()
blaze response = do
  modifyResponse $ addHeader "Content-Type" "text/html; charset=UTF-8"
  writeLBS $ renderHtml response

