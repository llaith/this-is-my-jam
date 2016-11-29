{-# LANGUAGE OverloadedStrings #-}
module Main (
  main
) where

import           Happstack.Server
import           Text.Blaze                  ((!))
import qualified Text.Blaze.Html5            as H
import qualified Text.Blaze.Html5.Attributes as A

import           LastFm


appTemplate :: String -> H.Html -> H.Html
appTemplate title body =
  do H.docType
     H.html $ do
       H.head $ do
         H.title (H.toHtml title)
         H.meta ! A.httpEquiv "Content-Type"
                ! A.content "text/html;charset=utf-8"
       H.body $ body

helloBlaze :: ServerPart Response
helloBlaze = ok $ toResponse $
  appTemplate "Hello, Blaze!"
              (H.p $ do "Hello, "
                        H.b "blaze-html!")

main :: IO ()
main = simpleHTTP nullConf $ helloBlaze
