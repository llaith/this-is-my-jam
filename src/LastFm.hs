{-# LANGUAGE OverloadedStrings #-}

module LastFm (
  Mbid,
  Url,
  Track (..),
  getTrackInfo
) where

import           Control.Monad   (guard)
import           Data.List
import qualified Data.Map        as Map
import           Data.Maybe
import           Data.String
import           Data.Text       (Text, unpack)
import           Network.HTTP
import           Prelude         hiding (readFile, writeFile)
import           Text.XML

import           LastFmSecrets
import           Utilities


data Track = Track {
  trackName       :: String,
  trackMbid       :: Maybe Mbid,
  trackUrl        :: Url,
  trackStreamable :: Bool,
  trackArtist     :: String,
  trackArtistMbid :: Maybe Mbid,
  trackAlbum      :: String,
  trackAlbumMbid  :: Maybe Mbid,
  trackImage      :: Url
} deriving (Eq, Show)

type Mbid = String
type Url = String


getTrackInfoUrl :: String -> String -> Url
getTrackInfoUrl artist track = "http://ws.audioscrobbler.com/2.0/?method=track.getInfo&api_key=" ++ apikey ++ "&artist=" ++ artist' ++ "&track=" ++ track'
  where artist' = urlEncode artist
        track' = urlEncode track


getTrackInfo :: String -> String -> IO (Maybe Track)
getTrackInfo artist track =
  do body <- getTrackInfoXML artist track
     putStrLn body
     return $ do Document _ root _ <- (rightToMaybe . parseText def . fromString) body

                 let Element name _ nodes = root
                 guard $ name == "lfm"

                 Element _ _ trackNodes  <- findNamedElem "track" nodes
                 Element _ _ artistNodes <- findNamedElem "artist" trackNodes
                 Element _ _ albumNodes  <- findNamedElem "album" trackNodes

                 nameText           <- getElemString "name" trackNodes
                 let mbidText       =  getElemString "mbid" trackNodes
                 urlText            <- getElemString "url" trackNodes
                 streamableText     <- getElemString "streamable" trackNodes
                 let streamableBool = streamableText == "1"

                 artistText         <- getElemString "name" artistNodes
                 let artistMbidText =  getElemString "mbid" artistNodes

                 albumText          <- getElemString "title" albumNodes
                 let albumMbidText  =  getElemString "mbid" albumNodes

                 let imageNodes = findNamedElems "image" albumNodes
                 imageElem <- firstJust $ map (\v -> findElemWithAttrEq "size" v imageNodes) ["extralarge", "large", "medium", "small"]
                 imageText <- elemContent imageElem

                 return Track {
                   trackName       = nameText,
                   trackMbid       = mbidText,
                   trackUrl        = urlText,
                   trackStreamable = streamableBool,
                   trackArtist     = artistText,
                   trackArtistMbid = artistMbidText,
                   trackAlbum      = albumText,
                   trackAlbumMbid  = albumMbidText,
                   trackImage      = unpack imageText
                 }


getTrackInfoXML :: String -> String -> IO (String)
getTrackInfoXML artist track = do response <- simpleHTTP (getRequest (getTrackInfoUrl artist track))
                                  getResponseBody response


findNamedElem :: Text -> [Node] -> Maybe Element
findNamedElem sName = listToMaybe . findNamedElems sName

findNamedElems :: Text -> [Node] -> [Element]
findNamedElems sName = map (\(NodeElement e) -> e) . filter isElemWithName
  where
    isElemWithName (NodeElement (Element (Name name _ _) _ _)) = name == sName
    isElemWithName _                                           = False


findElemWithAttrEq :: Text -> Text -> [Element] -> Maybe Element
findElemWithAttrEq sName value = listToMaybe . findElemsWithAttrEq sName value

findElemsWithAttrEq :: Text -> Text -> [Element] -> [Element]
findElemsWithAttrEq sAttr value = filter isNodeWirhAttrEq
  where
    isNodeWirhAttrEq (Element _ attrs _) = case Map.lookup (Name sAttr Nothing Nothing) attrs of
                                             Nothing -> False
                                             Just x  -> x == value

elemContent :: Element -> Maybe Text
elemContent (Element _ _ nodes) = do node <- listToMaybe nodes
                                     text <- case node of
                                               NodeContent text -> Just text
                                               otherwise        -> Nothing
                                     return text


getElemString :: Text -> [Node] -> Maybe String
getElemString elemName nodes = fmap unpack $ findNamedElem elemName nodes >>= elemContent