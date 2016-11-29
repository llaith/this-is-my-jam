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
import           Text.Hamlet.XML
import           Text.XML

import           LastFmSecrets
import           Utilities


data Track = Track {
  trackName       :: String,
  trackMbid       :: Mbid,
  trackUrl        :: Url,
  trackStreamable :: Bool,
  trackArtist     :: String,
  trackArtistMbid :: Mbid,
  trackAlbum      :: String,
  trackAlbumMbid  :: Mbid,
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

                 -- TODO: Do not return Nothing for all missing data, just the suff we care about,
                 --       i.e.: Add Maybe types to some Track fields

                 let Element name attrs nodes = root
                 guard $ name == "lfm"

                 Element _ _ trackNodes <- findNamedElem "track" nodes

                 nameElem       <- findNamedElem "name"       trackNodes
                 nameText       <- elemContent nameElem
                 mbidElem       <- findNamedElem "mbid"       trackNodes
                 mbidText       <- elemContent mbidElem
                 urlElem        <- findNamedElem "url"        trackNodes
                 urlText        <- elemContent urlElem
                 streamableElem <- findNamedElem "streamable" trackNodes
                 streamableText <- elemContent streamableElem
                 let streamableBool = streamableText == "1"

                 Element _ _ artistNodes <- findNamedElem "artist" trackNodes
                 artistNameElem <- findNamedElem "name" artistNodes
                 artistText     <- elemContent artistNameElem
                 artistMbidElem <- findNamedElem "mbid" artistNodes
                 artistMbidText <- elemContent artistMbidElem

                 Element _ _ albumNodes <- findNamedElem "album" trackNodes
                 albumTitleElem <- findNamedElem "title" albumNodes
                 albumText      <- elemContent albumTitleElem
                 albumMbidElem  <- findNamedElem "mbid" albumNodes
                 albumMbidText  <- elemContent albumMbidElem

                 let imageNodes = findNamedElems "image" albumNodes
                 imageElem <- firstJust $ map (\v -> findElemWithAttrEq "size" v imageNodes) ["extralarge", "large", "medium", "small"]
                 imageText <- elemContent imageElem

                 return Track {
                   trackName       = unpack nameText,
                   trackMbid       = unpack mbidText,
                   trackUrl        = unpack urlText,
                   trackStreamable = streamableBool,
                   trackArtist     = unpack artistText,
                   trackArtistMbid = unpack artistMbidText,
                   trackAlbum      = unpack albumText,
                   trackAlbumMbid  = unpack albumMbidText,
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
