{-# LANGUAGE OverloadedStrings #-}
module Web.Bing 
( AuthenticationResultCode 
, BingAddress(..)
, BingLocationResource(..)
, BingResourceSet(..)
, BingPoint(..)
, BingMapParams(..)
, bingBaseUrl
, bingAPIKey
, bingLocationQuery
, bingImageQuery
)
 where

import IHP.Prelude hiding (param, (&), intercalate, unwords)

import ClassyPrelude (print)
import Control.Lens ((^.), (^..), (^?), (.~), (&))
import Control.Monad (fail)

import Data.Text (replace, pack, unpack, intercalate, unwords)

import Data.Aeson (toJSON, FromJSON, Value(..), parseJSON 
                  ,withArray, withText, withObject, (.:?), (.:)
                  )

import Data.Aeson.Types (typeMismatch, prependFailure)
import Data.Aeson.Lens (key, nth , _String, _Double, _Number)
import Data.Aeson.Encode.Pretty

import qualified Data.ByteString.Lazy as LBS

import Data.Vector ((!))

import GHC.Records

import Network.Wreq hiding (statusCode)
import qualified Network.Wreq  as Wreq

-- Bing API docs
--      https://docs.microsoft.com/en-us/bingmaps/rest-services/common-parameters-and-types/location-and-area-types
--      https://docs.microsoft.com/en-us/bingmaps/rest-services/locations/location-data
--      https://docs.microsoft.com/en-us/bingmaps/rest-services/common-response-description

type BingAPIKey = Text

bingBaseUrl :: Text
bingBaseUrl = "http://dev.virtualearth.net/REST/v1/"

--Geography
data Latitude = Latitude Double deriving (Show,Eq) --[-90, +90]
data Longitude = Longitude Double deriving (Show, Eq) --[-180,+180]
data BingPoint = BingPoint {type_ :: Text, coordinates :: [Double]} deriving (Eq, Show)

instance FromJSON BingPoint where
    parseJSON = withObject "BingPoint" $ \v -> BingPoint
                            <$> v .: "type" 
                            <*> v .: "coordinates"
                
--Rectangular Area  :: [South/West Latitude/Longitude, North/East Latitude/Longitue]
data BingBoundingBox = BingBoundingBox Latitude Longitude Latitude Longitude deriving (Show, Eq)
instance FromJSON BingBoundingBox where
    parseJSON v = do
                     [w,x,y,z] <- parseJSON v
                     pure (BingBoundingBox (Latitude w) (Longitude x) (Latitude y) (Longitude z))



data Confidence = Low|Medium|High  deriving (Show, Eq, Ord)

instance FromJSON Confidence where
    parseJSON = withText "Confidence" $ \text ->
            case text of 
                "High"   -> pure High
                "Medium" -> pure Medium
                "Low"    -> pure Low
                _        -> fail "Confidence Level not a known type."
    
data MatchCode = Good|Ambiguous|UpHierarchy deriving (Show, Eq)

instance FromJSON MatchCode where
    parseJSON = withText "MatchCode" $ \text ->
            case text of 
                "Good" -> pure Good 
                "Ambiguous" -> pure Ambiguous
                "UpHierarchy" -> pure UpHierarchy
                _ -> fail "Matchcode Level not a known type."

data UsageType = Display|Route deriving (Show, Eq)

instance FromJSON UsageType where
    parseJSON = withText "UsageType" $ \text ->
            case text of 
                "Display" -> pure Display
                "Route" -> pure Route
                _ -> fail "Unknown UsageType"

data AuthenticationResultCode = ValidCredentials|InvalidCredentials|CredentialsExpired|NotAuthorized|NoCredentials|NoneARC deriving (Show, Eq)

instance FromJSON AuthenticationResultCode where
    parseJSON = withText "AuthenticationResultCode" $ \text ->
            case text of 
                "ValidCredentials" -> pure ValidCredentials
                "InvalidCredentials" -> pure InvalidCredentials
                "CredentialsExpired" -> pure CredentialsExpired
                "NotAuthorized" -> pure NotAuthorized
                "NoCredentials" -> pure NoCredentials
                "None" -> pure NoneARC
                _ -> fail "AuthenticationResultCode not a known value."

--In most cases we want Rooftop which maps a GeocodePoint to the rooftop of a building
data CalculationMethod = Interpolation|InterpolationOffset|Parcel|Rooftop|NoneCM deriving (Show, Eq)

instance FromJSON CalculationMethod where
    parseJSON = withText "CalculationMethod" $ \text ->
                case text of 
                    "Interpolation" -> pure Interpolation
                    "InterpolationOffset" -> pure InterpolationOffset
                    "Parcel" -> pure Parcel
                    "Rooftop" -> pure Rooftop
                    "None" -> pure NoneCM
                    _ -> fail "Invalid Option in CalculationMethod."

--Bing Data Structures:: BingResponse, 
--Refactor this into a GADT so that the resourceSet 
data BingResponse = BingResponse { statusCode :: Int
                                 , statusDescription :: Text
                                 , authenticationResultCode :: AuthenticationResultCode
                                 , traceId :: Text
                                 , copyright :: Text
                                 , brandLogoUri :: Text
                                 , resourceSets :: [BingResourceSet]
                                 , errorDetails :: Maybe [Text]
                                  } deriving (Show)

instance FromJSON BingResponse where
    parseJSON (Object v) = BingResponse
                            <$>  v .: "statusCode"
                            <*>  v .: "statusDescription"
                            <*>  v .: "authenticationResultCode"
                            <*>  v .: "traceId"
                            <*>  v .: "copyright"
                            <*>  v .: "brandLogoUri"
                            <*>  v .: "resourceSets" --(might want to throttle this with a `take 10`)
                            <*>  v .:? "errorDetails"
    parseJSON invalid = prependFailure "parsing BingResponse failed, " (typeMismatch "Object" invalid)

--can contain a variety of  ResourceTypes e.g. [BingLocationResource], [BingElevationResource]
data BingResourceSet = BingResourceSet {
                                         estimatedTotal :: Int
                                       , resources :: [BingLocationResource]
                                       } deriving (Show)

instance FromJSON BingResourceSet where
    parseJSON (Object v) = BingResourceSet
                                <$> v .: "estimatedTotal"
                                <*> v .: "resources" 
    parseJSON invalid = prependFailure "parsing BingResourceSet failed, " (typeMismatch "Object" invalid)

data BingLocationResource = BingLocationResource {
                                                   name :: Text
                                                 , point :: BingPoint
                                                 , bbox :: BingBoundingBox
                                                 , entityType :: Text
                                                 , address :: BingAddress 
                                                 , confidence :: Confidence
                                                 , matchCodes :: [MatchCode] 
                                                 , queryParseValues :: Maybe [QueryParseValue]
                                                 , geocodePoints :: [BingGeocodePoint]
                                                  } deriving (Show)

instance FromJSON BingLocationResource where
    parseJSON  = withObject "BingLocationResource" $ \v -> BingLocationResource 
                                <$> v .: "name"
                                <*> v .: "point"
                                <*> v .: "bbox"
                                <*> v .: "entityType" 
                                <*> v .: "address"
                                <*> v .: "confidence" 
                                <*> v .: "matchCodes" 
                                <*> v .:? "queryParseValues" 
                                <*> v .: "geocodePoints"

data BingAddress = BingAddress {
                                 addressLine :: Maybe Text
                               , locality :: Maybe Text
                               , neighborhood :: Maybe Text 
                               , adminDistrict :: Maybe Text
                               , adminDistrict2 :: Maybe Text
                               , formattedAddress :: Maybe Text
                               , postalCode :: Maybe Text
                               , countryRegion :: Maybe Text
                               , countryRegionIso2 :: Maybe Text
                               , landmark :: Maybe Text
                                } deriving (Show)

instance FromJSON BingAddress where
    parseJSON  = withObject "BingAddress" $ \v -> BingAddress 
                            <$> v .:? "addressLine"
                            <*> v .:? "locality"
                            <*> v .:? "neighborhood "
                            <*> v .:? "adminDistrict"
                            <*> v .:? "adminDistrict2"
                            <*> v .:? "formattedAddress"
                            <*> v .:? "postalCode"
                            <*> v .:? "countryRegion" 
                            <*> v .:? "countryRegionIso2"
                            <*> v .:? "landmark"

data BingGeocodePoint = BingGeocodePoint {
                                           type_ :: Text
                                         , coordinates :: [Double]
                                         , calculationMethod :: CalculationMethod
                                         , usageTypes :: [UsageType]
                                         } deriving (Show)

instance FromJSON BingGeocodePoint where
    parseJSON = withObject "BingGeocodePoint" $ \v -> 
                            BingGeocodePoint 
                            <$> v .: "type" 
                            <*> v .: "coordinates" 
                            <*> v .: "calculationMethod" 
                            <*> v .: "usageTypes" 

data QueryParseValue = QueryParseValue {
                                         property :: Text
                                       , value :: Text
                                       } deriving (Show)
                                          

instance FromJSON QueryParseValue where
    parseJSON (Object v) = QueryParseValue
                             <$> v .: "property"
                             <*> v .: "value"
    parseJSON invalid = prependFailure "parsing QueryParseValue failed, " (typeMismatch "Object" invalid)

--Query Should be from specific to general addressLine -> Country
addressRecordToList :: BingAddress -> [Maybe Text]
addressRecordToList address = [addressLine, locality, postalCode, adminDistrict, countryRegion] <*> pure address 

--words/unwords is to remove extra spaces. 
replaceSpace :: Maybe Text -> Text
replaceSpace Nothing = ""
replaceSpace (Just queryStr) = replace " " "%20" (unwords $ words queryStr)

buildBingQueryUrl :: BingAddress -> Text 
buildBingQueryUrl address = unwords $ map replaceSpace $ addressRecordToList address

bingLocationQuery :: BingAPIKey -> BingAddress -> IO ([BingResourceSet])
bingLocationQuery bingAPIKey -> address = do
                               let targetUrl = bingBaseUrl <> "Locations/" <> (buildBingQueryUrl address)
                                   opts = defaults & param "key"  .~ [bingAPIKey]
                                                   & param "output" .~ ["json"]

                               print targetUrl
                               r <- asJSON =<< (getWith opts (unpack targetUrl))
                               --DEBUG print (r ^. responseBody)
                               pure (resourceSets (r ^. responseBody))

-- ?mapSize={mapSize}&pushpin={pushpin}&mapLayer={mapLayer}&format={format}&
-- mapMetadata={mapMetadata}&key={BingMapsAPIKey}"
-- bingImageQuery :: ImageData -> 
-- US Only atm.
-- birdeyeUrl :: Text
-- birdseyeUrl = "Imagery/Map/Birdseye/"
-- data BirdseyeInput = BirdseyeInput {...} 
-- bingBirdseyeQuery :: BirdseyeInput
-- https://docs.microsoft.com/en-us/bingmaps/rest-services/imagery/get-a-static-map#response
-- https://docs.microsoft.com/en-us/bingmaps/rest-services/imagery/get-a-static-map#url-templates
-- imagerySet/centerPoint/zoomLevel

data BingMapParams = BingMapParams { 
                                     imagerySet :: Text
                                   , centerPoint :: Text
                                   , zoomLevel :: Text
                                   , mapSize :: Text
                                   }

-- ?mapSize={mapSize}&pushpin={pushpin}&mapLayer={mapLayer}&format={format}&mapMetadata={mapMetadata}&key={BingMapsAPIKey}

imageUrl :: Text
imageUrl = "Imagery/Map/"

bingImageQuery :: BingAPIKey ->  BingMapParams -> IO (Response LBS.ByteString)
bingImageQuery bingAPIkey bingMapParams = do 

                    let 
                        targetUrl = bingBaseUrl <> imageUrl <> (intercalate "/" ([imagerySet, centerPoint, zoomLevel] <*> [bingMapParams]))
                        opts = defaults & param "key" .~ [bingAPIKey]
                                        & param "mmd" .~ ["0"]  -- value of 1 would not return image only metadata 
                                        & param "fmt" .~ ["jpeg"]
                                        & param "ms" .~ [mapSize bingMapParams]
                    r <- (getWith opts (unpack targetUrl))
                    --DEBUG putStrLn $ cs (r ^. responseBody)
                    --putStrLn targetUrl 
                    --putStrLn $ tshow (r ^. responseStatus . Wreq.statusCode)
                    --putStrLn $ tshow (r ^. responseHeader "content-type")
                    pure r

--Locations/countryRegion/adminDistrict/postalCode/locality/addressLine
addressListToUrl :: [Text] -> Text
addressListToUrl bingaddresslist = intercalate "/" bingaddresslist

