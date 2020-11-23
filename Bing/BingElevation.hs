module Web.BingElevation 
    ( 
       BingElevationParams (..) 
    ,  BingElevationResource (..)
    ,  bingElevationQuery
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

import Application.Helper.Bing (AuthenticationResultCode, BingAPIKey, bingBaseUrl)

--Elevation lens
-- _elev :: Lens' Input Double
-- _elev = lens (elevation . resources) (\ x y -> ( x {location = ((resource x) { elevations =  })}))
-- https://docs.microsoft.com/en-us/bingmaps/rest-services/elevations/

data BingElevationResource = BingElevationResource {
                                                     __type :: Text
                                                   , elevations :: [Double]
                                                   , zoomLevel :: Int                    
                                                   } deriving (Show)

data BingElevationParams = BingElevationParams {
                                                 elevationPoints :: Text
                                               , elevationHeights :: Text
                                               , elevationBounds :: Text
                                               , elevationSamples :: Text
                                               } deriving (Show)

instance FromJSON BingElevationResource where
    parseJSON = withObject "ElevationResource" $ \v -> BingElevationResource
                            <$> v .: "__type"
                            <*> v .: "elevations"
                            <*> v .: "zoomLevel"
            
data BingResourceSet = BingResourceSet {
                                         estimatedTotal :: Int
                                       , resources :: [BingElevationResource]
                                       } deriving (Show)

instance FromJSON BingResourceSet where
    parseJSON (Object v) = BingResourceSet
                                <$> v .: "estimatedTotal"
                                <*> v .: "resources" 
    parseJSON invalid = prependFailure "parsing BingResourceSet failed, " (typeMismatch "Object" invalid)


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

elevationUrl :: Text
elevationUrl = "Elevation/List"

bingElevationQuery :: BingAPIKey -> BingElevationParams -> IO ([BingElevationResource]) 
bingElevationQuery bingAPIKey bingelevationparams= do 
                                           let targetUrl = bingBaseUrl <> elevationUrl
                                               opts = defaults & param "key" .~ [bingAPIKey]
                                                               & param "heights" .~ [elevationHeights bingelevationparams]
                                                               & param "points" .~ [elevationPoints bingelevationparams]

                                           r <- asJSON =<< (getWith opts (unpack targetUrl))
                                           --DEBUG putStrLn targetUrl 
                                           --putStrLn $ tshow (r ^. responseStatus . Wreq.statusCode)
                                           --putStrLn $ tshow (r ^. responseHeader "content-type")
                                           pure (mconcat $ resources <$> resourceSets (r ^. responseBody))

