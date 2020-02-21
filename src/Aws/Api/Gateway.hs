{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module Aws.Api.Gateway where

import Control.Lens hiding ((.=))
import Data.Aeson hiding (String)
import Data.Aeson.Casing (aesonDrop, camelCase)
import Data.Aeson.TH (deriveFromJSON, deriveToJSON)
import Data.ByteString (ByteString)
import qualified Data.CaseInsensitive as CI
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.IP
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import GHC.Generics (Generic)
import qualified Network.HTTP.Types as HTTP
import Relude.Bool.Guard (guarded)
import Prelude hiding (decodeUtf8, encodeUtf8)

type Method = Text

type HeaderName = Text

type HeaderValue = Text

type QueryParamName = Text

type QueryParamValue = Text

type PathParamName = Text

type PathParamValue = Text

type StageVarName = Text

type StageVarValue = Text

data RequestIdentity
  = RequestIdentity
      { _riCognitoIdentityPoolId :: Maybe Text,
        _riAccountId :: Maybe Text,
        _riCognitoIdentityId :: Maybe Text,
        _riCaller :: Maybe Text,
        _riApiKey :: Maybe Text,
        _riSourceIp :: Maybe IP,
        _riCognitoAuthenticationType :: Maybe Text,
        _riCognitoAuthenticationProvider :: Maybe Text,
        _riUserArn :: Maybe Text,
        _riUserAgent :: Maybe Text,
        _riUser :: Maybe Text
      }
  deriving (Eq, Show)

instance FromJSON RequestIdentity where
  parseJSON = withObject "RequestIdentity" $ \o ->
    RequestIdentity
      <$> o .:? "cognitoIdentityPoolId"
      <*> o .:? "accountId"
      <*> o .:? "cognitoIdentityId"
      <*> o .:? "caller"
      <*> o .:? "apiKey"
      <*> ((readMaybe =<<) <$> o .:? "sourceIp")
      <*> o .:? "cognitoAuthenticationType"
      <*> o .:? "cognitoAuthenticationProvider"
      <*> o .:? "userArn"
      <*> o .:? "userAgent"
      <*> o .:? "user"

instance ToJSON RequestIdentity where
  toJSON RequestIdentity {..} =
    object
      [ "cognitoIdentityPoolId" .= _riCognitoIdentityPoolId,
        "accountId" .= _riAccountId,
        "cognitoIdentityId" .= _riCognitoIdentityId,
        "caller" .= _riCaller,
        "apiKey" .= _riApiKey,
        "sourceIp" .= (show @String <$> _riSourceIp),
        "cognitoAuthenticationType" .= _riCognitoAuthenticationType,
        "cognitoAuthenticationProvider" .= _riCognitoAuthenticationProvider,
        "userArn" .= _riUserArn,
        "userAgent" .= _riUserAgent,
        "user" .= _riUser
      ]

$(makeLenses ''RequestIdentity)

data Authorizer
  = Authorizer
      { _aPrincipalId :: Maybe Text,
        _aClaims :: Object,
        _aContext :: Object
      }
  deriving (Eq, Show)

instance FromJSON Authorizer where
  parseJSON = withObject "Authorizer" $ \o ->
    Authorizer
      <$> o .:? "principalId"
      <*> o .:? "claims" .!= mempty
      <*> pure (HashMap.delete "principalId" $ HashMap.delete "claims" o)

instance ToJSON Authorizer where
  toJSON Authorizer {..} =
    object
      [ "principalId" .= _aPrincipalId,
        "claims" .= _aClaims,
        "context" .= _aContext
      ]

$(makeLenses ''Authorizer)

data ProxyRequestContext
  = ProxyRequestContext
      { _prcPath :: Maybe Text,
        _prcAccountId :: Text,
        _prcResourceId :: Text,
        _prcStage :: Text,
        _prcRequestId :: Text,
        _prcIdentity :: RequestIdentity,
        _prcResourcePath :: Text,
        _prcHttpMethod :: Text,
        _prcApiId :: Text,
        _prcProtocol :: Maybe Text,
        _prcAuthorizer :: Maybe Authorizer
      }
  deriving (Eq, Show)

deriveFromJSON (aesonDrop 4 camelCase) ''ProxyRequestContext

deriveToJSON (aesonDrop 4 camelCase) ''ProxyRequestContext

$(makeLenses ''ProxyRequestContext)

-- | https://is.gd/zu6Wgo <-- request schema
data ProxyRequest body
  = ProxyRequest
      { _proxyRequestResource :: Text,
        _proxyRequestPath :: ByteString,
        _proxyRequestHttpMethod :: HTTP.Method,
        _proxyRequestHeaders :: HTTP.RequestHeaders,
        _proxyRequestQueryStringParameters :: HTTP.Query,
        _proxyRequestPathParameters :: HashMap PathParamName PathParamValue,
        _proxyRequestStageVariables :: HashMap StageVarName StageVarValue,
        _proxyRequestRequestContext :: ProxyRequestContext,
        _proxyRequestBody :: Maybe body
      }
  deriving (Eq, Show, Generic, Functor)

instance FromJSON body => FromJSON (ProxyRequest body) where
  parseJSON = withObject "ProxyRequest" $ \o ->
    ProxyRequest
      <$> o .: "resource"
      <*> (encodeUtf8 <$> o .: "path")
      <*> (encodeUtf8 <$> o .: "httpMethod")
      <*> (fmap fromAWSHeaders <$> o .:? "headers") .!= mempty
      <*> (fmap fromAWSQuery <$> o .:? "queryStringParameters") .!= mempty
      <*> o .:? "pathParameters" .!= HashMap.empty
      <*> o .:? "stageVariables" .!= HashMap.empty
      <*> o .: "requestContext"
      <*> o .:? "body"
    where
      -- Explicit type signatures so that we don't accidentally tell Aeson
      -- to try to parse the wrong sort of structure

      fromAWSHeaders :: HashMap HeaderName HeaderValue -> HTTP.RequestHeaders
      fromAWSHeaders = fmap toHeader . HashMap.toList
        where
          toHeader = bimap (CI.mk . encodeUtf8) encodeUtf8
      fromAWSQuery :: HashMap QueryParamName QueryParamValue -> HTTP.Query
      fromAWSQuery = fmap toQueryItem . HashMap.toList
        where
          toQueryItem :: (QueryParamName, QueryParamValue) -> HTTP.QueryItem
          toQueryItem =
            bimap
              encodeUtf8
              (fmap encodeUtf8 . guarded (not . Text.null))

instance ToJSON body => ToJSON (ProxyRequest body) where
  toJSON ProxyRequest {..} =
    object
      [ "resource" .= _proxyRequestResource,
        "path" .= decodeUtf8 _proxyRequestPath,
        "httpMethod" .= decodeUtf8 _proxyRequestHttpMethod,
        "headers" .= toAWSHeaders _proxyRequestHeaders,
        "queryStringParameters" .= toAWSQuery _proxyRequestQueryStringParameters,
        "pathParameters" .= _proxyRequestPathParameters,
        "stageVariables" .= _proxyRequestStageVariables,
        "requestContext" .= _proxyRequestRequestContext,
        "body" .= _proxyRequestBody
      ]
    where
      toAWSHeaders :: HTTP.ResponseHeaders -> HashMap HeaderName HeaderValue
      toAWSHeaders =
        HashMap.fromList
          . fmap (bimap (decodeUtf8 . CI.original) decodeUtf8)
      toAWSQuery :: HTTP.Query -> HashMap QueryParamName QueryParamValue
      toAWSQuery = HashMap.fromList . fmap fromQueryItem
        where
          fromQueryItem :: HTTP.QueryItem -> (QueryParamName, QueryParamValue)
          fromQueryItem = bimap decodeUtf8 (maybe "" decodeUtf8)

makeLensesWith abbreviatedFields ''ProxyRequest

data ProxyResponse body
  = ProxyResponse
      { _proxyResponseStatusCode :: Int,
        _proxyResponseHeaders :: HTTP.ResponseHeaders,
        _proxyResponseBody :: Maybe body
      }
  deriving (Eq, Show, Generic, Functor)

instance ToJSON body => ToJSON (ProxyResponse body) where
  toJSON ProxyResponse {..} =
    object
      [ "statusCode" .= _proxyResponseStatusCode,
        "headers" .= toAWSHeaders _proxyResponseHeaders,
        "body" .= _proxyResponseBody
      ]
    where
      toAWSHeaders :: HTTP.ResponseHeaders -> HashMap HeaderName HeaderValue
      toAWSHeaders =
        HashMap.fromList
          . fmap (bimap (decodeUtf8 . CI.original) decodeUtf8)

instance FromJSON body => FromJSON (ProxyResponse body) where
  parseJSON = withObject "ProxyResponse" $ \o ->
    ProxyResponse
      <$> o .: "statusCode"
      <*> (fromAWSHeaders <$> o .: "headers")
      <*> o .:? "body"
    where
      -- Explicit type signatures so that we don't accidentally tell Aeson
      -- to try to parse the wrong sort of structure

      fromAWSHeaders :: HashMap HeaderName HeaderValue -> HTTP.RequestHeaders
      fromAWSHeaders = fmap toHeader . HashMap.toList
        where
          toHeader = bimap (CI.mk . encodeUtf8) encodeUtf8

$(makeLenses ''ProxyResponse)

response :: HTTP.Status -> ProxyResponse body
response status = ProxyResponse (HTTP.statusCode status) mempty Nothing

responseBody :: Setter' (ProxyResponse body) (Maybe body)
responseBody = proxyResponseBody . at ()

type Request = ProxyRequest ByteString

type Response = ProxyResponse ByteString
