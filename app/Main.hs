module Main where

import           AWS.Lambda.Context      (HasLambdaContext (withContext))
import           AWS.Lambda.Events.S3    (Records (..), bucket, key, name,
                                          object, s3)
import           AWS.Lambda.Runtime      (mRuntimeWithContext)
import           AwsDynDns.AWS           (getIp, updateResourceRecordSet)
import           Control.Monad.Catch     (MonadCatch, MonadThrow, bracket)
import           Control.Monad.IO.Class  (MonadIO)
import           Control.Monad.Reader    (MonadReader, ReaderT, asks, local,
                                          runReaderT)
import           Control.Monad.Trans.AWS (envLogger, runAWST)
import           Data.Aeson              (Value (..))
import           Data.Text               (Text)
import           Katip                   (ColorStrategy (ColorIfTerminal),
                                          Katip, KatipContext, LogContexts,
                                          LogEnv, Namespace (..),
                                          Severity (InfoS), Verbosity (V2))
import qualified Katip                   as K
import           Lens.Micro              ((&), (.~))
import           Network.AWS             (AWS, Credentials (Discover), MonadAWS,
                                          newEnv, runResourceT)
import           System.Envy             (FromEnv (fromEnv), decodeEnv, env)
import           System.IO               (stdout)


data Environment = Environment {
  hostedZoneId :: Text
}

data Context = Context {
  environment  :: Environment,
  logNamespace :: Namespace,
  logContext   :: LogContexts,
  logEnv       :: LogEnv
}

instance FromEnv Environment where
  fromEnv = Environment <$> env "HOSTED_ZONE_ID"

instance HasLambdaContext Context where
  withContext _ e = e

newtype AwsDynDns a = AwsDynDns {
  runAwsDynDns :: ReaderT Context AWS a
} deriving (Applicative, Functor, Monad, MonadIO, MonadReader Context,
            MonadCatch, MonadThrow, MonadAWS)

instance Katip AwsDynDns where
  getLogEnv = asks logEnv
  localLogEnv f = local (\s -> s { logEnv = f (logEnv s)})

instance KatipContext AwsDynDns where
  getKatipContext = asks logContext
  localKatipContext f = local (\s -> s { logContext = f (logContext s)})
  getKatipNamespace = asks logNamespace
  localKatipNamespace f = local (\s -> s { logNamespace = f (logNamespace s)})

handler :: Records -> AwsDynDns Value
handler Records { records = [] } =
    fail "Got no events, something is very wrong."
handler Records { records = [record] } = do

    Environment { hostedZoneId } <- asks environment

    let targetBucket = (name . bucket . s3) record
    let targetKey    = (key . object . s3) record

    ipAddress <- getIp targetBucket targetKey

    $(K.logTM) InfoS
        $  K.logStr
        $  "Found '"
        <> ipAddress
        <> "' in '"
        <> targetKey
        <> "'."

    val <- updateResourceRecordSet hostedZoneId targetKey ipAddress

    $(K.logTM) InfoS $ K.logStr $ show val

    return Null
handler Records { records } =
    fail
        $  "Got "
        ++ (show . length $ records)
        ++ " records when we expected 1. "

main :: IO ()
main = do
    result <- decodeEnv :: IO (Either String Environment)
    case result of
        Left  err -> fail err
        Right env -> do
            awsEnv       <- newEnv Discover

            handleScribe <- K.mkHandleScribe ColorIfTerminal stdout InfoS V2
            let
                mkLogEnv =
                    K.registerScribe
                            "stdout"
                            handleScribe
                            K.defaultScribeSettings
                        =<< K.initLogEnv "AwsDynDns" "production"
            bracket mkLogEnv K.closeScribes $ \le -> do
                let ctx = Context env mempty mempty le
                runResourceT . runAWST awsEnv $ runReaderT
                    (runAwsDynDns (mRuntimeWithContext handler))
                    ctx
