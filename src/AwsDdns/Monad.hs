module AwsDdns.Monad where

import           AWS.Lambda.Context      (HasLambdaContext (withContext))
import           AWS.Lambda.Runtime      (mRuntimeWithContext)
import           Control.Monad.Catch     (MonadCatch, MonadThrow, bracket)
import           Control.Monad.IO.Class  (MonadIO)
import           Control.Monad.Reader    (MonadReader, ReaderT, asks, local,
                                          runReaderT)
import           Control.Monad.Trans.AWS (runAWST)
import           Data.Aeson              (FromJSON, ToJSON)
import           Data.Text               (Text)
import           Katip                   (ColorStrategy (ColorIfTerminal),
                                          Katip (..), KatipContext (..),
                                          LogContexts, LogEnv, Namespace (..),
                                          Severity (InfoS), Verbosity (V2))
import qualified Katip                   as K
import           Network.AWS             (AWS, Credentials (Discover), MonadAWS,
                                          newEnv, runResourceT)
import           System.Envy             (FromEnv (fromEnv), decodeEnv, env)
import           System.IO               (stdout)

newtype Environment = Environment {
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

newtype AwsDdns a = AwsDdns {
  runAwsDdns :: ReaderT Context AWS a
} deriving (Applicative, Functor, Monad, MonadIO, MonadReader Context,
            MonadCatch, MonadThrow, MonadAWS)

instance Katip AwsDdns where
  getLogEnv = asks logEnv
  localLogEnv f = local (\s -> s { logEnv = f (logEnv s)})

instance KatipContext AwsDdns where
  getKatipContext = asks logContext
  localKatipContext f = local (\s -> s { logContext = f (logContext s)})
  getKatipNamespace = asks logNamespace
  localKatipNamespace f = local (\s -> s { logNamespace = f (logNamespace s)})

runLambda :: (FromJSON e, ToJSON r) => (e -> AwsDdns r) -> IO ()
runLambda handler = do
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
                        =<< K.initLogEnv "AwsDdns" "production"
            bracket mkLogEnv K.closeScribes $ \le -> do
                let ctx = Context env mempty mempty le
                runResourceT . runAWST awsEnv $ runReaderT
                    (runAwsDdns (mRuntimeWithContext handler))
                    ctx
