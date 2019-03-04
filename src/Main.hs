module Main where

import           AWS.Lambda.Events.S3 (Records (..), bucket, key, name, object,
                                       s3)
import           AwsDdns.AWS          (getIp, updateResourceRecordSet)
import           AwsDdns.Monad        (AwsDdns (..), Context (..),
                                       Environment (..), runLambda)
import           Control.Monad.Reader (asks)
import           Data.Aeson           (Value (..))
import           Katip                (Severity (InfoS))
import qualified Katip                as K

handler :: Records -> AwsDdns Value
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
main = runLambda handler
