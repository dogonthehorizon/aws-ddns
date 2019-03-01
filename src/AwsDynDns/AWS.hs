module AwsDynDns.AWS where

import           Data.Aeson               (decode)
import           Data.Conduit.Binary      (sinkLbs)
import           Data.Text                (Text)
import           Data.Text.Lazy           (toStrict)
import qualified Data.Text.Lazy.Encoding  as TE
import           Lens.Micro.Extras        (view)
import           Network.AWS              (MonadAWS, send, sinkBody)
import           Network.AWS.S3.GetObject (getObject, gorsBody)
import           Network.AWS.S3.Types     (BucketName (..), ObjectKey (..))
import Network.AWS.Route53 (changeResourceRecordSets, ResourceId(..), ChangeAction(Upsert), RecordType(A), resourceRecordSet, changeBatch, change, rrsResourceRecords, resourceRecord)
import Lens.Micro ((&), (.~))

-- TODO eventually it'll make sense to validate the IP
getIp :: (MonadAWS m) => Text -> Text -> m Text
getIp bucketName bucketKey = do
    val <- send $ getObject (BucketName bucketName) (ObjectKey bucketKey)
    toStrict . TE.decodeUtf8 <$> sinkBody (view gorsBody val) sinkLbs

updateResourceRecordSet :: (MonadAWS m) => Text -> Text -> Text -> m ()
updateResourceRecordSet hostedZoneId recordSetDomain ipAddress =
  send $ changeResourceRecordSets (ResourceId hostedZoneId) $
    changeBatch (change Upsert aRecord)
      where aRecord = (resourceRecordSet recordSetDomain A)
                        & rrsResourceRecords .~ resourceRecord ipAddress

