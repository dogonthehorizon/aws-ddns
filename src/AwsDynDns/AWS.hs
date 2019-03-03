module AwsDynDns.AWS where

import           Data.Conduit.Binary                          (sinkLbs)
import           Data.Text                                    (Text)
import           Data.Text.Lazy                               (strip, toStrict)
import qualified Data.Text.Lazy.Encoding                      as TE
import           Lens.Micro                                   ((&), (?~))
import           Lens.Micro.Extras                            (view)
import           Network.AWS                                  (MonadAWS, send,
                                                               sinkBody)
import           Network.AWS.Prelude                          (NonEmpty ((:|)))
import           Network.AWS.Route53                          (ChangeAction (Upsert),
                                                               RecordType (A),
                                                               ResourceId (..),
                                                               change,
                                                               changeBatch,
                                                               changeResourceRecordSets,
                                                               resourceRecord,
                                                               resourceRecordSet,
                                                               rrsResourceRecords,
                                                               rrsTTL)
import           Network.AWS.Route53.ChangeResourceRecordSets (ChangeResourceRecordSetsResponse)
import           Network.AWS.S3.GetObject                     (getObject,
                                                               gorsBody)
import           Network.AWS.S3.Types                         (BucketName (..),
                                                               ObjectKey (..))

consNEmpty :: a -> NonEmpty a
consNEmpty = flip (:|) []

-- TODO eventually it'll make sense to validate the IP
getIp :: (MonadAWS m) => Text -> Text -> m Text
getIp bucketName bucketKey = do
    val <- send $ getObject (BucketName bucketName) (ObjectKey bucketKey)
    toStrict . strip . TE.decodeUtf8 <$> sinkBody (view gorsBody val) sinkLbs

-- | Update the given A record in the given hosted zone with the given IP.
updateResourceRecordSet
    :: (MonadAWS m)
    => Text -- ^ The hosted zone id to change record sets in.
    -> Text -- ^ The A record in the hosted zone to update.
    -> Text -- ^ The IP address to point our A record to.
    -> m ChangeResourceRecordSetsResponse
updateResourceRecordSet hostedZoneId recordSetDomain ipAddress =
    send $ changeResourceRecordSets (ResourceId hostedZoneId) $ changeBatch
        (consNEmpty $ change Upsert aRecord)
  where
    aRecord =
        resourceRecordSet recordSetDomain A
            & (rrsResourceRecords ?~ (consNEmpty . resourceRecord $ ipAddress))
            . (rrsTTL ?~ 3600) -- TODO make configurable

