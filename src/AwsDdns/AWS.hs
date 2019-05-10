module AwsDdns.AWS where

import qualified Data.ByteString.Lazy.Char8                   as BS
import           Data.Conduit.Binary                          (sinkLbs)
import           Data.IP                                      (IPv4, fromIPv4)
import           Data.Text                                    (Text)
import qualified Data.Text                                    as T
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

-- | Read the IP address out of bucketKey in bucketName.
getIp
    :: (MonadAWS m)
    => Text -- ^ The target bucket.
    -> Text -- ^ The target key to read from bucket.
    -> m IPv4
getIp bucketName bucketKey = do
    val <- send $ getObject (BucketName bucketName) (ObjectKey bucketKey)
    read . BS.unpack <$> sinkBody (view gorsBody val) sinkLbs

-- | Convert an IP address to it's 'Text' representation.
--
-- Why we need to jump through such hoops seems silly.
fromIp :: IPv4 -> Text
fromIp = T.intercalate "." . fmap (T.pack . show) . fromIPv4

-- | Update the given A record in the given hosted zone with the given IP.
-- TODO make TTL configurable
-- TODO return something more semantically relevant than the raw AWS response
updateResourceRecordSet
    :: (MonadAWS m)
    => Text -- ^ The hosted zone id to change record sets in.
    -> Text -- ^ The A record in the hosted zone to update.
    -> IPv4   -- ^ The IP address to point our A record to.
    -> m ChangeResourceRecordSetsResponse
updateResourceRecordSet hostedZoneId recordSetDomain ipAddress =
    send $ changeResourceRecordSets (ResourceId hostedZoneId) $ changeBatch
        (consNEmpty $ change Upsert aRecord)
  where
    aRecord =
        resourceRecordSet recordSetDomain A
            & (  rrsResourceRecords
              ?~ (consNEmpty . resourceRecord . fromIp $ ipAddress)
              )
            . (rrsTTL ?~ 3600)
