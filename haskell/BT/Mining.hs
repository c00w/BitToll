{-# LANGUAGE OverloadedStrings #-}
module BT.Mining where
import Network.Bitcoin (HashData, blockData, HashData, hdTarget)
import Data.ByteString as B hiding (head)
import qualified Data.ByteString.Char8 as BC
import Prelude hiding (take, drop)
import Data.Text.Encoding as E
import BT.Types (PersistentConns)
import BT.Redis (get, set)
import Numeric (readHex)
import Network.Bitcoin (BTC)

storeMerkleDiff :: PersistentConns -> HashData -> IO ()
storeMerkleDiff conn hashData = do
    let merkle = extractMerkle hashData
    let diffstring = encodeUtf8 $ hdTarget hashData
    res <- setMerkleDiff conn merkle diffstring
    case res of
        False -> storeMerkleDiff conn hashData
        True  -> return ()

extractMerkle :: HashData -> B.ByteString
extractMerkle hash = (take 64). (drop 72) .E.encodeUtf8 .  blockData $ hash

setMerkleDiff :: PersistentConns -> B.ByteString -> B.ByteString -> IO Bool
setMerkleDiff conn merkle diff = set conn "merklediff_" merkle diff

getMerkleDiff :: PersistentConns -> B.ByteString -> IO (Maybe B.ByteString)
getMerkleDiff conn merkle = get conn "merklediff_" merkle

hexDiffToInt :: B.ByteString -> BTC
hexDiffToInt hd = fromIntegral int
    where int = (fst . head . readHex . BC.unpack . B.reverse) hd :: Int

currPayout :: BTC
currPayout = 25

miningDiff :: BTC
miningDiff = 1

payout :: B.ByteString -> BTC
payout hexdiff = (miningDiff * currPayout) / diff
    where diff = (hexDiffToInt hexdiff) :: BTC

