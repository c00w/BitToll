{-# LANGUAGE OverloadedStrings #-}
module BT.Mining where
import Network.Bitcoin (HashData, blockData, HashData, hdTarget)
import Data.ByteString as B hiding (head)
import qualified Data.ByteString.Char8 as BC
import Prelude hiding (take, drop)
import Data.Text.Encoding as E
import BT.Types
import BT.Redis
import BT.Util
import Numeric (readHex)
import Network.Bitcoin (BTC)
import Data.IORef (readIORef)
import Control.Monad (when, liftM)

storeMerkleDiff :: PersistentConns -> HashData -> IO ()
storeMerkleDiff conn hashData = do
    let merkle = extractMerkle hashData
    let diffstring = encodeUtf8 $ hdTarget hashData
    res <- setMerkleDiff conn merkle diffstring
    case res of
        False -> storeMerkleDiff conn hashData
        True  -> return ()

extractMerkle :: HashData -> B.ByteString
extractMerkle hash = (take 64). (drop 72) .E.encodeUtf8 . blockData $ hash

extractMerkleRecieved :: MiningDataResult -> B.ByteString 
extractMerkleRecieved hash = take(64) . (drop 72) . BC.pack . result_data $ hash

setMerkleDiff :: PersistentConns -> B.ByteString -> B.ByteString -> IO Bool
setMerkleDiff conn merkle diff = set conn "m:" merkle "diff" diff

getMerkleDiff :: PersistentConns -> B.ByteString -> IO (Maybe B.ByteString)
getMerkleDiff conn merkle = get conn "m:" merkle "diff"

hexDiffToInt :: B.ByteString -> BTC
hexDiffToInt hd = fromIntegral int
    where int = (fst . head . readHex . BC.unpack . B.reverse) hd :: Int

getPayout :: PersistentConns -> B.ByteString -> IO BTC
getPayout conn hexdiff = do
    payout <- readIORef . curPayout $ conn
    miningDiff <- readIORef . curTarget $ conn
    let diff = (hexDiffToInt hexdiff) :: BTC
    return $ (miningDiff * payout) / diff

setShareUsername :: PersistentConns -> B.ByteString -> B.ByteString -> IO Bool
setShareUsername conn shareid username = setnx conn "s:" shareid "username" username

setSharePayout :: PersistentConns -> B.ByteString -> BTC -> IO Bool
setSharePayout conn shareid payout = setbtc conn "s:" shareid "payout" payout

setSharePercentPaid :: PersistentConns -> B.ByteString -> BTC -> IO Bool
setSharePercentPaid conn shareid payout = set conn "s:" shareid "percentpaid" (BC.pack . show $ payout)

--- Make a share object containing a payout, percent paid, username
makeShare :: PersistentConns -> B.ByteString -> IO B.ByteString
makeShare conn username = do
    shareid <- (liftM BC.pack) random256String
    resp <- setShareUsername conn shareid username
    when (resp == True) $ do
        _ <- setSharePayout conn shareid 0
        _ <- setSharePercentPaid conn shareid 0.0
        return ()
    case resp of
        True -> return shareid
        False -> makeShare conn username

--- Structure
--- username set of all share keys sorted by payout
--- global sorted set of all share keys sorted by payout



--- Get the current mining share
--- getCurrentMiningShare :: PersistentConns -> B.ByteString -> IO B.ByteString
--- getCurrentMiningShare conn username = do
---     zrangebyscore "username"
