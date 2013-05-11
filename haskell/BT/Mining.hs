{-# LANGUAGE OverloadedStrings #-}
module BT.Mining where
import Network.Bitcoin (HashData, blockData, HashData, hdTarget)
import qualified Data.ByteString as B hiding (head)
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

get_mining_address :: PersistentConns -> IO (Maybe B.ByteString)
get_mining_address conn = get conn "g:" "global" "mining_address"

set_mining_address :: PersistentConns -> B.ByteString -> IO (Bool)
set_mining_address conn value = setnx conn "g:" "global" "mining_address" value

storeMerkleDiff :: PersistentConns -> HashData -> IO ()
storeMerkleDiff conn hashData = do
    let merkle = extractMerkle hashData
    let diffstring = encodeUtf8 $ hdTarget hashData
    res <- setMerkleDiff conn merkle diffstring
    case res of
        False -> storeMerkleDiff conn hashData
        True  -> return ()

extractMerkle :: HashData -> B.ByteString
extractMerkle hash = (BC.take 64). (BC.drop 72) .E.encodeUtf8 . blockData $ hash

extractMerkleRecieved :: MiningDataResult -> B.ByteString 
extractMerkleRecieved hash = (BC.take 64) . (BC.drop 72) . BC.pack . result_data $ hash

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

incrementSharePayout :: PersistentConns -> B.ByteString -> BTC -> IO BTC
incrementSharePayout conn shareid payout = incrementbtc conn "s:" shareid "payout" payout

setSharePercentPaid :: PersistentConns -> B.ByteString -> BTC -> IO Bool
setSharePercentPaid conn shareid payout = set conn "s:" shareid "percentpaid" (BC.pack . show $ payout)

getUserShares :: PersistentConns -> B.ByteString -> Double -> Double -> IO [B.ByteString]
getUserShares conn username minscore maxscore = zrangebyscore conn "us:" username minscore maxscore

--- Make a share object containing a payout, percent paid, username
--- Also adds it to the appropriate indices
makeShare :: PersistentConns -> B.ByteString -> IO B.ByteString
makeShare conn username = do
    shareid <- (liftM BC.pack) random256String
    resp <- setShareUsername conn shareid username
    when (resp == True) $ do
        _ <- setSharePayout conn shareid 0
        _ <- setSharePercentPaid conn shareid 0.0
        _ <- addShareUserQueue conn username shareid 0.0
        _ <- addShareGlobalQueue conn shareid 0.0
        return ()
    case resp of
        True -> return shareid
        False -> makeShare conn username

addShareUserQueue :: PersistentConns -> BC.ByteString -> BC.ByteString -> Double -> IO Integer
addShareUserQueue conn username shareid payout = zadd conn "us:" username payout shareid

addShareGlobalQueue :: PersistentConns -> BC.ByteString -> Double -> IO Integer
addShareGlobalQueue conn shareid payout = zadd conn "us:" "global" payout shareid

--- Structure
--- username set of all share keys sorted by payout
--- global sorted set of all share keys sorted by payout

--- Get the current mining share
getCurrentMiningShare :: PersistentConns -> B.ByteString -> IO B.ByteString
getCurrentMiningShare conn username = do
    shares <- zrangebyscore conn "us:" username 0.0 0.0
    case Prelude.length shares of
        0 -> do
            makeShare conn username
        _ -> do (return.head) shares

