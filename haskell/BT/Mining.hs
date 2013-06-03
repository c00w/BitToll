{-# LANGUAGE OverloadedStrings #-}
module BT.Mining where
import Network.Bitcoin (HashData, blockData, HashData, hdTarget, BTC)
import qualified Data.ByteString as B hiding (head)
import qualified Data.ByteString.Char8 as BC
import Prelude hiding (take, drop)
import Data.Text.Encoding as E
import BT.Types
import BT.Redis
import BT.Util
import Numeric (readHex)
import Data.IORef (readIORef)
import Control.Monad (when, liftM, unless)

getMiningAddress :: PersistentConns -> IO (Maybe B.ByteString)
getMiningAddress conn = get conn "g:" "global" "mining_address"

setMiningAddress :: PersistentConns -> B.ByteString -> IO Bool
setMiningAddress conn = setnx conn "g:" "global" "mining_address"

storeMerkleDiff :: PersistentConns -> HashData -> IO ()
storeMerkleDiff conn hashData = do
    let merkle = extractMerkle hashData
    let diffstring = encodeUtf8 $ hdTarget hashData
    res <- setMerkleDiff conn merkle diffstring
    unless res (storeMerkleDiff conn hashData)

extractMerkle :: HashData -> B.ByteString
extractMerkle hash = BC.take 64 . BC.drop 72 .E.encodeUtf8 . blockData $ hash

extractMerkleRecieved :: String -> B.ByteString
extractMerkleRecieved hash = BC.take 64 . BC.drop 72 . BC.pack $ hash

setMerkleDiff :: PersistentConns -> B.ByteString -> B.ByteString -> IO Bool
setMerkleDiff conn merkle value = do
    resp <- set conn "m:" merkle "diff" value
    _ <- expire conn "m:" merkle 600
    return resp

getMerkleDiff :: PersistentConns -> B.ByteString -> IO (Maybe B.ByteString)
getMerkleDiff conn merkle = get conn "m:" merkle "diff"

hexDiffToInt :: B.ByteString -> BTC
hexDiffToInt hd = fromIntegral int
    where int = (fst . head . readHex . BC.unpack . B.reverse) hd :: Int

getPayout :: PersistentConns -> B.ByteString -> IO BTC
getPayout conn hexdiff = do
    payout <- readIORef . curPayout $ conn
    miningDiff <- readIORef . curTarget $ conn
    let diff = hexDiffToInt hexdiff :: BTC
    return $ (miningDiff * payout) / diff

setShareUsername :: PersistentConns -> B.ByteString -> B.ByteString -> IO Bool
setShareUsername conn shareid = setnx conn "s:" shareid "username"

getShareUsername :: PersistentConns -> B.ByteString -> IO (Maybe B.ByteString)
getShareUsername conn shareid = get conn "s:" shareid "username"

setSharePayout :: PersistentConns -> B.ByteString -> BTC -> IO Bool
setSharePayout conn shareid = setbtc conn "s:" shareid "payout"

getSharePayout :: PersistentConns -> B.ByteString -> IO BTC 
getSharePayout conn shareid = getbtc conn "s:" shareid "payout"


getMineRecieved :: PersistentConns -> IO BTC
getMineRecieved conn = getbtc conn "g:" "global" "mine_recieved"

setMineRecieved :: PersistentConns -> BTC -> IO Bool
setMineRecieved conn = setbtc conn "g:" "global" "mine_recieved"

incrementSharePayout :: PersistentConns -> B.ByteString -> BTC -> IO BTC
incrementSharePayout conn shareid = incrementbtc conn "s:" shareid "payout"

setSharePercentPaid :: PersistentConns -> B.ByteString -> BTC -> IO Bool
setSharePercentPaid conn shareid payout = set conn "s:" shareid "percentpaid" (BC.pack . show $ payout)

getSharePercentPaid :: PersistentConns -> B.ByteString -> IO BTC
getSharePercentPaid conn shareid = do
    resp <- liftM (getMaybe (RedisException "No share set")) $ get conn "s:" shareid "percentpaid"
    return $ (read . BC.unpack) resp

getUserShares :: PersistentConns -> B.ByteString -> Double -> Double -> IO [B.ByteString]
getUserShares conn = zrangebyscore conn "us:"

getGlobalShares :: PersistentConns -> Double -> Double -> IO [B.ByteString]
getGlobalShares conn = zrangebyscore conn "us:" "global_"


getNextShareLevel :: PersistentConns -> Double -> IO Double
getNextShareLevel conn start = do
    totalwrap <- zrangebyscoreWithscores conn "us:" "global" start 1.0
    let total = filter (\s -> snd s > start) totalwrap
    case total of
        x:_ -> return $ snd x
        [] -> return 1.0

--- Make a share object containing a payout, percent paid, username
--- Also adds it to the appropriate indices
makeShare :: PersistentConns -> B.ByteString -> IO B.ByteString
makeShare conn username = do
    shareid <- liftM BC.pack random256String
    resp <- setShareUsername conn shareid username
    when resp $ do
        _ <- setSharePayout conn shareid 0
        _ <- setSharePercentPaid conn shareid 0.0
        _ <- addShareUserQueue conn username shareid 0.0
        _ <- addShareGlobalQueue conn shareid 0.0
        return ()
    if resp then return shareid
       else makeShare conn username

addShareUserQueue :: PersistentConns -> BC.ByteString -> BC.ByteString -> Double -> IO Integer
addShareUserQueue conn username shareid payout = zadd conn "us:" username payout shareid

remShareUserQueue :: PersistentConns -> BC.ByteString -> BC.ByteString -> IO Integer
remShareUserQueue conn username shareid = zrem conn "us:" username [shareid]


addShareGlobalQueue :: PersistentConns -> BC.ByteString -> Double -> IO Integer
addShareGlobalQueue conn shareid payout = zadd conn "us:" "global" payout shareid

getCurrentMiningShares :: PersistentConns -> IO [B.ByteString]
getCurrentMiningShares conn = zrangebyscore conn "us:" "global" 0.0 0.0

removeGlobalMiningShares :: PersistentConns -> [B.ByteString] -> IO Integer
removeGlobalMiningShares conn = zrem conn "us:" "global"

--- Structure
--- username set of all share keys sorted by payout
--- global sorted set of all share keys sorted by payout

--- Get the current mining share
getCurrentMiningShare :: PersistentConns -> B.ByteString -> IO B.ByteString
getCurrentMiningShare conn username = do
    shares <- zrangebyscore conn "us:" username 0.0 0.0
    case Prelude.length shares of
        0 -> makeShare conn username
        _ -> (return.head) shares

