{-# LANGUAGE OverloadedStrings #-}
import BT.Global
import BT.Types
import BT.ZMQ
import BT.Mining
import BT.Util
import BT.User
import Control.Monad (when, liftM)
import Network.Bitcoin (BTC)
import Numeric

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC

removeUserQueue :: PersistentConns -> B.ByteString -> IO ()
removeUserQueue conn share = do
    username <- (liftM (getMaybe (RedisException "no share Username"))) $ getShareUsername conn share
    _ <- remShareUserQueue conn username share
    return ()

getKeyOwed :: PersistentConns -> BTC -> B.ByteString -> IO BTC
getKeyOwed conn end key = do
    amount <- getSharePayout conn key
    percent <- getSharePercentPaid conn key
    return $ (end - percent) * amount

payKeyOwed :: PersistentConns -> BTC -> B.ByteString -> IO ()
payKeyOwed conn increment key = do
    amount <- getSharePayout conn key
    percent <- getSharePercentPaid conn key
    _ <- setSharePercentPaid conn key (percent + increment)
    let amount_increment = (-1) * amount * increment
    username <- (liftM (getMaybe (RedisException "no share Username"))) $ getShareUsername conn key
    BC.putStrLn $ B.concat ["Paying unconfirmed user:", username, " amount:", (BC.pack.show$ amount_increment), " percent:", (BC.pack.show$ increment)]
    _ <- increment_unconfirmed_balance conn username amount_increment
    return ()

handle_mine :: PersistentConns -> B.ByteString -> IO ()
handle_mine conn mine_addr = do

    actual_recv <- liftM (read . BC.unpack) $ send conn $ B.append "recieved" mine_addr :: IO BTC

    stored_recv <- getMineRecieved conn

    when (stored_recv < actual_recv) $ do

        BC.putStrLn . B.concat $ ["amount to payout ", (BC.pack.show) $ actual_recv - stored_recv]
        _ <- setMineRecieved conn actual_recv

        let payout_amount = actual_recv - stored_recv

        mine_keys <- getCurrentMiningShares conn
        _ <- removeGlobalMiningShares conn mine_keys
        mapM_ (removeUserQueue conn) mine_keys

        BC.putStrLn . B.concat $ ["mine_keys", (BC.pack.show) $ mine_keys]

        next_level <- (liftM realToFrac) $ getNextShareLevel conn 0.0

        BC.putStrLn . B.concat $ ["nextsharelevel", (BC.pack.show) $ next_level]

        amount_owed <- liftM sum $ mapM (getKeyOwed conn next_level) mine_keys

        payout_fraction <- case (amount_owed, payout_amount / amount_owed >1.0) of
            (0, _) -> return 1.0
            (_, True) -> return 1.0
            (_, False) -> return $ payout_amount / amount_owed

        BC.putStrLn . B.concat $ ["amount owed ", (BC.pack.show) $ amount_owed]

        mapM_ (payKeyOwed conn payout_fraction) mine_keys

        payout conn next_level (payout_amount - amount_owed * payout_fraction)

        return ()
    return ()

payout :: PersistentConns -> BTC -> BTC -> IO ()
payout _ _   0 = return ()
payout _ 1.0 _ = return ()
payout conn startlevel payout_amount = when (payout_amount > 0) $ do

    mine_keys <- getGlobalShares conn (fromRat $ toRational startlevel) (fromRat $ toRational startlevel)

    BC.putStrLn . B.concat $ ["mine_keys ", (BC.pack.show) $ mine_keys]

    next_level <- (liftM realToFrac) $ getNextShareLevel conn (fromRat $ toRational startlevel)

    BC.putStrLn . B.concat $ ["next_level", (BC.pack.show) $ next_level]

    amount_owed <- liftM sum $ mapM (getKeyOwed conn next_level) mine_keys

    BC.putStrLn . B.concat $ ["amount owed ", (BC.pack.show) $ amount_owed]

    payout_fraction <- case (amount_owed, payout_amount / amount_owed > 1.0) of
        (0, _) -> return 1.0
        (_, True) -> return 1.0
        (_, False) -> return $ payout_amount / amount_owed

    mapM_ (payKeyOwed conn payout_fraction) mine_keys

    payout conn next_level (payout_amount - amount_owed * payout_fraction)

main :: IO ()
main = do
    conn <- makeCons
    addr <- get_mining_address conn
    case addr of
        Just a -> handle_mine conn a
        Nothing -> return ()
