{-# LANGUAGE OverloadedStrings #-}
import BT.Global
import BT.Types
import BT.ZMQ
import BT.Mining
import BT.Util
import BT.User
import Control.Monad (when, liftM)
import Control.Monad.Exception (runEMT, catchWithSrcLoc)
import Network.Bitcoin (BTC)
import Numeric

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC

removeUserQueue :: PersistentConns -> B.ByteString -> BTIO ()
removeUserQueue conn share = do
    username <- getMaybe (RedisException "no share Username") =<< getShareUsername conn share
    _ <- remShareUserQueue conn username share
    return ()

getKeyOwed :: PersistentConns -> BTC -> B.ByteString -> BTIO BTC
getKeyOwed conn end key = do
    amount <- getSharePayout conn key
    percent <- getSharePercentPaid conn key
    return $ (end - percent) * amount

payKeyOwed :: PersistentConns -> BTC -> B.ByteString -> BTIO ()
payKeyOwed conn increment key = do
    amount <- getSharePayout conn key
    percent <- getSharePercentPaid conn key
    _ <- setSharePercentPaid conn key (percent + increment)
    let amount_increment = (-1) * amount * increment
    username <- getMaybe (RedisException "no share Username") =<< getShareUsername conn key
    logMsg $ concat ["Paying unconfirmed user:", show username, " amount:", show amount_increment, " percent:", show increment]
    _ <- incrementUnconfirmedBalance conn username amount_increment
    return ()

handleMine :: PersistentConns -> B.ByteString -> BTIO ()
handleMine conn mine_addr = do

    actual_recv <- liftM (read . BC.unpack) $ send conn $ B.append "recieved" mine_addr :: BTIO BTC

    stored_recv <- getMineRecieved conn

    when (stored_recv < actual_recv) $ do

        logMsg . concat $ ["amount to payout ", show $ actual_recv - stored_recv]
        _ <- setMineRecieved conn actual_recv

        let payout_amount = actual_recv - stored_recv

        mine_keys <- getCurrentMiningShares conn
        _ <- removeGlobalMiningShares conn mine_keys
        mapM_ (removeUserQueue conn) mine_keys

        logMsg . concat $ ["mine_keys", show mine_keys]

        next_level <- liftM realToFrac $ getNextShareLevel conn 0.0

        logMsg . concat $ ["nextsharelevel ", show next_level]

        amount_owed <- liftM sum $ mapM (getKeyOwed conn next_level) mine_keys

        payout_fraction <- case (amount_owed, payout_amount / amount_owed >1.0) of
            (0, _) -> return 1.0
            (_, True) -> return 1.0
            (_, False) -> return $ payout_amount / amount_owed

        logMsg . concat $ ["amount owed ", show amount_owed]

        mapM_ (payKeyOwed conn payout_fraction) mine_keys

        payout conn next_level (payout_amount - amount_owed * payout_fraction)

        return ()
    return ()

payout :: PersistentConns -> BTC -> BTC -> BTIO ()
payout _ _   0 = return ()
payout _ 1.0 _ = return ()
payout conn startlevel payout_amount = when (payout_amount > 0) $ do

    mine_keys <- getGlobalShares conn (fromRat $ toRational startlevel) (fromRat $ toRational startlevel)

    logMsg . concat $ ["mine_keys ", show mine_keys]

    next_level <- liftM realToFrac $ getNextShareLevel conn (fromRat $ toRational startlevel)

    logMsg . concat $ ["next_level", show next_level]

    amount_owed <- liftM sum $ mapM (getKeyOwed conn next_level) mine_keys

    logMsg . concat $ ["amount owed ", show amount_owed]

    payout_fraction <- case (amount_owed, payout_amount / amount_owed > 1.0) of
        (0, _) -> return 1.0
        (_, True) -> return 1.0
        (_, False) -> return $ payout_amount / amount_owed

    mapM_ (payKeyOwed conn payout_fraction) mine_keys

    payout conn next_level (payout_amount - amount_owed * payout_fraction)

main :: IO ()
main = do
    conn <- makeCons
    runEMT $ catchWithSrcLoc (do
        addr <- getMiningAddress conn
        case addr of
            Just a -> handleMine conn a
            Nothing -> return ()
        ) logCatch
