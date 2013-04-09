import Data.ByteString (ByteString)
import Database.Redis (sendRequest, RedisCtx)

lock
    :: (RedisCtx m f)
    => ByteString -- ^ lock
    -> ByteString -- ^ lock_id
    -> ByteString -- ^ locking_string
    -> ByteString -- ^ hold_time
    -> m (f ByteString)
lock key value seconds = sendRequest (["SET"] ++ [key] ++ [value] ++ ["EX"] ++ [seconds] ++ ["NX"]  )
