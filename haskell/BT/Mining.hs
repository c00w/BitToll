module BT.Mining where 
import Network.Bitcoin (HashData, blockData)
import Data.ByteString as B
import Prelude hiding (take, drop)
import Data.Text.Encoding as E

extractMerkle :: HashData -> B.ByteString
extractMerkle hash = (take 64). (drop 72) .E.encodeUtf8 .  blockData $ hash
