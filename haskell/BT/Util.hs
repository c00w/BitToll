module BT.Util(random256String) where
import System.Random (randomIO)
import Numeric (showHex)
import Data.Word (Word64)

randomNum :: IO Word64 
randomNum = randomIO 

randomString :: IO String 
randomString = do 
    a <- randomNum 
    return $ showHex a "" 

random256String :: IO String 
random256String = do 
    a <- randomString 
    b <- randomString 
    c <- randomString 
    d <- randomString 
    return $ a ++ b ++ c ++ d 

