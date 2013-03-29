module BT.Util(random256String, getRight, getMaybe, getResult) where
import System.Random (randomIO)
import Numeric (showHex)
import Data.Word (Word64)
import BT.Types
import Control.Exception(throw)
import Text.JSON (Result(Ok, Error))

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

getResult :: Result a -> a
getResult res = case res of
    Ok val -> val
    Error err -> error err

getMaybe :: MyException -> Maybe a -> a
getMaybe b may = case may of
    Just a -> a
    _ -> throw b

getRight :: (a -> MyException) -> Either a b -> b
getRight exc i = case i of
    Right a -> a
    Left b -> throw (exc b)

