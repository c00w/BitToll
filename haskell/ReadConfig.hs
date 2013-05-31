import BT.Config (makeConfig, getConfig)
import System.Environment (getArgs)

main :: IO ()
main = do
    args <- getArgs
    config <- makeConfig
    value <- getConfig config (head args) :: IO Int
    print value
