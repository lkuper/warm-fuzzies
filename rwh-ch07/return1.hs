import Data.Char(toUpper)

isGreen :: IO Bool
isGreen =
    do putStrLn "Is green your favorite color?"
       inpStr <- getLine
       return ((toUpper . head $ inpStr) == 'Y')
       
-- | A version that factors out as much pure code as possible.
isYes :: String -> Bool
isYes inpStr = (toUpper . head $ inpStr) == 'Y'

isGreenMonadic :: IO Bool
isGreenMonadic =
    do putStrLn "Is green your favorite color?"
       inpStr <- getLine
       return (isYes inpStr)
