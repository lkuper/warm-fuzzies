main = do
  -- Write out a String.
  putStrLn "Greetings!  What is your name?"
  
  -- Read a line from stdin and bind the result to a name.  (getLine
  -- is an IO action; the <- operator pulls the pure result out of
  -- it.)
  inpStr <- getLine
  
  -- Concatenate strings so we can write out a string again.
  putStrLn $ "Welcome to Haskell, " ++ inpStr ++ "!"
  
-- | putStrLn's type is "String -> IO ()".  That is, it takes a string
-- and returns an IO *action*.  Here, "storeAction" is an IO action,
-- too:

storeAction = putStrLn "Greetings!"

-- | When given an IO action, ghci will perform it for yo uon the
-- spot.
-- 
-- *Main> storeAction 
-- Greetings!

-- | getLine's type is "IO String".  That is, it is an IO action.
-- It's weird that getLine doesn't have an arrow type, but, in fact,
-- it makes sense, because an IO action is like a thunk or delayed
-- computation that is invoked by main, by ghci, or inside a "do"
-- block.


-- | Here's what this would look like without "do".  inpStr is the
-- variable bound by a lambda.
altMain =
    putStrLn "Greetings!  What is your name?" >>
    getLine >>=
    (\inpStr -> putStrLn $ "Welcome to Haskell, " ++ inpStr ++ "!")