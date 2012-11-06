import System.IO
import Data.Char(toUpper)

main :: IO ()
main = do 
  -- Here, as expected, openFile returns an IO action; in this case,
  -- it's an IO Handle.  So, inh is a Handle, because <- gets the
  -- result of the action.  Similarly for outh.
  inh <- openFile "input.txt" ReadMode
  outh <- openFile "output.txt" WriteMode
  
  -- "mainloop inh outh" evaluates to an IO action whose result is (),
  -- so we can drop it right here, no "<-" needed.  (If we wanted to,
  -- we could write "foo <- mainloop inh outh", but we don't need
  -- "foo" for anything, so we won't bother.)
  mainloop inh outh
  
  -- hClose also returns an IO action whose result is (), so again, we
  -- can drop it here.
  hClose inh
  hClose outh

mainloop :: Handle -> Handle -> IO ()
mainloop inh outh =
  -- hISEOF returns an IO Bool, so ineof is a Bool.
  do ineof <- hIsEOF inh
     if ineof
        -- Here, what we're doing is the opposite of "<-" -- that is,
        -- we're taking a pure value, in this case (), and wrapping it
        -- inside IO.
       then return ()
       else do inpStr <- hGetLine inh
               hPutStrLn outh (map toUpper inpStr)
               mainloop inh outh