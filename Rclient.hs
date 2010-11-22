module Rclient where 

import Network
import System.IO
import Data.ByteString as B
import Data.ByteString.Lazy.Char8 as BL
import Data.Binary
import Data.Binary.Put

rclientVersion :: [Char]
rclientVersion = "1.0.2"

data RConn = RConn Handle deriving (Show)

parseIdString :: B.ByteString -> [B.ByteString]
parseIdString b = let (h,r) = B.splitAt 4 b 
                  in if B.length r > 0 then h : parseIdString r else [h] 

rConnection :: String->Int->IO RConn
rConnection server port = do 
  h <- connectTo server (PortNumber (fromIntegral port))
  hSetBuffering h NoBuffering
  idString <- B.hGetLine h
  let attributes = parseIdString idString
  mapM_ B.putStrLn attributes
  rTest h
  return (RConn h)

cmdLogin = 1 
cmdVoidEval = 2
cmdEval = 3

bin2BS :: Binary a => a -> BL.ByteString
bin2BS x = runPut (put x)

createMessage :: Int ->BL.ByteString -> BL.ByteString
createMessage cmd content = BL.concat (bin2BS cmd : bin2BS (BL.length content - 16) : bin2BS dof : bin2BS res : [content])
  where res = 0 :: Int
        dof = 0 :: Int

rTest :: Handle -> IO ()
rTest h = do
  BL.hPut h (createMessage cmdEval (BL.pack "1+1"))
  response <- B.hGetLine h
  B.putStrLn response
   
