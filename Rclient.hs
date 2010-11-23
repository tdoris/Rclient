module Rclient where 

import Network
import System.IO
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy.Char8 as BL
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

cmdLogin = 1 :: Word32
cmdVoidEval = 2 :: Word32
cmdEval = 3 :: Word32

bin2BS :: Binary a => a -> BL.ByteString
bin2BS = runPut . put 

createMessage :: Word32 ->BL.ByteString -> BL.ByteString
createMessage cmd content = BL.concat (runPut (putWord32le cmd) : runPut (putWord32le (fromIntegral(BL.length content + 1)::Word32)) : bin2BS dof : bin2BS res : content : [bin2BS nullt])
  where res = 0 :: Word32
        dof = 0 :: Word32
        nullt = 0 :: Word32

rTest :: Handle -> IO ()
rTest h = do
  BL.hPut h (createMessage cmdLogin (BL.pack "1+1\n"))
  suckit h
  
suckit :: Handle -> IO ()
suckit h = do
  r <- B.hGetLine h
  putStrLn ("response length:" ++ show (B.length r))
  B.putStrLn r
  suckit h

   
