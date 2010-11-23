module Rclient where 

import Network
import System.IO
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.Binary
import Data.Binary.Put
import Data.Binary.Get
import Data.Char

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

dt_int        = 1  
dt_char       = 2  
dt_double     = 3  
dt_string     = 4  
dt_bytestream = 5  
dt_sexp       = 10 
dt_array      = 11 
dt_large      = 64 
                              

cmdLogin = 1 :: Word32
cmdVoidEval = 2 :: Word32
cmdEval = 3 :: Word32

bin2BS :: Binary a => a -> BL.ByteString
bin2BS = runPut . put 

createMessage :: Word32 ->String -> BL.ByteString
createMessage cmd content = BL.concat (runPut (putWord32le cmd >> putWord32le (fromIntegral(length content'+4)::Word32) >> put dof >> put res >> putWord8 dt_string >> putWord16le (fromIntegral (length content')) >> putWord8 0) : [BL.pack content'] )
  where res = 0 :: Word32
        dof = 0 :: Word32
        nullt = 0 :: Word32
        content' = content ++ take (rem (length content) 4) (repeat '\0')


rTest :: Handle -> IO ()
rTest h = do
  let cmd = "rnorm(100)\n" :: String
  let msg = createMessage cmdEval cmd
  putStrLn("request:"++lazyByteStringToString msg)
  BL.hPut h msg
  suckit h

deserialiseHeader :: Get (Word32, Word32, Word32, Word32)
deserialiseHeader = do
  cmd <- getWord32le
  len <- getWord32le
  dof <- getWord32le
  res <- getWord32le
  return (cmd, len, dof, res)

suckit :: Handle -> IO ()
suckit h = do
  header <- BL.hGet h 16
  let (cmd, len, dof,res) = runGet deserialiseHeader header
  let bodyLength = fromIntegral(len) :: Int
  body <- BL.hGetNonBlocking h bodyLength
  putStrLn ("header:" ++ lazyByteStringToString header ++ "\nbody:"++lazyByteStringToString body)
  suckit h

byteStringToString :: B.ByteString->String
byteStringToString r = show (map ord (B.unpack r))
   
lazyByteStringToString :: BL.ByteString->String
lazyByteStringToString r = show (map ord (BL.unpack r))
