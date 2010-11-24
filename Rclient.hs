module Rclient where 

import Network
import System.IO
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.Binary
import Data.Binary.Put
import Data.Binary.Get
import Data.Binary.IEEE754
import Data.Char
import Data.Bits
import Control.Monad

xt_ARRAY_DOUBLE = 33 :: Word8

data RHeader = RHeader { headerCmd::Word32, headerLen ::Word32, headerDof::Word32, headerRes::Word32} deriving Show

instance Binary RHeader where
  put r = do putWord32le (headerCmd r) >> putWord32le (headerLen r) >> putWord32le (headerDof r) >> putWord32le (headerRes r)
  get = do c <- getWord32le
           l <- getWord32le
           d <- getWord32le
           r <- getWord32le
           return (RHeader c l d r)

data RMessage = RMessage { messageCmd :: Word32, messageContent::BL.ByteString}

--instance Binary RMessage where
--  put m = do putWord32le (messageCmd m) >> putWord32le (fromIntegral(length content'+4)::Word32 >> putWord32 0 >> putWord32 0 
createMessage :: Word32 ->String -> BL.ByteString
createMessage cmd content = BL.concat (runPut (putWord32le cmd >> putWord32le (fromIntegral(length content'+4)::Word32) >> put dof >> put res >> putWord8 dt_string >> putWord16le (fromIntegral (length content')) >> putWord8 0) : [BL.pack content'] )
  where res = 0 :: Word32
        dof = 0 :: Word32
        nullt = 0 :: Word32
        content' = content ++ take (rem (length content) 4) (repeat '\0')
data RSEXP = XTInt Int | XTDouble Double | XTStr String | XTSym String | XTBool Bool 
  | XTVector [RSEXP] 
  | RList RSEXP -- head, tag and tail
  | XTClos RSEXP RSEXP -- formals, body
  | XTArrayInt [Int]
  | XTArrayDouble [Double]
  | XTArrayString [String]
  | XTArrayBool [Bool]
  | XTRaw [Word8]
  | XTArrayComplex [(Double, Double)]
  deriving (Show)

instance Binary RSEXP where
  put (XTArrayDouble arr) = do putWord8 xt_ARRAY_DOUBLE >> putWord16le (fromIntegral(length arr)::Word16) >> putWord8 0
  get = do t <- getWord8 
           case fromIntegral(t)::Int of 
             10 -> do l1 <- getWord8
                      l2 <- getWord8
                      l3 <- getWord8
                      e <- get
                      return (RList e)
             33 -> do l1 <- getWord8
                      l2 <- getWord8 
                      l3 <- getWord8
                      let len = (from24Bitle l1 l2 l3) `div` 8
                      doubles <- sequence (replicate len getFloat64le)
                      return (XTArrayDouble doubles)
             _  -> error ("unsuppported RSEXP type code:"++show (fromIntegral(t)::Int))

data RList a = RListNil | RListCons a a (RList a)

to24bit :: Int -> (Word8,Word8,Word8)
to24bit i = (x,y,z)
  where x = fromIntegral(i .&. 0xff)::Word8
        y = fromIntegral((i `shiftR` 8) .&. 0xff)::Word8
        z = fromIntegral((i `shiftR` 16) .&. 0xff)::Word8

from24Bitle :: Word8 -> Word8 -> Word8 -> Int
from24Bitle x y z = (fromIntegral(x)::Int) + (fromIntegral(y)::Int) `shiftL` 8 + (fromIntegral(z)::Int) `shiftL` 16

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
  -- the Rserve server sends some junk like newlines and a couple of dashes, drain that and throw it away 
  B.hGetNonBlocking h 10000
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


rRepl :: IO()
rRepl = do
  conn <- rConnection "localhost" 6311
  rReplLoop conn
    
rReplLoop :: RConn -> IO()
rReplLoop conn = do
  cmd <- getLine
  result <- rEval conn cmd
  let rsexp = (decode result) :: RSEXP
  putStrLn ("value:" ++ if BL.length result > 0 then show rsexp else "")
  rReplLoop conn
   
rEval :: RConn->String -> IO BL.ByteString
rEval (RConn h) cmd = do
  let msg = createMessage cmdEval cmd
  putStrLn("request:"++lazyByteStringToString msg)
  BL.hPut h msg
  header <- BL.hGet h 16
  let rheader = (decode header) :: RHeader
  let bodyLength = fromIntegral(headerLen rheader) :: Int
  putStrLn ("header:" ++ lazyByteStringToString header)
  putStrLn ("bodylen:"++ show bodyLength)
  body <- readBytes h bodyLength
  putStrLn ("body:"++lazyByteStringToString body)
  return body

readBytes :: Handle -> Int -> IO BL.ByteString
readBytes _ l | l == 0 = return (BL.pack "")
readBytes h l = BL.hGet h l

byteStringToString :: B.ByteString->String
byteStringToString r = show (map ord (B.unpack r))
 
lazyByteStringToString :: BL.ByteString->String
lazyByteStringToString r = show (map ord (BL.unpack r))

