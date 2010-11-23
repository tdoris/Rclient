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
import Control.Monad

xt_ARRAY_DOUBLE = 33 :: Word8

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

--- head, tag, tail
data RList a = RListNil | RListCons a a (RList a)

to24bit :: Int -> (Word8,Word8,Word8)
to24bit i = undefined

--this doesn't work too well because of a known bug with decode causing a read beyond the end of the supplied bytestring, bo?
instance Binary RSEXP where
  put (XTArrayDouble arr) = putWord8 xt_ARRAY_DOUBLE >> putWord16le (fromIntegral(length arr)::Word16) >> putWord8 0
  get = do t <- getWord8 
           case t of 
            xt_ARRAY_DOUBLE -> do v <- get
                                  return (XTArrayDouble [v])

decodeRsexp :: Get RSEXP
decodeRsexp = do t <- getWord8 
                 case fromIntegral(t)::Int of 
                   33 -> do l <- getWord16le
                            l' <- getWord8 
                            v1 <- getFloat64le
                            v2 <- getFloat64le
                            return (XTArrayDouble [fromIntegral(fromIntegral(xt_ARRAY_DOUBLE)::Int)::Double,fromIntegral(fromIntegral(t)::Int)::Double,fromIntegral(fromIntegral(l)::Int)::Double, fromIntegral(fromIntegral(l')::Int)::Double,v1,v2])
                   _ -> decodeRsexp

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
  let cmd = "rnorm(12)\n" :: String
  rTest h cmd
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


rTest :: Handle->String -> IO ()
rTest h cmd = do
  let msg = createMessage cmdEval cmd
  putStrLn("request:"++lazyByteStringToString msg)
  BL.hPut h msg
  header <- BL.hGet h 16
  let (cmd, len, dof,res) = runGet deserialiseHeader header
  let bodyLength = fromIntegral(len) :: Int
  putStrLn ("header:" ++ lazyByteStringToString header)
  putStrLn ("bodylen:"++ show bodyLength)
  body <- readBytes h bodyLength
  putStrLn ("header:" ++ lazyByteStringToString header ++ "\nbody:"++lazyByteStringToString body)
  let rsexp = runGet decodeRsexp body
  putStrLn ("value:" ++ if bodyLength > 0 then show rsexp else "")
  cmd' <- getLine 
  rTest h cmd'

deserialiseHeader :: Get (Word32, Word32, Word32, Word32)
deserialiseHeader = do
  cmd <- getWord32le
  len <- getWord32le
  dof <- getWord32le
  res <- getWord32le
  return (cmd, len, dof, res)

readBytes :: Handle -> Int -> IO BL.ByteString
readBytes _ l | l == 0 = return (BL.pack "")
readBytes h l = BL.hGet h l

byteStringToString :: B.ByteString->String
byteStringToString r = show (map ord (B.unpack r))
   
lazyByteStringToString :: BL.ByteString->String
lazyByteStringToString r = show (map ord (BL.unpack r))

