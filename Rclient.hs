module Rclient where 

import Network
import System.IO
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.ByteString.Internal as BI
import Data.Binary
import Data.Binary.Put
import Data.Binary.Get
import Data.Binary.IEEE754
import Data.Char
import Data.Bits
import Control.Monad

xt_INT = 1 :: Word8
xt_DOUBLE = 2 :: Word8
xt_STR = 3 :: Word8
xt_LANG = 4 :: Word8
xt_SYM = 5 :: Word8
xt_BOOL = 6 :: Word8
xt_S4 = 7 :: Word8
xt_VECTOR = 16 :: Word8
xt_LIST = 17 :: Word8
xt_CLOS = 18 :: Word8
xt_SYMNAME = 19 :: Word8
xt_LIST_NOTAG = 20 :: Word8
xt_LIST_TAG = 21  :: Word8
xt_VECTOR_EXP = 26 :: Word8
xt_VECTOR_STR = 27 :: Word8
xt_ARRAY_INT = 32 :: Word8
xt_ARRAY_DOUBLE = 33 :: Word8
xt_ARRAY_STR = 34 ::Word8
xt_ARRAY_BOOL = 36 ::Word8
xt_RAW = 37 :: Word8
xt_ARRAY_CPLX = 38 :: Word8



cmdLogin = 1 :: Word32
cmdVoidEval = 2 :: Word32
cmdEval = 3 :: Word32

data QAP1Header = QAP1Header { headerCmd::Word32, headerLen ::Word32, headerDof::Word32, headerRes::Word32} deriving Show

instance Binary QAP1Header where
  put r = do putWord32le (headerCmd r) >> putWord32le (headerLen r) >> putWord32le (headerDof r) >> putWord32le (headerRes r)
  get = do c <- getWord32le
           l <- getWord32le
           d <- getWord32le
           r <- getWord32le
           return (QAP1Header c l d r)

data QAP1Message = QAP1Message { qap1Header :: QAP1Header, qap1Content ::DT } deriving Show

instance Binary QAP1Message where
  put m = do put (qap1Header m) >> put (qap1Content m)
  get = do header <- get
           content <- get
           return (QAP1Message header content)

createMessage :: Word32 -> String -> QAP1Message
createMessage cmdId content = QAP1Message (QAP1Header cmdId tlen 0 0) (DTString (content ++ padding))
  where tlen = fromIntegral(length (content++padding) + 4):: Word32
        padding = replicate (max 1 (rem (length content) 4)) '\0'

dt_int        = 1  
dt_char       = 2  
dt_double     = 3  
dt_string     = 4  
dt_bytestream = 5  
dt_sexp       = 10 
dt_array      = 11 
dt_large      = 64 
                              
data DT = DTInt Int | DTChar Char | DTDouble Double | DTString String | DTBytestream [Word8] | DTSexp RSEXP  
  deriving (Show)

instance Binary DT where
  put (DTInt i) = do putWord8 dt_int >> putWord8 0 >> putWord8 0 >>putWord8 0 >> putWord32le (fromIntegral i::Word32)
  put (DTString s) = do putWord8 dt_string >> put len24 >> mapM_ put s 
                        where len24 = to24bit (length s)
  put (DTBytestream s)= do putWord8 dt_bytestream >> put len24 >> mapM_ putWord8 s
                          where len24 = to24bit (length s)
  put (DTSexp s) = do put s
  get = do t <- getWord8 
           case fromIntegral(t)::Int of
              1 -> liftM DTInt get
              2 -> liftM DTChar get
              3 -> liftM DTDouble get
              4 -> do len24 <- get
                      let len = (from24Bit len24) 
                      chars <- sequence (replicate len getWord8)
                      return (DTString (map BI.w2c chars))
              5 -> do len24 <- get
                      let len = (from24Bit len24)
                      words <- sequence (replicate len getWord8)
                      return (DTBytestream words)
              10 -> do sequence_ (replicate 3 getWord8) 
                       e <- get
                       return (DTSexp e)

               
data RSEXP = RInt Int | RDouble Double | RString String | RSym String | RBool Bool 
  | RVector [RSEXP] 
  | RList RSEXP -- head, tag and tail
  | RClos RSEXP RSEXP -- formals, body
  | RArrayInt [Int]
  | RArrayDouble [Double]
  | RArrayString [String]
  | RArrayBool [Bool]
  | RRaw [Word8]
  | RArrayComplex [(Double, Double)]
  deriving (Show)

data Len24 = Len24 Word8 Word8 Word8
instance Binary Len24 where
  put (Len24 l1 l2 l3) = do putWord8 l1 >> putWord8 l2 >> putWord8 l3
  get = do l1 <- getWord8
           l2 <- getWord8
           l3 <- getWord8
           return (Len24 l1 l2 l3)


instance Binary RSEXP where
  put (RInt i) = do putWord8 xt_INT >> putWord8 0 >> putWord8 0 >> putWord8 0 >> putWord32le (fromIntegral i :: Word32)
  put (RArrayDouble arr) = do putWord8 xt_ARRAY_DOUBLE >> put len24  >> mapM_ putFloat64le arr
                              where len24 = to24bit (length arr)
  put (RString s) = do putWord8 xt_STR >> put len24 >> mapM_ put s >> putWord8 0
                    where len24 = to24bit (length s +1)
  put (RSym s) = do putWord8 xt_SYM >> put len24 >> mapM_ put s 
                    where len24 = to24bit (length s )
  put (RBool b) = do putWord8 xt_BOOL >> putWord8 0 >> putWord8 0 >> putWord8 0 >> putWord8 (if b then 1 else 0)

  put _ = error "unknown type in put Binary instance RSEXP"
  get = do t <- getWord8 
           case fromIntegral(t)::Int of 
             1  -> do sequence_ (replicate 3 getWord8) 
                      e <- get
                      return (RInt e)
             2  -> do sequence_ (replicate 3 getWord8) 
                      e <- get
                      return (RDouble e)
             3  -> do len24 <- get
                      let len = from24Bit len24 - 1
                      chars <- sequence (replicate len getWord8)
                      return (RString (map BI.w2c chars))
             5  -> do len24 <- get
                      let len = from24Bit len24
                      chars <- sequence (replicate len getWord8)
                      return (RSym (map BI.w2c chars))
             6  -> do sequence_ (replicate 3 getWord8)
                      b <- getWord8
                      return (RBool (b == 1))  -- 1=TRUE, 0=FALSE, 2=NA
             33 -> do len24 <- get
                      let len = (from24Bit len24) `div` 8
                      doubles <- sequence (replicate len getFloat64le)
                      return (RArrayDouble doubles)
             _  -> error ("unsuppported RSEXP type code:"++show (fromIntegral(t)::Int))
        
data RList a = RListNil | RListCons a a (RList a)

to24bit :: Int -> Len24
to24bit i = Len24 x y z 
  where x = fromIntegral(i .&. 0xff)::Word8
        y = fromIntegral((i `shiftR` 8) .&. 0xff)::Word8
        z = fromIntegral((i `shiftR` 16) .&. 0xff)::Word8

from24Bit :: Len24 -> Int
from24Bit (Len24 x y z) = (fromIntegral(x)::Int) + (fromIntegral(y)::Int) `shiftL` 8 + (fromIntegral(z)::Int) `shiftL` 16

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

rRepl :: IO()
rRepl = do
  conn <- rConnection "localhost" 6311
  rReplLoop conn
    
rReplLoop :: RConn -> IO()
rReplLoop conn = do
  cmd <- getLine
  result <- rEval conn cmd
  let rsexp = (decode result) :: DT
  putStrLn ("value:" ++ if BL.length result > 0 then show rsexp else "")
  rReplLoop conn
   
rEval :: RConn->String -> IO BL.ByteString
rEval (RConn h) cmd = do
  let msg = createMessage cmdEval cmd
  putStrLn("request:"++lazyByteStringToString (encode msg))
  BL.hPut h (encode msg)
  header <- BL.hGet h 16
  let rheader = (decode header) :: QAP1Header
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

