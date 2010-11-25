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
import Data.List
import Data.List.Split


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

padRstring :: String -> String
padRstring s = padded
  where padded = nullTerminated ++ replicate gapLen '\1'
        nullTerminated = s ++ "\0"
        r = rem (length nullTerminated) 4
        gapLen = if 4 - r == 0 then 0 else 4 - r 

depadRstring :: String -> String
depadRstring s = takeWhile (/= '\0') s

createMessage :: Word32 -> String -> QAP1Message
createMessage cmdId content = QAP1Message (QAP1Header cmdId tlen 0 0) (DTString padded)
  where tlen = fromIntegral(length padded + 4):: Word32
        padded = padRstring content

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
  put (DTInt i) = do putWord8 dt_int >> put (to24bit 0) >> putWord32le (fromIntegral i::Word32)
  put (DTChar c) = do putWord8 dt_char >> put (to24bit 0)>> putWord8 (BI.c2w c)
  put (DTDouble d) = do putWord8 dt_double >> put (to24bit 0) >> putFloat64le d
  put (DTString s) = do putWord8 dt_string >> put (to24bit 0) >> mapM_ put s 
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

data RSEXP = RInt Int 
  | RDouble Double 
  | RString String 
  | RSym String 
  | RBool Bool 
  | RVector [RSEXP] 
  | RList RSEXP -- head, tag and tail
  | RClos RSEXP RSEXP -- formals, body
  | RListTag [(RSEXP,RSEXP)]
  | RArrayInt [Int]
  | RArrayDouble [Double]
  | RArrayString [String]
  | RArrayBool [Bool]
  | RRaw [Word8]
  | RArrayComplex [(Double, Double)]
  | RSEXPWithAttrib RSEXP RSEXP
  deriving (Show)

getTypeCode :: RSEXP -> Word8
getTypeCode (RInt _)        = xt_INT
getTypeCode (RDouble _)     = xt_DOUBLE
getTypeCode (RString _)     = xt_STR
getTypeCode (RSym _)        = xt_SYM
getTypeCode (RBool _)       = xt_BOOL
getTypeCode (RVector _)     = xt_VECTOR
getTypeCode (RList _)       = xt_LIST
getTypeCode (RClos _ _)     = xt_CLOS
getTypeCode (RListTag _)    = xt_LIST
getTypeCode (RArrayInt _)   = xt_ARRAY_INT
getTypeCode (RArrayDouble _)= xt_ARRAY_DOUBLE
getTypeCode (RArrayString _)= xt_ARRAY_STR
getTypeCode (RArrayBool _)  = xt_ARRAY_BOOL
getTypeCode (RArrayComplex _) = xt_ARRAY_CPLX
getTypeCode (RRaw _)        = xt_RAW
 

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
xt_HAS_ATTR = 128 :: Word8

data Len24 = Len24 Word8 Word8 Word8
instance Binary Len24 where
  put (Len24 l1 l2 l3) = do putWord8 l1 >> putWord8 l2 >> putWord8 l3
  get = do l1 <- getWord8
           l2 <- getWord8
           l3 <- getWord8
           return (Len24 l1 l2 l3)

data RTypeCode = RType Word8 | RTypeAttr Word8 deriving Show

getCode :: Word8 -> RTypeCode 
getCode t = if hasAttribute then RTypeAttr code else RType code
  where hasAttribute = t .&. 128 == 128 
        isLarge = t .&. 64 == 64
        code =  t .&. 63

instance Binary RSEXP where
  put (RInt i) = do putWord8 xt_INT >> put (to24bit 0) >> putWord32le (fromIntegral i :: Word32)
  put (RDouble d) = do putWord8 xt_DOUBLE >> put (to24bit 0) >> putFloat64le d
  put (RBool b) = do putWord8 xt_BOOL >> put (to24bit 0) >> putWord8 (if b then 1 else 0)
  put (RString s) = do putWord8 xt_STR >> put len24 >> mapM_ (putWord8 . BI.c2w) ps
                    where len24 = to24bit (length ps)
                          ps = padRstring s
  put (RSym s) = do putWord8 xt_SYM >> put len24 >> mapM_ (putWord8 . BI.c2w) ps
                    where len24 = to24bit (length ps)
                          ps = padRstring s
  put (RArrayInt arr) = do putWord8 xt_ARRAY_INT >> put len24  >> mapM_ (putWord32le . fromIntegral) arr
                              where len24 = to24bit ((length arr)*4)
  put (RArrayDouble arr) = do putWord8 xt_ARRAY_DOUBLE >> put len24  >> mapM_ putFloat64le arr
                              where len24 = to24bit ((length arr)*8)
  put (RArrayBool arr) = do putWord8 xt_ARRAY_BOOL >> put len24  >> mapM_ (putWord8 . (\x -> if x then 1 else 0)) arr
                              where len24 = to24bit (length arr)
  put (RArrayString ss) = do putWord8 xt_ARRAY_STR >> put len24 >> mapM_ (putWord8 . BI.c2w) ss'
                             where len24 = to24bit (length ss')
                                   ss' = padRstring (intercalate "\0" ss)
  put (RListTag lt) = do putWord8 xt_LIST_TAG >> put len24 >> mapM_ put lt
                         where len24 = to24bit (sum (map encodedLength rsexps))
                               rsexps = detuple lt
  put (RClos e1 e2) = do putWord8 xt_CLOS >> put len24 >> put e1 >> put e2
                         where len24 = to24bit (encodedLength e1 + encodedLength e2 + 4)
  put (RList e) = do putWord8 xt_LIST >> put len24 >> put e
                     where len24 = to24bit (encodedLength e + 4)
  put (RVector v) = do putWord8 xt_VECTOR  >> put len24 >> mapM_ put v
                       where len24 = 4+sum (map encodedLength v) 
  put (RSEXPWithAttrib attrib val) = do putWord8 (fromIntegral code) >> put len24 >> put attrib >> mapM_ (putWord8 . BI.c2w) (BL.unpack (BL.drop 4 (encode val)))
                                   where code = getTypeCode val .|. xt_HAS_ATTR 
                                         len24 = (encodedLength attrib + encodedLength val)
   
  put _ = error "unknown type in put Binary instance RSEXP"
  get = do t <- getWord8 
           len24 <- get
           let len = from24Bit len24
           let typeCode = getCode t
           case typeCode of 
                 -- if we have an attribute attached, we have to parse the attribute and use the first type code to parse the RSEXP data 
                 (RTypeAttr x) -> do attrib <- get -- parse the attribute RSEXP
                                     let remainingLen = len - (fromIntegral(BL.length (encode attrib))::Int) 
                                     val <- getRType (RType x) remainingLen
                                     return (RSEXPWithAttrib attrib val)
                 (RType x) -> do getRType typeCode len

getRType :: RTypeCode -> Int -> Get RSEXP
getRType typeCode len = 
         do case typeCode of
                 (RType 1)  -> do e <- get
                                  return (RInt e)
                 (RType 2)  -> do e <- get
                                  return (RDouble e)
                 (RType 3)  -> do chars <- sequence (replicate (len-1) getWord8)
                                  return (RString (depadRstring (map BI.w2c chars)))
                 (RType 5)  -> do chars <- sequence (replicate len getWord8)
                                  return (RSym (depadRstring (map BI.w2c chars)))
                 (RType 6)  -> do b <- getWord8
                                  return (RBool (b == 1))  -- 1=TRUE, 0=FALSE, 2=NA
                 (RType 16) -> do rsexpsBytes <- sequence (replicate len getWord8)
                                  let rsexps = vectorRSEXPDecode rsexpsBytes
                                  return (RVector rsexps)
                 (RType 19) -> do chars <- sequence (replicate len getWord8)
                                  return (RSym (depadRstring (map BI.w2c chars)))
                 (RType 21) -> do listTagBytes <- sequence (replicate len getWord8)
                                  let taglist = listTagDecode listTagBytes
                                  return (RListTag taglist)
                 (RType 32) -> do ints <- sequence (replicate (len `div` 4) getWord32le)
                                  return (RArrayInt (map fromIntegral ints))
                 (RType 33) -> do doubles <- sequence (replicate (len `div` 8) getFloat64le)
                                  return (RArrayDouble doubles)
                 (RType 34) -> do stringsBytes <- sequence (replicate len getWord8)
                                  let strings = vectorStringDecode (map BI.w2c stringsBytes)
                                  return (RArrayString strings)
                 (RType 36) -> do boolCount <-getWord32le
                                  bools <- sequence (replicate ((fromIntegral boolCount)::Int) getWord8)
                                  return (RArrayBool (map (==1) bools))
                 _  -> error ("unsuppported RSEXP type code:"++show typeCode)
           
vectorRSEXPDecode :: [Word8] -> [RSEXP]
vectorRSEXPDecode [] = []
vectorRSEXPDecode ws = val : vectorRSEXPDecode (drop (encodedLength val) ws)
  where content = BL.pack (map BI.w2c ws)
        val = vectorRSEXPDecode1 content

encodedLength :: RSEXP -> Int
encodedLength s = fromIntegral(BL.length (encode s))::Int

vectorRSEXPDecode1 :: BL.ByteString -> RSEXP
vectorRSEXPDecode1 content = decode content
 
vectorStringDecode :: String -> [String]
vectorStringDecode s = filter (/="") $ map (dropWhile (=='\1')) (splitOn "\0" s)

pairs :: [a] -> [(a,a)]
pairs [] = []
pairs [x] = []
pairs (x:y:xs) = (x,y) : pairs xs

detuple :: [(a,a)] -> [a]
detuple [] =[]
detuple ((x,y):xs) = x:y: detuple xs

listTagDecode :: [Word8] -> [(RSEXP,RSEXP)]
listTagDecode [] = []
listTagDecode ws = pairs $ vectorRSEXPDecode ws

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

