module Network.Rserve.Internal where

import System.IO
import Data.Bits
import Data.Binary
import Data.Binary.Put
import Data.Binary.Get
import Data.Binary.IEEE754
import Data.Char
import Data.List
import Data.List.Split
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.ByteString.Internal as BI
import Control.Monad
import Network.Rserve.Constants

data Len24 = Len24 Word8 Word8 Word8 deriving Show

instance Binary Len24 where
  put (Len24 l1 l2 l3) = putWord8 l1 >> putWord8 l2 >> putWord8 l3
  get = do l1 <- getWord8
           l2 <- getWord8
           l3 <- getWord8
           return (Len24 l1 l2 l3)

data QAP1Header = QAP1Header { headerCmd::Word32, headerLen ::Word32, headerDof::Word32, headerRes::Word32} deriving Show

data QAP1Message = QAP1Message { qap1Header :: QAP1Header, qap1Content :: Maybe DT } deriving Show

instance Binary QAP1Header where
  put r = putWord32le (headerCmd r) >> putWord32le (headerLen r) >> putWord32le (headerDof r) >> putWord32le (headerRes r)
  get = do c <- getWord32le
           l <- getWord32le
           d <- getWord32le
           r <- getWord32le
           return (QAP1Header c l d r)

instance Binary QAP1Message where
  put m = case qap1Content m of 
               Just dt -> put (qap1Header m) >> put dt
               Nothing -> put (qap1Header m)

  get = do header <- get
           content <- get
           return (QAP1Message header content)


data DT = DTInt Int | DTChar Char | DTDouble Double | DTString String | DTBytestream [Word8] | DTSexp RSEXP | DTAssign String RSEXP
  deriving (Show)

instance Binary DT where
  put (DTInt i) = putWord8 dtInt >> put (to24bit 0) >> putWord32le (fromIntegral i::Word32)
  put (DTChar c) = putWord8 dtChar >> put (to24bit 0)>> putWord8 (BI.c2w c)
  put (DTDouble d) = putWord8 dtDouble >> put (to24bit 0) >> putFloat64le d
  put (DTString s) = putWord8 dtString >> put len24 >> mapM_ put paddedString
                        where len24 = to24bit (length paddedString)
                              paddedString = padRstring s 
  put (DTBytestream s)= putWord8 dtBytestream >> put len24 >> mapM_ putWord8 s
                          where len24 = to24bit (length s)
  put (DTSexp s) = putWord8 dtSexp >> put len24 >> put s
                     where len24 = to24bit (fromIntegral (BL.length (encode s))::Int)
  put (DTAssign symbol sexp) = put (DTString symbol) >> put (DTSexp sexp)
  get = do t <- getWord8 
           case fromIntegral t::Int of
              1 -> liftM DTInt get
              2 -> liftM DTChar get
              3 -> liftM DTDouble get
              4 -> do len24 <- get
                      let len = (from24Bit len24) 
                      chars <- replicateM len getWord8
                      return (DTString (map BI.w2c chars))
              5 -> do len24 <- get
                      let len = (from24Bit len24)
                      ws <- replicateM len getWord8
                      return (DTBytestream ws)
              10 -> do replicateM_ 3 getWord8
                       liftM DTSexp get
              x -> error ("Unsupported DT id:"++show x)

data RConn = RConn {rcHandle::Handle, rcRserveSig:: String, rcRserveVersion::String, rcProtocol::String, rcAttributes::[String]} deriving (Show)

data RSEXP = 
   RNULL
  | RInt Int 
  | RDouble Double 
  | RString String 
  | RSym String 
  | RBool Bool 
  | RVector [RSEXP] 
--  | RList RSEXP RSEXP RSEXP -- head vals tag -- I can't find any evidence that this is ever used in Rserve
  | RClos Word32  -- just store the length 
  | RListTag [(RSEXP,RSEXP)]
  | RArrayInt [Int]
  | RArrayDouble [Double]
  | RArrayString [String]
  | RArrayBool [Bool]
  | RArrayComplex [(Double, Double)]
  | RSEXPWithAttrib RSEXP RSEXP
  | RUnknown Word32
  deriving (Show,Eq)

getTypeCode :: RSEXP -> Word8
getTypeCode (RNULL)       = xtNull
getTypeCode (RInt _)        = xtInt
getTypeCode (RDouble _)     = xtDouble
getTypeCode (RString _)     = xtStr
getTypeCode (RSym _)        = xtSym
getTypeCode (RBool _)       = xtBool
getTypeCode (RVector _)     = xtVector
getTypeCode (RClos _)     = xtClos
getTypeCode (RListTag _)    = xtListTag
getTypeCode (RArrayInt _)   = xtArrayInt
getTypeCode (RArrayDouble _)= xtArrayDouble
getTypeCode (RArrayString _)= xtArrayStr
getTypeCode (RArrayBool _)  = xtArrayBool
getTypeCode (RArrayComplex _) = xtArrayCplx
getTypeCode (RSEXPWithAttrib _ v) = xtHasAttr .|. getTypeCode v
getTypeCode (RUnknown _) = xtUnknown 

data RTypeCode = RType Word8 | RTypeAttr Word8 deriving Show

to24bit :: Int -> Len24
to24bit i = Len24 x y z 
  where x = fromIntegral(i .&. 0xff)::Word8
        y = fromIntegral((i `shiftR` 8) .&. 0xff)::Word8
        z = fromIntegral((i `shiftR` 16) .&. 0xff)::Word8

from24Bit :: Len24 -> Int
from24Bit (Len24 x y z) = (fromIntegral x::Int) + (fromIntegral y ::Int) `shiftL` 8 + (fromIntegral z ::Int) `shiftL` 16

-- TODO add support for large 
getCode :: Word8 -> RTypeCode 
getCode t | isLarge = error "LARGE" 
          | hasAttribute = RTypeAttr code
          | otherwise = RType code
  where hasAttribute = t .&. 128 == 128 
        isLarge = t .&. 64 == 64
        code =  t .&. 63

instance Binary RSEXP where
  put (RNULL) = putWord8 xtNull >> put (to24bit 0) 
  put (RInt i) = putWord8 xtInt >> put (to24bit 4) >> putWord32le (fromIntegral i :: Word32)
  put (RDouble d) = putWord8 xtDouble >> put (to24bit 8) >> putFloat64le d
  put (RBool b) = putWord8 xtBool >> put (to24bit 4) >> putWord8 (if b then 1 else 0) >> replicateM_ 3 (putWord8 0)
  put (RString s) = putWord8 xtStr >> put len24 >> mapM_ (putWord8 . BI.c2w) ps
                    where len24 = to24bit (length ps)
                          ps = padRstring s
  put (RSym s) = putWord8 xtSym >> put len24 >> mapM_ (putWord8 . BI.c2w) ps
                    where len24 = to24bit (length ps)
                          ps = padRstring s
  put (RArrayInt arr) = putWord8 xtArrayInt >> put len24  >> mapM_ (putWord32le . fromIntegral) arr
                              where len24 = to24bit (4* length arr)
  put (RArrayDouble arr) = putWord8 xtArrayDouble >> put len24  >> mapM_ putFloat64le arr
                              where len24 = to24bit (length arr *8)
  put (RArrayBool arr) = putWord8 xtArrayBool >> put len24  >> putWord32le (fromIntegral (length arr) :: Word32) >> mapM_ (putWord8 . (\x -> if x then 1 else 0)) arr >> replicateM_ (gapLen (length arr)) (putWord8 255)
                              where len24 = to24bit (4 + length arr + gapLen (length arr))
                                    
  put (RArrayString ss) = putWord8 xtArrayStr >> put len24 >> mapM_ (putWord8 . BI.c2w) ss'
                             where len24 = to24bit (length ss')
                                   ss' = if length ss > 0  then padRstring (intercalate "\0" ss) else [] 
  put (RListTag lt) = putWord8 xtListTag >> put len24 >> mapM_ put lt
                         where len24 = to24bit (sum (map encodedLength rsexps))
                               rsexps = detuple lt
  put (RClos v) = putWord8 xtClos >> put (to24bit 4) >> putWord32le v 
--  put (RList h vals tag) = putWord8 xtList >> put len24 >> put h >> put vals >> put tag
--                     where len24 = to24bit (encodedLength h + encodedLength vals + encodedLength tag)
  put (RVector v) = putWord8 xtVector  >> put len24 >> mapM_ put v
                       where len24 = to24bit (sum (map encodedLength v))
-- The way Rserve serialises this isn't great, the val header is munged into the header for the entire object and the id bitwised ored with 128 
-- which also means it's impossible to serialise an RSEXPWithAttrib which has an RSEXPWithAttrib as a value
  put (RSEXPWithAttrib attrib val) = putWord8 (fromIntegral code) >> put len24 >> put attrib >> mapM_ (putWord8 . BI.c2w) (BL.unpack (BL.drop 4 (encode val)))
                                   where code = getTypeCode val .|. xtHasAttr 
                                         len24 = to24bit (encodedLength attrib + encodedLength val - 4) 
  put (RArrayComplex cs) = putWord8 xtArrayCplx >> put len24 >> mapM_ putFloat64le (detuple cs)
                           where len24 = to24bit (16 * length cs)
  put (RUnknown v) = putWord8 xtUnknown >> put (to24bit 4) >> putWord32le v
  get = do t <- getWord8 
           len24 <- get
           let len = from24Bit len24
           let typeCode = getCode t
           case typeCode of 
                 (RTypeAttr x) -> do attribType <- getWord8
                                     attribLen24 <- get
                                     let lenAttrib = from24Bit attribLen24
                                     let attribTypeCode = getCode attribType
                                     attrib <- getRType attribTypeCode lenAttrib
                                     let remainingLen = len - lenAttrib - 4
                                     val <- getRType (RType x) remainingLen
                                     return (RSEXPWithAttrib attrib val)
                 (RType _) -> getRType typeCode len

getRType :: RTypeCode -> Int -> Get RSEXP
getRType typeCode len = 
          case typeCode of
                 (RType 0)  -> return RNULL
                 (RType 1)  -> do e <- getWord32le
                                  return (RInt (fromIntegral e))
                 (RType 2)  -> do e <- getFloat64le
                                  return (RDouble e)
                 (RType 3)  -> do chars <- replicateM len getWord8
                                  return (RString (depadRstring (map BI.w2c chars)))
                 (RType 5)  -> do chars <- replicateM len getWord8
                                  return (RSym (depadRstring (map BI.w2c chars)))
                 (RType 6)  -> do b <- getWord32le -- this may be a byte in the spec but it's always padded
                                  return (RBool (b == 1))  -- 1=TRUE, 0=FALSE, 2=NA
                 (RType 16) -> getVector len
                 (RType 18) -> do v <- getWord32le
                                  return (RClos v )
                 (RType 19) -> do chars <- replicateM len getWord8
                                  return (RSym (depadRstring (map BI.w2c chars)))
                 (RType 21) -> getListTag len
                 (RType 22) -> getVector len
                 (RType 23) -> getListTag len
                 (RType 26) -> getVector len
                 (RType 32) -> do ints <- replicateM (len `div` 4) getWord32le
                                  return (RArrayInt (map fromIntegral ints))
                 (RType 33) -> do doubles <- replicateM (len `div` 8) getFloat64le
                                  return (RArrayDouble doubles)
                 (RType 34) -> do stringsBytes <- replicateM len getWord8
                                  let strings = vectorStringDecode (map BI.w2c stringsBytes)
                                  return (RArrayString strings)
                 (RType 36) -> do boolCount <-getWord32le
                                  bools <- replicateM (fromIntegral boolCount::Int) getWord8
                                  replicateM_ (len - 4 - fromIntegral boolCount::Int) getWord8
                                  return (RArrayBool (map (==1) bools))
                 (RType 38) -> do doubles <- replicateM (len `div` 8) getFloat64le
                                  return (RArrayComplex (pairs doubles))
                 (RType 48) -> do v <- getWord32le
                                  return (RUnknown v)
                 (RType x) ->  do br <- bytesRead
                                  error ("unsuppported type code:" ++ show x ++ " bytes read:"++show br)
                 _  -> error ("unsuppported RSEXP type code:"++show typeCode)

getVector :: Int -> Get RSEXP
getVector len = do rsexpsBytes <- replicateM len getWord8
                   let rsexps = vectorRSEXPDecode rsexpsBytes
                   return (RVector rsexps)
                                                                     
getListTag :: Int -> Get RSEXP
getListTag len = do listTagBytes <- replicateM len getWord8
                    let taglist = listTagDecode listTagBytes
                    return (RListTag taglist)
                                                                     
vectorRSEXPDecode :: [Word8] -> [RSEXP]
vectorRSEXPDecode [] = []
vectorRSEXPDecode ws@(_:l1:l2:l3:rws) = if encodingOK then val : vectorRSEXPDecode (drop (from24Bit (Len24 l1 l2 l3)) rws) 
                                                      else error ("Encoding error in val:"++show val)
  where content = BL.pack (map BI.w2c ws)
        val = decode content
        encodingOK = encodedLength val == from24Bit (Len24 l1 l2 l3) + 4
vectorRSEXPDecode _ = error "vectorRSEXPDecode: trailing bytes"

encodedLength :: RSEXP -> Int
encodedLength s = if l `mod` 4 == 0 then l else error "Encoded length not a multiple of 4"
  where l = fromIntegral(BL.length (encode s))::Int

vectorStringDecode :: String -> [String]
vectorStringDecode = init . splitOn "\0" 

padRstring :: String -> String
padRstring s = padded
  where padded = nullTerminated ++ replicate (gapLen r) '\1'
        nullTerminated = s ++ "\0"
        r = length nullTerminated

gapLen :: Int -> Int
gapLen r = if gap == 0 then 0 else 4 - gap 
  where gap = rem r 4

depadRstring :: String -> String
depadRstring = takeWhile (/= '\0') 

pairs :: [a] -> [(a,a)]
pairs [] = []
pairs [_] = []
pairs (x:y:xs) = (x,y) : pairs xs

detuple :: [(a,a)] -> [a]
detuple [] =[]
detuple ((x,y):xs) = x:y: detuple xs

listTagDecode :: [Word8] -> [(RSEXP,RSEXP)]
listTagDecode [] = []
listTagDecode ws = pairs $ vectorRSEXPDecode ws

parseIdString :: B.ByteString -> (String, String, String, [String])
parseIdString b = (sig, h, r, attributes)
  where [sig,h,r] = map B.unpack [(B.take 4 b), (B.take 4 (B.drop 4 b)), (B.take 4 (B.drop 8 b))]
        attributes  = parseAttributes (B.drop 12 b)

parseAttributes :: B.ByteString -> [String]
parseAttributes = filter (=="") . map B.unpack . B.lines 

lazyByteStringToString :: BL.ByteString->String
lazyByteStringToString = show . map ord . BL.unpack 

