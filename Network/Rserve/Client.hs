module Network.Rserve.Client 
  (RSEXP(..)
--  , DT(..)
  , QAP1Message(..)
--  , QAP1Header
  , RConn
  , connect
  , eval
  , voidEval
  , login
  , shutdown
  , openFile
  , createFile
  , closeFile
  , removeFile
  --, Network.Rserve.Client.readFile
  , Network.Rserve.Client.writeFile
  , assign
  , unpack
  , unpackRArrayInt
  , unpackRArrayBool
  , unpackRArrayDouble
  , unpackRArrayComplex
  , unpackRArrayString
  , unpackRInt
  , unpackRBool
  , unpackRDouble
  , unpackRString
  , unpackRSym
  , unpackRVector
  , unpackRListTag
  , unpackRSEXPWithAttrib
  , unpackBytestream   
  , rRepl
  , errorList) where 

 -- TODO :
 -- Handle Error responses better
 -- handle authenticated connections to Rserve
 -- support LARGE 
 -- support readFile (if Rserve fixes their bug omitting DT_BYTESTREAM header)

import Network
import System.IO (hSetBuffering, hFlush, BufferMode(NoBuffering), stdout)
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.Binary
import Data.Bits
import qualified Data.ByteString.Internal as BI
import Network.Rserve.Constants
import Network.Rserve.Internal 

-- | Connect to Rserve server 
connect :: String      -- ^ server name, e.g. "localhost"
           ->Int       -- ^ port, e.g. 6311
           ->IO RConn
connect server port = do 
  h <- connectTo server (PortNumber (fromIntegral port))
  hSetBuffering h NoBuffering
  idString <- B.hGet h 12
  a <- B.hGetNonBlocking h 10000
  let (sig, rserveVersion, protocol, attributes) = parseIdString (B.append idString a)
  return RConn {rcHandle=h, rcRserveSig=sig, rcRserveVersion=rserveVersion, rcProtocol=protocol, rcAttributes=attributes}

eval :: RConn -> String -> IO QAP1Message
eval rconn = eval' cmdEval rconn . DTString 

voidEval :: RConn -> String -> IO ()
voidEval rconn cmd = do _ <- eval' cmdVoidEval rconn (DTString cmd)
                        return ()
login :: RConn -> String -> IO QAP1Message
login rconn = eval' cmdLogin rconn . DTString 

-- TODO can optionally provide an admin password with the shutdown call
shutdown :: RConn -> IO()
shutdown rconn = do 
  _ <- eval' cmdShutdown rconn (DTString "")
  return ()

openFile :: RConn -> String -> IO QAP1Message
openFile rconn = eval' cmdOpenFile rconn . DTString 

createFile :: RConn -> String -> IO QAP1Message
createFile rconn = eval' cmdCreateFile rconn . DTString 

closeFile :: RConn -> String -> IO QAP1Message
closeFile rconn = eval' cmdCloseFile rconn . DTString 

removeFile :: RConn -> String -> IO QAP1Message
removeFile rconn = eval' cmdRemoveFile rconn . DTString 

-- | read file, Rserve seems not to have ever fixed the bug whereby the response is omits the DT_BYTESTREAM header, so we have to write custom serialisation for this one function.
--readFile :: RConn -> String -> IO QAP1Message
--readFile rconn = eval' cmdReadFile rconn . DTString 

writeFile :: RConn -> B.ByteString -> IO QAP1Message
writeFile rconn = eval' cmdWriteFile rconn . DTBytestream . map BI.c2w . B.unpack 

assign :: RConn -> String -> RSEXP -> IO QAP1Message
assign rconn symbol = eval' cmdSetSexp rconn . DTAssign symbol 

unpack :: QAP1Message -> Maybe RSEXP
unpack m =  qap1Content m  >>= unpackDT

unpackBytestream :: QAP1Message -> Maybe [Word8]
unpackBytestream m = qap1Content m >>= unpackDTBytestream

unpackDTBytestream :: DT -> Maybe [Word8]
unpackDTBytestream (DTBytestream ws) = Just ws
unpackDTBytestream _ = Nothing

unpackDT :: DT -> Maybe RSEXP
unpackDT (DTSexp x) = Just x
unpackDT _          = Nothing

unpackRArrayInt :: RSEXP -> Maybe [Int]
unpackRArrayInt (RArrayInt is) = Just is
unpackRArrayInt _ = Nothing

unpackRArrayDouble ::RSEXP -> Maybe [Double]
unpackRArrayDouble (RArrayDouble ds) = Just ds
unpackRArrayDouble _ = Nothing

unpackRArrayComplex :: RSEXP -> Maybe [(Double, Double)]
unpackRArrayComplex (RArrayComplex cs) = Just cs
unpackRArrayComplex _ = Nothing

unpackRArrayString :: RSEXP -> Maybe [String]
unpackRArrayString (RArrayString ss) = Just ss
unpackRArrayString _ = Nothing

unpackRArrayBool :: RSEXP -> Maybe [Bool]
unpackRArrayBool (RArrayBool bs) = Just bs
unpackRArrayBool _ = Nothing

unpackRInt :: RSEXP -> Maybe Int
unpackRInt (RInt i) = Just i
unpackRInt _        = Nothing

unpackRDouble :: RSEXP -> Maybe Double
unpackRDouble (RDouble d) = Just d
unpackRDouble _ = Nothing

unpackRString :: RSEXP -> Maybe String
unpackRString (RString s) = Just s
unpackRString _ = Nothing

unpackRSym :: RSEXP -> Maybe String
unpackRSym (RSym s) = Just s
unpackRSym _ = Nothing

unpackRBool :: RSEXP -> Maybe Bool
unpackRBool (RBool b) = Just b
unpackRBool _ = Nothing

unpackRVector :: RSEXP -> Maybe [RSEXP]
unpackRVector (RVector v) = Just v
unpackRVector _ = Nothing

unpackRListTag :: RSEXP -> Maybe [(RSEXP, RSEXP)]
unpackRListTag (RListTag ls) = Just ls
unpackRListTag _ = Nothing

unpackRSEXPWithAttrib :: RSEXP -> Maybe (RSEXP, RSEXP)
unpackRSEXPWithAttrib (RSEXPWithAttrib attrib value)=Just (attrib, value)
unpackRSEXPWithAttrib _ = Nothing

eval' :: Word32 -> RConn -> DT -> IO QAP1Message
eval' rcmd rconn cmd = do
  let msg = createMessage rcmd (Just cmd)
  request rconn msg

request :: RConn -> QAP1Message -> IO QAP1Message
request rconn msg = do
  let msgContent = encode msg
--  putStrLn ("request:"++ show (lazyByteStringToString msgContent))
  BL.hPut h msgContent
  header <- BL.hGet h 16
  let rheader = decode header :: QAP1Header
--  putStrLn ("header:"++show (lazyByteStringToString header))
  let bodyLength = fromIntegral(headerLen rheader) :: Int
  body <- if bodyLength > 0 then BL.hGet h bodyLength else return (BL.pack "")
--  putStrLn ("bodylen:"++show bodyLength)
  let content = if bodyLength > 0 then Just (decode body :: DT) else Nothing
--  putStrLn ("body:"++show (lazyByteStringToString body))
  return QAP1Message {qap1Header = rheader, qap1Content = content }
  where h = rcHandle rconn

createMessage :: Word32 -> Maybe DT -> QAP1Message
createMessage cmdId content = QAP1Message (QAP1Header cmdId tlen 0 0) content
  where tlen = fromIntegral contentLength :: Word32
        contentLength = case content of 
                          Nothing -> 0
                          Just x -> BL.length (encode x)

rRepl :: IO()
rRepl = connect "localhost" 6311 >>= rReplLoop
    
rReplLoop :: RConn -> IO()
rReplLoop conn = do
  putStr ">"
  hFlush stdout
  cmd <- getLine
  response <- eval conn cmd
  if responseOK response then printContent response else print (errorList response)
  rReplLoop conn

responseOK :: QAP1Message -> Bool
responseOK h = headerCmd (qap1Header h) .&. respOK == respOK

errorList :: QAP1Message -> [(Word32, String)]
errorList h = filter (\(c,_) -> c == s) errorStats
  where s = cmdStat (headerCmd (qap1Header h))

printContent :: QAP1Message -> IO()
printContent m = 
  case qap1Content m of 
    Just x -> print x
    Nothing -> putStrLn "Nothing"

{--byteStringToString :: B.ByteString->String
byteStringToString  = show . map ord . B.unpack 
--}
 
