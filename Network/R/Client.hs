module Network.R.Client 
  (connect
  , eval
  , voidEval
  , login
  , shutdown
  , openFile
  , createFile
  , closeFile
  , removeFile
  , Network.R.Client.readFile
  , Network.R.Client.writeFile
  , rRepl) where 

 -- TODO :
 -- Handle Error responses
 -- implement all commands supported by Rserve
 -- figure out automatic marshalling to native types
 -- organise code into files
 -- handle authenticated connections to Rserve

import Network
import System.IO (hSetBuffering, hFlush, BufferMode(..), stdout)
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.Binary
import Data.Bits
import qualified Data.ByteString.Internal as BI
import Network.R.Constants
import Network.R.Internal 

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
  return (RConn {rcHandle=h, rcRserveSig=sig, rcRserveVersion=rserveVersion, rcProtocol=protocol, rcAttributes=attributes})

eval :: RConn -> String -> IO QAP1Message
eval rconn cmd = eval' cmdEval rconn (DTString cmd)

voidEval :: RConn -> String -> IO ()
voidEval rconn cmd = do _ <- eval' cmdVoidEval rconn (DTString cmd)
                        return ()

login :: RConn -> String -> IO QAP1Message
login rconn params = eval' cmdLogin rconn (DTString params)

-- TODO can optionally provide an admin password with the shutdown call
shutdown :: RConn -> IO()
shutdown rconn = do 
  _ <- eval' cmdShutdown rconn (DTString "")
  return ()

openFile :: RConn -> String -> IO QAP1Message
openFile rconn fileName = eval' cmdOpenFile rconn (DTString fileName)

createFile :: RConn -> String -> IO QAP1Message
createFile rconn fileName = eval' cmdCreateFile rconn (DTString fileName)

closeFile :: RConn -> String -> IO QAP1Message
closeFile rconn fileName = eval' cmdCloseFile rconn (DTString fileName)

removeFile :: RConn -> String -> IO QAP1Message
removeFile rconn fileName = eval' cmdRemoveFile rconn (DTString fileName)

readFile :: RConn -> String -> IO QAP1Message
readFile rconn fileName = eval' cmdReadFile rconn (DTString fileName)

writeFile :: RConn -> B.ByteString -> IO QAP1Message
writeFile rconn fileData = eval' cmdWriteFile rconn (DTBytestream (map BI.c2w (B.unpack fileData)))

eval' :: Word32 -> RConn -> DT -> IO (QAP1Message)
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
  return (QAP1Message {qap1Header = rheader, qap1Content = content })
  where h = rcHandle rconn

createMessage :: Word32 -> Maybe DT -> QAP1Message
createMessage cmdId content = QAP1Message (QAP1Header cmdId tlen 0 0) (content)
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
printContent m = do
  case qap1Content m of 
    Just x -> print x
    Nothing -> putStrLn "Nothing"

{--byteStringToString :: B.ByteString->String
byteStringToString  = show . map ord . B.unpack 
 
lazyByteStringToString :: BL.ByteString->String
lazyByteStringToString = show . map ord . BL.unpack 
--}
