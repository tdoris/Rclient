{-# LANGUAGE FlexibleInstances,TypeSynonymInstances #-}

module Network.Rserve.Client 
  (RSEXP(..)
  , RConn(..)
  , Result
  , RserveError
  , ResultUnpack(..)
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
  , rRepl
  ) where 

 -- TODO :
 -- handle authenticated connections to Rserve
 -- support LARGE 
 -- support readFile (if Rserve fixes their bug omitting DT_BYTESTREAM header, and/or someone asks for readFile)

import Network
import System.IO (hSetBuffering, hFlush, BufferMode(NoBuffering), stdout)
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.Binary
import Data.Bits
import qualified Data.ByteString.Internal as BI
import Network.Rserve.Constants
import Network.Rserve.Internal 

type RserveError = String

type Result = Either RserveError (Maybe RSEXP)

-- | Connect to Rserve server 
connect :: String      -- ^ server name, e.g. "localhost"
           ->Int       -- ^ port, e.g. 6311
           ->IO RConn  -- ^ the connection
connect server port = do 
  h <- connectTo server (PortNumber (fromIntegral port))
  hSetBuffering h NoBuffering
  idString <- B.hGet h 12
  a <- B.hGetNonBlocking h 10000
  let (sig, rserveVersion, protocol, attributes) = parseIdString (B.append idString a)
  return RConn {rcHandle=h, rcRserveSig=sig, rcRserveVersion=rserveVersion, rcProtocol=protocol, rcAttributes=attributes}

-- | evaluate an R expression
eval :: RConn -> String -> IO Result
eval rconn = eval' cmdEval rconn . DTString 

-- | evaluate an R expression, discarding any result
voidEval :: RConn -> String -> IO ()
voidEval rconn cmd = do _ <- eval' cmdVoidEval rconn (DTString cmd)
                        return ()

-- | login to Rserve, not normally required, for authenticated sessions only
login :: RConn 
      -> String  -- ^ user name
      -> String  -- ^ password 
      -> IO Result
login rconn name pwd = eval' cmdLogin rconn (DTString (name ++ "\n" ++ pwd))

-- | shutdown the Rserve server
shutdown :: RConn -> IO()
shutdown rconn = do 
  _ <- eval' cmdShutdown rconn (DTString "")
  return ()

-- | open a file
openFile :: RConn -> String -> IO Result
openFile rconn = eval' cmdOpenFile rconn . DTString 

-- | write content to a file accessible to the Rserve session
writeFile :: RConn -> B.ByteString -> IO Result
writeFile rconn = eval' cmdWriteFile rconn . DTBytestream . map BI.c2w . B.unpack 

-- | create a file
createFile :: RConn -> String -> IO Result
createFile rconn = eval' cmdCreateFile rconn . DTString 

-- | close a file
closeFile :: RConn -> String -> IO Result
closeFile rconn = eval' cmdCloseFile rconn . DTString 

-- | remove a file
removeFile :: RConn -> String -> IO Result
removeFile rconn = eval' cmdRemoveFile rconn . DTString 

-- read file, Rserve seems not to have ever fixed the bug whereby the response is omits the DT_BYTESTREAM header, so we have to write custom serialisation for this one function.
-- nobody's expressed a need for it, so leaving it out for now
--readFile :: RConn -> String -> IO QAP1Message
--readFile rconn = undefined

-- | assign a RSEXP value to a symbol
assign :: RConn -> String -> RSEXP -> IO Result
assign rconn symbol = eval' cmdSetSexp rconn . DTAssign symbol 

-- | The ResultUnpack instances are used to extract R data structures from the Result container
class ResultUnpack a where
  unpack :: Result -> Maybe a
-- | unpack RInt
instance ResultUnpack Int where 
  unpack (Right (Just (RInt x))) = Just x
  unpack _ = Nothing

-- | unpack RDouble
instance ResultUnpack Double where
  unpack (Right (Just (RDouble x))) = Just x
  unpack _           = Nothing 

-- | unpack RString, RSym
instance ResultUnpack String where
  unpack (Right (Just (RString x))) = Just x
  unpack (Right (Just (RSym x)))    = Just x
  unpack _           = Nothing

-- | unpack RBool
instance ResultUnpack Bool where
  unpack (Right (Just (RBool x))) = Just x
  unpack _         = Nothing

-- | unpack RArrayInt
instance ResultUnpack [Int] where
  unpack (Right (Just (RArrayInt x))) = Just x
  unpack _ = Nothing

-- | unpack RArrayDouble
instance ResultUnpack [Double] where
  unpack (Right (Just (RArrayDouble x))) = Just x
  unpack _ = Nothing

-- | unpack RArrayString
instance ResultUnpack [String] where
  unpack (Right (Just (RArrayString x))) = Just x
  unpack _ = Nothing

-- | unpack RArrayBool
instance ResultUnpack [Bool] where
  unpack (Right (Just (RArrayBool x))) = Just x
  unpack _ = Nothing

-- | unpack RArrayComplex
instance ResultUnpack [(Double, Double)] where
  unpack (Right (Just (RArrayComplex x))) = Just x
  unpack _ = Nothing

-- | unpack RSEXPWithAttrib
instance ResultUnpack (RSEXP, RSEXP) where
  unpack (Right (Just (RSEXPWithAttrib a v))) = Just (a,v)
  unpack _ = Nothing
-- | unpack RVector
instance ResultUnpack [RSEXP] where
  unpack (Right (Just (RVector v))) = Just v
  unpack _ = Nothing

-- | unpack RListTag
instance ResultUnpack [(RSEXP, RSEXP)] where
  unpack (Right (Just (RListTag l))) = Just l
  unpack _ = Nothing


unpackDT :: DT -> Maybe RSEXP
unpackDT (DTSexp x) = Just x
unpackDT _          = Nothing

-- | unpack a Result containing an RArrayInt
unpackRArrayInt :: Result -> Maybe [Int]
unpackRArrayInt (Right (Just (RArrayInt is))) = Just is
unpackRArrayInt _ = Nothing

-- | unpack a Result containing an RArrayDouble
unpackRArrayDouble :: Result -> Maybe [Double]
unpackRArrayDouble (Right (Just (RArrayDouble ds))) = Just ds
unpackRArrayDouble _ = Nothing

-- | unpack a Result containing an RArrayComplex
unpackRArrayComplex :: Result -> Maybe [(Double, Double)]
unpackRArrayComplex (Right (Just (RArrayComplex cs))) = Just cs
unpackRArrayComplex _ = Nothing

-- | unpack a Result containing an RArrayString
unpackRArrayString :: Result -> Maybe [String]
unpackRArrayString (Right (Just (RArrayString ss))) = Just ss
unpackRArrayString _ = Nothing

-- | unpack a Result containing an RArrayBool
unpackRArrayBool :: Result -> Maybe [Bool]
unpackRArrayBool (Right (Just (RArrayBool bs))) = Just bs
unpackRArrayBool _ = Nothing

-- | unpack a Result containing an RInt
unpackRInt :: Result -> Maybe Int
unpackRInt (Right (Just (RInt i))) = Just i
unpackRInt _        = Nothing

-- | unpack a Result containing an RDouble
unpackRDouble :: Result -> Maybe Double
unpackRDouble (Right (Just (RDouble d))) = Just d
unpackRDouble _ = Nothing

-- | unpack a Result containing an RString
unpackRString :: Result -> Maybe String
unpackRString (Right (Just (RString s))) = Just s
unpackRString _ = Nothing

-- | unpack a Result containing an RSym
unpackRSym :: Result -> Maybe String
unpackRSym (Right (Just (RSym s))) = Just s
unpackRSym _ = Nothing

-- | unpack a Result containing an RBool
unpackRBool :: Result -> Maybe Bool
unpackRBool (Right (Just (RBool b))) = Just b
unpackRBool _ = Nothing

-- | unpack a Result containing an RVector
unpackRVector :: Result -> Maybe [RSEXP]
unpackRVector (Right (Just (RVector v))) = Just v
unpackRVector _ = Nothing

-- | unpack a Result containing an RListTag
unpackRListTag :: Result -> Maybe [(RSEXP, RSEXP)]
unpackRListTag (Right (Just (RListTag ls))) = Just ls
unpackRListTag _ = Nothing

-- | unpack a Result containing an RSEXPWithAttrib
unpackRSEXPWithAttrib :: Result -> Maybe (RSEXP, RSEXP)
unpackRSEXPWithAttrib (Right (Just (RSEXPWithAttrib attrib value)))=Just (attrib, value)
unpackRSEXPWithAttrib _ = Nothing

eval' :: Word32 -> RConn -> DT -> IO Result 
eval' rcmd rconn cmd = do
  let msg = createMessage rcmd (Just cmd)
  response <- request rconn msg
  if responseOK response then return (Right (qap1Content response  >>= unpackDT))
                         else return (Left (getError response))

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

-- | Read-evaluate-print-loop for interacting with Rserve session. 
--   in ghci, load this module and run this command to test and play with Rserve
rRepl :: IO()
rRepl = connect "localhost" 6311 >>= rReplLoop
    
rReplLoop :: RConn -> IO()
rReplLoop conn = do
  putStr ">"
  hFlush stdout
  cmd <- getLine
  response <- eval conn cmd
  case response of 
    Right x -> print x
    Left x -> print ("Error:"++show x)
  rReplLoop conn

responseOK :: QAP1Message -> Bool
responseOK h = headerCmd (qap1Header h) .&. respOK == respOK

getError :: QAP1Message -> String
getError h = if errmatch == [] then (show h) ++ " cmd stat:"++ (show s) else snd (head errmatch)
  where s = cmdStat (headerCmd (qap1Header h))
        errmatch = filter (\(c,_) -> c == s) errorStats

