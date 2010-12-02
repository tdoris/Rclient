module Network.Rserve.Constants where

import Data.Binary
import Data.Bits

cmds :: [(String, Word32)]
cmds = [("login", cmdLogin), ("voidEval", cmdVoidEval), ("eval", cmdEval), ("shutdown", cmdShutdown), 
        ("openFile", cmdOpenFile), ("createFile", cmdCreateFile), ("closeFile", cmdCloseFile), 
        ("readFile", cmdReadFile), ("writeFile", cmdWriteFile), ("removeFile", cmdRemoveFile), ("setSexp", cmdSetSexp), ("assignSexp", cmdAssignSexp), 
        ("detachSession", cmdDetachSession), ("detachedVoidEval", cmdDetachedVoidEval), ("attachSession", cmdAttachSession)]

cmdLogin :: Word32
cmdLogin = 0x0001 
cmdVoidEval :: Word32
cmdVoidEval = 0x0002 
cmdEval :: Word32
cmdEval = 0x0003 
cmdShutdown :: Word32
cmdShutdown = 0x0004
cmdOpenFile :: Word32
cmdOpenFile = 0x0010
cmdCreateFile ::Word32
cmdCreateFile = 0x0011
cmdCloseFile :: Word32
cmdCloseFile = 0x0012
cmdReadFile :: Word32
cmdReadFile = 0x0013
cmdWriteFile :: Word32
cmdWriteFile = 0x0014
cmdRemoveFile ::Word32
cmdRemoveFile = 0x0015
cmdSetSexp :: Word32
cmdSetSexp = 0x0020
cmdAssignSexp :: Word32
cmdAssignSexp = 0x0021
cmdDetachSession :: Word32
cmdDetachSession = 0x0030
cmdDetachedVoidEval :: Word32
cmdDetachedVoidEval = 0x0031
cmdAttachSession :: Word32
cmdAttachSession = 0x0032
cmdResp :: Word32
cmdResp = 0x10000

cmdStat :: Word32 -> Word32
cmdStat x = (x `shiftR` 24) .&. 127

respOK :: Word32
respOK = cmdResp .|. 0x0001
respErr :: Word32
respErr = cmdResp .|. 0x0002

errAuthFailed     :: Word32
errAuthFailed      = 0x41
errConnBroken     :: Word32
errConnBroken      = 0x42
errInvCmd         :: Word32
errInvCmd          = 0x43
errInvPar         :: Word32
errInvPar          = 0x44
errRerror          :: Word32
errRerror           = 0x45
errIOerror         :: Word32
errIOerror          = 0x46
errNotOpen         :: Word32
errNotOpen          = 0x47
errAccessDenied    :: Word32
errAccessDenied     = 0x48
errUnsupportedCmd  :: Word32
errUnsupportedCmd   = 0x49
errDataOverflow   :: Word32
errDataOverflow    = 0x4b 
errObjectTooBig  :: Word32
errObjectTooBig   = 0x4c
errOutOfMem      :: Word32
errOutOfMem       = 0x4d
errCtrlClosed     :: Word32
errCtrlClosed      = 0x4e
errSessionBusy    :: Word32
errSessionBusy     = 0x50
errDetachFailed   :: Word32
errDetachFailed    = 0x51
errorStats :: [(Word32, String)]
errorStats = [(errAuthFailed,"errAuthFailed"), (errConnBroken,"errConnBroken"), (errInvCmd,"errInvCmd"), (errInvPar,"errInvPar"),
              (errRerror,"errRerror"), (errIOerror,"errIOerror"), (errNotOpen,"errNotOpen"), (errAccessDenied,"errAccessDenied"),
              (errUnsupportedCmd,"errUnsupportedCmd"), (errDataOverflow,"errDataOverflow"), (errObjectTooBig,"errObjectTooBig"),
              (errOutOfMem,"errOutOfMem"), (errCtrlClosed,"errCtrlClosed"), (errSessionBusy,"errSessionBusy"), (errDetachFailed,"errDetachFailed")]

dtInt       :: Word8
dtInt        = 1  
dtChar      :: Word8
dtChar       = 2  
dtDouble    :: Word8
dtDouble     = 3  
dtString    :: Word8
dtString     = 4  
dtBytestream:: Word8
dtBytestream = 5  
dtSexp      :: Word8
dtSexp       = 10 
dtArray     :: Word8
dtArray      = 11 
dtLarge     :: Word8
dtLarge      = 64 



xtNull:: Word8
xtNull = 0 
xtInt:: Word8
xtInt = 1 
xtDouble:: Word8
xtDouble = 2 
xtStr:: Word8
xtStr = 3 
xtLang:: Word8
xtLang = 4 
xtSym:: Word8
xtSym = 5 
xtBool:: Word8
xtBool = 6 
xtS4:: Word8
xtS4 = 7 
xtVector:: Word8
xtVector = 16 
xtList:: Word8
xtList = 17 
xtClos:: Word8
xtClos = 18 
xtSymName:: Word8
xtSymName = 19 
xtListNotag:: Word8
xtListNotag = 20 
xtListTag:: Word8
xtListTag = 21  
xtVectorExp:: Word8
xtVectorExp = 26 
xtVectorStr:: Word8
xtVectorStr = 27
xtArrayInt:: Word8
xtArrayInt = 32
xtArrayDouble:: Word8
xtArrayDouble = 33
xtArrayStr:: Word8
xtArrayStr = 34
xtArrayBool:: Word8
xtArrayBool = 36
xtArrayCplx:: Word8
xtArrayCplx = 38
xtHasAttr:: Word8
xtHasAttr = 128
xtUnknown:: Word8
xtUnknown = 48

