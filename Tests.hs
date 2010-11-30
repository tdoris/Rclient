import Network.Rserve.Client
import Test.QuickCheck
import Text.Printf
import Data.Maybe
import Data.List
import Data.Binary
import qualified Data.ByteString.Lazy.Char8 as BL
import Control.Monad

normalCharacters = ['A'..'Z'] ++ ['a' .. 'z'] ++ " ~!@#$%^&*()"

--TODO arbitrary RSEXPs are likely to be enormous, limit nesting depth somehow
instance Arbitrary RSEXP where
  arbitrary = oneof [ return RNULL
                    , liftM RInt arbitrary
                    , liftM RDouble arbitrary
                    , liftM RString arbitraryNonemptyString
                    , liftM RSym arbitraryNonemptyString
                    , liftM RBool arbitrary
                    , liftM RVector (listOf primitiveRSEXP)
                    , liftM RClos arbitrary
                    , liftM RListTag primitiveTagList -- TODO this can generate large recursive structures
                    , liftM RArrayInt arbitrary
                    , liftM RArrayDouble arbitrary
                    , liftM RArrayString (listOf arbitraryNonemptyString)
                    , liftM RArrayBool arbitrary
                    , liftM RArrayComplex arbitrary
                    , liftM2 RSEXPWithAttrib arbitrary nonRSEXPWithAttrib -- TODO, can't have a val that is an RSEXPWithAttrib limit recursion here
                    , liftM RUnknown arbitrary
                    , liftM RString arbitraryNonemptyString 
                    ]

arbitraryNonemptyString :: Gen String
arbitraryNonemptyString = do 
  x <- arbitrary
  let x' = filter (\c -> c `elem` normalCharacters) x
  let s = if x' == "" then "empt" else x'
  return s

nonRSEXPWithAttrib :: Gen RSEXP
nonRSEXPWithAttrib = do
              oneof [ return RNULL
                    , liftM RInt arbitrary
                    , liftM RDouble arbitrary
                    , liftM RString arbitraryNonemptyString
                    , liftM RSym arbitraryNonemptyString
                    , liftM RBool arbitrary
                    , liftM RVector arbitrary
                    , liftM RClos arbitrary
                    , liftM RListTag arbitrary  -- TODO this can generate large recursive structures
                    , liftM RArrayInt arbitrary
                    , liftM RArrayDouble arbitrary
                    , liftM RArrayString (listOf arbitraryNonemptyString)
                    , liftM RArrayBool arbitrary
                    , liftM RArrayComplex arbitrary
                    , liftM RUnknown arbitrary
                    , liftM RString arbitraryNonemptyString 
                    ]
primitiveTagList :: Gen [(RSEXP, RSEXP)]
primitiveTagList = do liftM2 zip (listOf primitiveRSEXP) (listOf primitiveRSEXP)

primitiveRSEXP :: Gen RSEXP
primitiveRSEXP = do
              oneof [ return RNULL
                    , liftM RInt arbitrary
                    , liftM RDouble arbitrary
                    , liftM RString arbitraryNonemptyString
                    , liftM RSym arbitraryNonemptyString
                    , liftM RBool arbitrary
                    , liftM RArrayInt arbitrary
                    , liftM RArrayDouble arbitrary
                    , liftM RArrayString (listOf arbitraryNonemptyString)
                    , liftM RArrayBool arbitrary
                    , liftM RArrayComplex arbitrary
                    , liftM RUnknown arbitrary
                    , liftM RString arbitraryNonemptyString 
                    ]
propEncodeDecodeIdentity :: RSEXP -> Bool
propEncodeDecodeIdentity r = (decode (encode r)) == r

propEncodedLength ::RSEXP -> Bool
propEncodedLength r = BL.length (encode r) `mod` 4 == 0

tests = [("encodeDecodeIdentity", quickCheck propEncodeDecodeIdentity),
         ("encodedLength", quickCheck propEncodedLength)
        ]

main = do --sample arbitraryNonemptyString
          mapM_ (\(s,a) -> printf "%-25s: " s >> a) tests

