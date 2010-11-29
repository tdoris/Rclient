import Rclient
import Test.QuickCheck
import Text.Printf
import Data.Maybe
import Data.List
import Data.Binary
import qualified Data.ByteString.Lazy.Char8 as BL
import Control.Monad

instance Arbitrary RSEXP where
  arbitrary = oneof [ return RNULL
                    , liftM RInt arbitrary
                    , liftM RDouble arbitrary
                    , liftM RString arbitrary
                    , liftM RSym arbitrary
                    , liftM RBool arbitrary
                    , liftM RVector arbitrary
                    , liftM3 RList arbitrary arbitrary arbitrary
                    , liftM RClos arbitrary
                    , liftM RListTag arbitrary
                    , liftM RArrayInt arbitrary
                    , liftM RArrayDouble arbitrary
                    , liftM RArrayString arbitrary
                    , liftM RArrayBool arbitrary
                    , liftM RArrayComplex arbitrary
                    , liftM2 RSEXPWithAttrib arbitrary arbitrary
                    , liftM RUnknown arbitrary
                    ]

--TODO arbitrary RSEXPs are likely to be enormous, limit nesting depth somehow
propEncodeDecodeIdentity :: RSEXP -> Bool
propEncodeDecodeIdentity r = (decode (encode r)) == r

propEncodedLength ::RSEXP -> Bool
propEncodedLength r = BL.length (encode r) `mod` 4 == 0

tests = [("encodeDecodeIdentity", quickCheck propEncodeDecodeIdentity),
         ("encodedLength", quickCheck propEncodedLength)
        ]

main = mapM_ (\(s,a) -> printf "%-25s: " s >> a) tests

