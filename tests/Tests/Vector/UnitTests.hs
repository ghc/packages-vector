{-# LANGUAGE CPP #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Tests.Vector.UnitTests (tests) where

import Control.Applicative as Applicative
import Control.Exception
import Control.Monad.Primitive
import Control.Monad.ST
import qualified Data.List as List
import qualified Data.Vector.Generic  as Generic
import qualified Data.Vector.Generic.Mutable as MGeneric
import qualified Data.Vector as Boxed
import qualified Data.Vector.Primitive as Primitive
import qualified Data.Vector.Storable as Storable
import qualified Data.Vector.Unboxed as Unboxed
import Foreign.Ptr
import Foreign.Storable
import Text.Printf

import Test.Framework
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit (Assertion, assertBool, assertFailure)

newtype Aligned a = Aligned { getAligned :: a }

instance (Storable a) => Storable (Aligned a) where
  sizeOf _    = sizeOf (undefined :: a)
  alignment _ = 128
  peek ptr    = Aligned Applicative.<$> peek (castPtr ptr)
  poke ptr    = poke (castPtr ptr) . getAligned

checkAddressAlignment :: forall a. (Storable a) => Storable.Vector a -> Assertion
checkAddressAlignment xs = Storable.unsafeWith xs $ \ptr -> do
  let ptr'  = ptrToWordPtr ptr
      msg   = printf "Expected pointer with alignment %d but got 0x%08x" (toInteger align) (toInteger ptr')
      align :: WordPtr
      align = fromIntegral $ alignment dummy
  assertBool msg $ (ptr' `mod` align) == 0
  where
    dummy :: a
    dummy = undefined

tests :: [Test]
tests =
  [ testGroup "Data.Vector.Storable.Vector Alignment"
      [ testCase "Aligned Double" $
          checkAddressAlignment alignedDoubleVec
      , testCase "Aligned Int" $
          checkAddressAlignment alignedIntVec
      ]
  , testGroup "Regression tests"
    [ testGroup "checkSlice (slice crash #257)"
      [ testCase "Boxed" $ checkSliceOverflow Boxed.slice
      , testCase "Primitive" $ checkSliceOverflow Primitive.slice
      , testCase "Storable" $ checkSliceOverflow Storable.slice
      , testCase "Unboxed" $ checkSliceOverflow Unboxed.slice
      ]
    ]
  ]

checkSliceOverflow ::
     Generic.Vector v Int => (Int -> Int -> v Int -> v Int) -> Assertion
checkSliceOverflow slice' = do
  eRes <- try (pure $! slice' 1 m vec)
  case eRes of
    Right _ ->
      assertFailure "Data.Vector.Internal.Check.checkSlice has overflown"
    Left (ErrorCall err) ->
      let assertMsg =
            List.concat
              [ "Expected slice function to produce an 'error' ending with: \""
              , errSuffix
              , "\" instead got: \""
              , err
              ]
       in assertBool assertMsg (errSuffix `List.isSuffixOf` err)
  where
    m = maxBound :: Int
    errSuffix =
      "(slice): invalid slice (1," ++ show m ++ "," ++ show (length xs) ++ ")"
    xs = [1, 2, 3, 4, 5] :: [Int]
    -- Ensure vector is not build from a stream
    vec = runST $ do
      mv <- MGeneric.new 5
      mapM_ (\(i, e) -> MGeneric.write mv i e) $ List.zip [0..] xs
      Generic.freeze mv

alignedDoubleVec :: Storable.Vector (Aligned Double)
alignedDoubleVec = Storable.fromList $ map Aligned [1, 2, 3, 4, 5]

alignedIntVec :: Storable.Vector (Aligned Int)
alignedIntVec = Storable.fromList $ map Aligned [1, 2, 3, 4, 5]

#if __GLASGOW_HASKELL__ >= 800
-- Ensure that Mutable is really an injective type family by typechecking a
-- function which relies on injectivity.
_f :: (Generic.Vector v a, Generic.Vector w a, PrimMonad f)
   => Generic.Mutable v (PrimState f) a -> f (w a)
_f v = Generic.convert `fmap` Generic.unsafeFreeze v
#endif
