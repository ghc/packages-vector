{-# LANGUAGE ConstraintKinds #-}
module Tests.Vector.Boxed (tests) where

import Boilerplater
import Utilities as Util hiding (limitUnfolds)

import Data.Functor.Identity
import qualified Data.Traversable as T (Traversable(..))
import Data.Foldable (Foldable(foldMap))
import Data.Orphans ()

import qualified Data.Vector.Generic as V
import qualified Data.Vector
import qualified Data.Vector.Primitive
import qualified Data.Vector.Storable
import qualified Data.Vector.Unboxed
import qualified Data.Vector.Fusion.Bundle as S

import Test.QuickCheck

import Test.Framework
import Test.Framework.Providers.QuickCheck2

import Text.Show.Functions ()
import Data.List
import Data.Monoid
import qualified Control.Applicative as Applicative
import System.Random       (Random)

import Data.Functor.Identity
import Control.Monad.Trans.Writer

import Control.Monad.Zip

import Data.Data

import Tests.Vector.Property


testGeneralBoxedVector :: forall a. (CommonContext a Data.Vector.Vector, Ord a, Data a) => Data.Vector.Vector a -> [Test]
testGeneralBoxedVector dummy = concatMap ($ dummy) [
        testSanity,
        testPolymorphicFunctions,
        testOrdFunctions,
        testTuplyFunctions,
        testNestedVectorFunctions,
        testMonoidFunctions,
        testFunctorFunctions,
        testMonadFunctions,
        testApplicativeFunctions,
        testAlternativeFunctions,
        testDataFunctions
    ]

testBoolBoxedVector dummy = concatMap ($ dummy)
  [
    testGeneralBoxedVector
  , testBoolFunctions
  ]

testNumericBoxedVector :: forall a. (CommonContext a Data.Vector.Vector, Ord a, Num a, Enum a, Random a, Data a) => Data.Vector.Vector a -> [Test]
testNumericBoxedVector dummy = concatMap ($ dummy)
  [
    testGeneralBoxedVector
  , testNumFunctions
  , testEnumFunctions
  ]

tests =
  [ testGroup "Bool" $
    testBoolBoxedVector (undefined :: Data.Vector.Vector Bool)
  , testGroup "Int" $
    testNumericBoxedVector (undefined :: Data.Vector.Vector Int)
  ]
