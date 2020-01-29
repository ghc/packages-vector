{-# LANGUAGE ConstraintKinds #-}
module Tests.Vector.Primitive (tests) where

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


testGeneralPrimitiveVector :: forall a. (CommonContext a Data.Vector.Primitive.Vector, Data.Vector.Primitive.Prim a, Ord a, Data a) => Data.Vector.Primitive.Vector a -> [Test]
testGeneralPrimitiveVector dummy = concatMap ($ dummy) [
        testSanity,
        testPolymorphicFunctions,
        testOrdFunctions,
        testMonoidFunctions,
        testDataFunctions
    ]

testNumericPrimitiveVector :: forall a. (CommonContext a Data.Vector.Primitive.Vector, Data.Vector.Primitive.Prim a, Ord a, Num a, Enum a, Random a, Data a) => Data.Vector.Primitive.Vector a -> [Test]
testNumericPrimitiveVector dummy = concatMap ($ dummy)
 [
   testGeneralPrimitiveVector
 , testNumFunctions
 , testEnumFunctions
 ]

tests =
  [ testGroup "Int" $
    testNumericPrimitiveVector (undefined :: Data.Vector.Primitive.Vector Int)
  , testGroup "Double" $
    testNumericPrimitiveVector
      (undefined :: Data.Vector.Primitive.Vector Double)
  ]
