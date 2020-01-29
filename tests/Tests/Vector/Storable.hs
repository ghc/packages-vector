{-# LANGUAGE ConstraintKinds #-}
module Tests.Vector.Storable (tests) where

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


testGeneralStorableVector :: forall a. (CommonContext a Data.Vector.Storable.Vector, Data.Vector.Storable.Storable a, Ord a, Data a) => Data.Vector.Storable.Vector a -> [Test]
testGeneralStorableVector dummy = concatMap ($ dummy) [
        testSanity,
        testPolymorphicFunctions,
        testOrdFunctions,
        testMonoidFunctions,
        testDataFunctions
    ]

testNumericStorableVector :: forall a. (CommonContext a Data.Vector.Storable.Vector, Data.Vector.Storable.Storable a, Ord a, Num a, Enum a, Random a, Data a) => Data.Vector.Storable.Vector a -> [Test]
testNumericStorableVector dummy = concatMap ($ dummy)
  [
    testGeneralStorableVector
  , testNumFunctions
  , testEnumFunctions
  ]

tests =
  [ testGroup "Data.Vector.Storable.Vector (Int)" $
    testNumericStorableVector (undefined :: Data.Vector.Storable.Vector Int)
  , testGroup "Data.Vector.Storable.Vector (Double)" $
    testNumericStorableVector (undefined :: Data.Vector.Storable.Vector Double)
  ]
