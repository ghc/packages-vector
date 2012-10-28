{-# LANGUAGE CPP, ExistentialQuantification, MultiParamTypeClasses, FlexibleInstances, Rank2Types, BangPatterns, KindSignatures, GADTs, ScopedTypeVariables #-}

-- |
-- Module      : Data.Vector.Fusion.MultiStream.Monadic
-- Copyright   : (c) Geoffrey Mainland 2011-2012
-- License     : BSD-style
--
-- Maintainer  : Geoffrey Mainland <mainland@apeiron.net>
-- Stability   : experimental
-- Portability : non-portable
--
-- Monadic @Multi@ stream combinators.
--

#if !defined(__GLASGOW_HASKELL_LLVM__)
module Data.Vector.Fusion.MultiStream.Monadic {-# WARNING "Only useful with the LLVM back end" #-} where
#else /* defined(__GLASGOW_HASKELL_LLVM__) */
module Data.Vector.Fusion.MultiStream.Monadic (
  MultiStream(..),

  -- * Multi Mapping
  mmap, mmapM, mmapM_, trans,

  -- * Multi Zipping
  mzipWithM_, mzipWithM, mzipWith,

  -- * Multi Folding
  mfoldl, mfoldlM, mfoldM,
  mfoldlu,
  mfoldl', mfoldlM', mfoldM',
  mfoldlu',

  -- * Conversion
  fromStream
) where

import Control.Monad  ( liftM )

import Data.Primitive.Multi
import Data.Vector.Fusion.Stream.Monadic ( Stream(..), Step(..), SPEC(..) )
import qualified Data.Vector.Fusion.Stream.Monadic as S

#include "vector.h"

data MultiStream m a = forall s .  MultiStream (s -> m (Step s (Multi a))) (s -> m (Step s a)) s

-- Multi Mapping
-- -------------

mmap :: Monad m => (a -> b) -> (Multi a -> Multi b) -> MultiStream m a -> MultiStream m b
{-# INLINE mmap #-}
mmap p q = mmapM (return . p) (return . q)

mmapM :: Monad m
      => (a -> m b)
      -> (Multi a -> m (Multi b))
      -> MultiStream m a
      -> MultiStream m b
{-# INLINE_FUSED mmapM #-}
mmapM p q (MultiStream stepm step1 s) = MultiStream stepm' step1' s
  where
    {-# INLINE_INNER stepm' #-}
    stepm' s = do
        r <- stepm s
        case r of
          Yield  x s' -> liftM  (`Yield` s') (q x)
          Skip     s' -> return (Skip    s')
          Done        -> return Done

    {-# INLINE_INNER step1' #-}
    step1' s = do
        r <- step1 s
        case r of
          Yield  x s' -> liftM  (`Yield` s') (p x)
          Skip     s' -> return (Skip    s')
          Done        -> return Done

consume :: Monad m => MultiStream m a -> m ()
{-# INLINE_FUSED consume #-}
consume (MultiStream stepm step1 s) = consumem_loop SPEC s
  where
    consumem_loop !sPEC s
      = do
          r <- stepm s
          case r of
            Yield _ s' -> consumem_loop SPEC s'
            Skip    s' -> consumem_loop SPEC s'
            Done       -> consume1_loop SPEC s

    consume1_loop !sPEC s
      = do
          r <- step1 s
          case r of
            Yield _ s' -> consume1_loop SPEC s'
            Skip    s' -> consume1_loop SPEC s'
            Done       -> return ()

mmapM_ :: Monad m => (a -> m b) -> (Multi a -> m (Multi b)) -> MultiStream m a -> m ()
{-# INLINE_FUSED mmapM_ #-}
mmapM_ p q = consume . mmapM p q

-- | Transform a 'Stream' to use a different monad
trans :: (Monad m, Monad m')
      => (forall a. m a -> m' a) -> MultiStream m a -> MultiStream m' a
{-# INLINE_FUSED trans #-}
trans f (MultiStream stepm step1 s) = MultiStream (f . stepm) (f . step1) s

-- Multi Zipping
-- -------------

mzipWithM :: forall m v a b c . Monad m
          => (a -> b -> m c)
          -> (Multi a -> Multi b -> m (Multi c))
          -> MultiStream m a
          -> MultiStream m b
          -> MultiStream m c
{-# INLINE_FUSED mzipWithM #-}
mzipWithM p q (MultiStream stepma step1a sa) (MultiStream stepmb step1b sb)
  = MultiStream stepm step1 (sa, sb, Nothing)
  where
    {-# INLINE_INNER stepm #-}
    stepm (sa, sb, Nothing) = do
        r <- stepma sa
        case r of
          Yield x sa' -> return $ Skip (sa', sb, Just (Left x))
          Skip    sa' -> return $ Skip (sa', sb, Nothing)
          Done        -> return $ Done

    stepm (sa, sb, Just (Left x)) = do
        r <- stepmb sb
        case r of
          Yield y sb' -> do { z <- q x y; return $ Yield z (sa, sb', Nothing) }
          Skip    sb' -> return $ Skip (sa, sb', Just (Left x))
          Done        -> return $ Done

    -- Can't happen
    stepm (_, _, Just (Right _)) =
        return Done

    {-# INLINE_INNER step1 #-}
    step1 (s1a, s1b, Nothing) = do
        r <- step1a s1a
        case r of
          Yield x s1a' -> return $ Skip (s1a', s1b, Just (Right x))
          Skip    s1a' -> return $ Skip (s1a', s1b, Nothing)
          Done         -> return $ Done

    step1 (s1a, s1b, Just (Right x)) = do
        r <- step1b s1b
        case r of
          Yield y s1b' -> do { z <- p x y; return $ Yield z (s1a, s1b', Nothing) }
          Skip    s1b' -> return $ Skip (s1a, s1b', Just (Right x))
          Done         -> return $ Done

    -- Can't happen
    step1 (_, _, Just (Left _)) =
        return Done

mzipWithM_ ::  forall m v a b c . Monad m
           =>  (a -> b -> m c)
           ->  (Multi a -> Multi b -> m (Multi c))
           ->  MultiStream m a
           ->  MultiStream m b
           ->  m ()
{-# INLINE mzipWithM_ #-}
mzipWithM_ p q sa sb = consume (mzipWithM p q sa sb)

mzipWith :: Monad m
         => (a -> b -> c)
         -> (Multi a -> Multi b -> Multi c)
         -> MultiStream m a
         -> MultiStream m b
         -> MultiStream m c
{-# INLINE mzipWith #-}
mzipWith p q = mzipWithM (\a b -> return (p a b)) (\a b -> return (q a b))

-- Multi Folding
-- -------------

-- | Left mfold
mfoldl  ::  Monad m
        =>  (a -> b -> a)
        ->  (a -> Multi b -> a)
        ->  a
        ->  MultiStream m b
        ->  m a
{-# INLINE mfoldl #-}
mfoldl p q =
    mfoldlM (\a b -> return (p a b)) (\a b -> return (q a b))

mfoldlM  ::  Monad m
         =>  (a -> b -> m a)
         ->  (a -> Multi b -> m a)
         ->  a
         ->  MultiStream m b
         ->  m a
{-# INLINE_FUSED mfoldlM #-}
mfoldlM p q z (MultiStream stepm step1 s) =
    mfoldlM_loopm SPEC z s
  where
    mfoldlM_loopm !sPEC z s
      = do
          r <- stepm s
          case r of
            Yield x s' -> do { z' <- q z x; mfoldlM_loopm SPEC z' s' }
            Skip    s' -> mfoldlM_loopm SPEC z s'
            Done       -> mfoldlM_loop1 SPEC z s

    mfoldlM_loop1 !sPEC z s
      = do
          r <- step1 s
          case r of
            Yield x s' -> do { z' <- p z x; mfoldlM_loop1 SPEC z' s' }
            Skip    s' -> mfoldlM_loop1 SPEC z s'
            Done       -> return z

-- | Same as 'mfoldlM'
mfoldM  ::  Monad m
        =>  (a -> b -> m a)
        ->  (a -> Multi b -> m a)
        ->  a
        ->  MultiStream m b
        ->  m a
{-# INLINE mfoldM #-}
mfoldM = mfoldlM

mfoldlu  ::  (Monad m, MultiType a)
         =>  (a -> a -> a)
         ->  (Multi a -> Multi a -> Multi a)
         ->  a
         ->  MultiStream m a
         ->  m a
{-# INLINE_FUSED mfoldlu #-}
mfoldlu p q z (MultiStream stepm step1 s) =
    mfoldlu_loopm SPEC (multireplicate z) s
  where
    mfoldlu_loopm !sPEC mz s
      = do
          r <- stepm s
          case r of
            Yield x s' -> do { let { mz' = q mz x }; mfoldlu_loopm SPEC mz' s' }
            Skip    s' -> mfoldlu_loopm SPEC mz s'
            Done       -> do { z' <- mfoldlu_loop1 SPEC z s 
                             ; return $ multifold p z' mz
                             }

    mfoldlu_loop1 !sPEC z s
      = do
          r <- step1 s
          case r of
            Yield x s' -> do { let { z' = p z x }; mfoldlu_loop1 SPEC z' s' }
            Skip    s' -> mfoldlu_loop1 SPEC z s'
            Done       -> return z

-- | Left mfold
mfoldl'  ::  Monad m
         =>  (a -> b -> a)
         ->  (a -> Multi b -> a)
         ->  a
         ->  MultiStream m b
         ->  m a
{-# INLINE mfoldl' #-}
mfoldl' p q =
    mfoldlM' (\a b -> return (p a b)) (\a b -> return (q a b))

mfoldlM'  ::  Monad m
          =>  (a -> b -> m a)
          ->  (a -> Multi b -> m a)
          ->  a
          ->  MultiStream m b
          ->  m a
{-# INLINE_FUSED mfoldlM' #-}
mfoldlM' p q z (MultiStream stepm step1 s) =
    mfoldlM'_loopm SPEC z s
  where
    mfoldlM'_loopm !sPEC z s
      = z `seq` do
          r <- stepm s
          case r of
            Yield x s' -> do { z' <- q z x; mfoldlM'_loopm SPEC z' s' }
            Skip    s' -> mfoldlM'_loopm SPEC z s'
            Done       -> mfoldlM'_loop1 SPEC z s

    mfoldlM'_loop1 !sPEC z s
      = z `seq` do
          r <- step1 s
          case r of
            Yield x s' -> do { z' <- p z x; mfoldlM'_loop1 SPEC z' s' }
            Skip    s' -> mfoldlM'_loop1 SPEC z s'
            Done       -> return z

-- | Same as 'mfoldlM''
mfoldM'  ::  Monad m
         =>  (a -> b -> m a)
         ->  (a -> Multi b -> m a)
         ->  a
         ->  MultiStream m b
         ->  m a
{-# INLINE mfoldM' #-}
mfoldM' = mfoldlM'

mfoldlu'  ::  (Monad m, MultiType a)
          =>  (a -> a -> a)
          ->  (Multi a -> Multi a -> Multi a)
          ->  a
          ->  MultiStream m a
          ->  m a
{-# INLINE_FUSED mfoldlu' #-}
mfoldlu' p q z (MultiStream stepm step1 s) =
    mfoldlu'_loopm SPEC (multireplicate z) s
  where
    mfoldlu'_loopm !sPEC mz s
      = mz `seq` do
          r <- stepm s
          case r of
            Yield x s' -> do { let { mz' = q mz x }; mfoldlu'_loopm SPEC mz' s' }
            Skip    s' -> mfoldlu'_loopm SPEC mz s'
            Done       -> do { z' <- mfoldlu'_loop1 SPEC z s 
                             ; return $ multifold p z' mz
                             }

    mfoldlu'_loop1 !sPEC z s
      = z `seq` do
          r <- step1 s
          case r of
            Yield x s' -> do { let { z' = p z x }; mfoldlu'_loop1 SPEC z' s' }
            Skip    s' -> mfoldlu'_loop1 SPEC z s'
            Done       -> return z

fromStream :: Monad m => Stream m a -> MultiStream m a
{-# INLINE_FUSED fromStream #-}
fromStream (Stream step s) = MultiStream (const (return Done)) step s
#endif /* defined(__GLASGOW_HASKELL_LLVM__) */
