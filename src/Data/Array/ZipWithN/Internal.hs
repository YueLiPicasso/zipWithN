{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.Array.ZipWithN.Internal where

import qualified Data.Key as K
import           Prelude hiding (zipWith)

----------------------------------------------------------------
-- realm for zipWithN
----------------------------------------------------------------


-- | This class provides the necessary polymorphism. It is only
-- exported for the sake of giving type signatures.
--
-- Because we can't do functor composition without a lot of noise
-- from newtype wrappers, we use @gr@ and @kr@ to precompose the
-- direct/list functor with the reader functor and the return type.
class K.Zip f => ZipWithN f a gr kr | kr -> a gr where
    _zipWithN :: f (a -> gr) -> f a -> kr

instance (K.Zip f, f b ~ fb) => ZipWithN f a b fb where
    _zipWithN = K.zipWith ($)

instance (K.Zip f, ZipWithN f b gr kr) =>
         ZipWithN f a (b -> gr) (f b -> kr) where
    _zipWithN = (_zipWithN .) . K.zipWith ($)


-- | Polyadic version of 'map'/'zipWith'. The given type signature
-- isn't terribly helpful or intuitive. The /real/ type signature
-- is:
--
-- > zipWithN :: {forall a}^N. ({a->}^N  r) -> ({f a ->}^N  r)
--
-- Note that the @a@ type variables are meta and so are independent
-- from one another, despite being correlated in N across all
-- repetitions.
--
-- >>> let f i c d = c : show (i::Int) ++ " " ++ show (d::Double)
-- >>> let xs = [1 :: Int ..]
-- >>> let ys = "hoge"
-- >>> let zs = [1 :: Double ..]
-- >>> zipWithN f xs ys zs :: [String]
-- ["h1 1.0","o2 2.0","g3 3.0","e4 4.0"]
--
-- >>> import qualified Data.Vector as V
-- >>> instance K.Zip V.Vector where zipWith = V.zipWith
-- >>> let vx = V.fromList [1..10] :: V.Vector Int
-- >>> zipWithN (*) vx vx
-- fromList [1,4,9,16,25,36,49,64,81,100]

zipWithN :: (ZipWithN f b gr kr)
            => (a -> b -> gr) -> f a -> f b -> kr
zipWithN func xs = _zipWithN (fmap func xs)


----------------------------------------------------------------
-- realm for withNZip
----------------------------------------------------------------


-- type level, first-in first-out list, that contains only values of type (v a)
-- we don't export this to avoid minimum confusion with the outer world.
data Cons (v :: * -> *) a b = Cons a b deriving (Eq, Show)
data Nil  (v :: * -> *)     = Nil      deriving (Eq, Show)


-- | the type-class states that if you insert
--   (v a) into vxS, the resulting type is vyS
class Insert v a vxS vyS | v a vxS -> vyS where
  insert :: v a -> vxS -> vyS

instance Insert v a (Nil v) (Cons v (v a) (Nil v)) where
  insert va Nil = Cons va Nil

instance  (Insert v a vxS vyS) => Insert v a (Cons v (v x) vxS) (Cons v (v x) vyS) where
  insert va (Cons vb vbS) = (Cons vb $ insert va vbS)



-- | perfom functional applications inside the container,
--   as many time as possible, and return the results.
class Reduce v f vxS result vyS | v f vxS -> result vyS where
  reduce :: v f -> vxS -> (result, vyS)

instance (Functor v) => Reduce v f (Nil v) (v f) (Nil v) where
  reduce vf Nil = (vf, Nil)

instance (K.Zip v, Reduce v f vxS result vyS) => Reduce v (a->f) (Cons v (v a)  vxS) result vyS where
  reduce vf (Cons va vxS) = reduce (K.zipWith ($) vf va) vxS

reduceFinal :: Reduce v f vxS result (Nil v) => v f -> vxS -> result
reduceFinal vf vxS = vr where (vr, Nil) = reduce vf vxS



class PType a r where
  spr :: a -> r


instance (Insert v b (vaS v) vyS, PType vyS r) => PType (vaS v) (v b->r) where
  spr vaS = (\vb -> spr (insert vb vaS))

instance (Insert v b (vaS v a2 b2) vyS, PType vyS r) => PType (vaS v a2 b2) (v b->r) where
  spr vaS = (\vb -> spr (insert vb vaS))


instance (K.Zip v, Reduce v f0 vaS result (Nil v)) =>  PType (Cons v (v i)  vaS) ((i -> f0)->result) where
  spr (Cons vi vaS) = (\f -> reduceFinal (fmap f vi) vaS)

-- | A variant of zipWithN that takes the zipping function as its last
--   argument.
--
-- >>> let f i c d = c : show (i::Int) ++ " " ++ show (d::Double)
-- >>> let xs = [1 :: Int ..]
-- >>> let ys = "hoge"
-- >>> let zs = [1 :: Double ..]
-- >>> withNZip xs ys zs f
-- ["h1 1.0","o2 2.0","g3 3.0","e4 4.0"]


withNZip :: forall v r a. PType (Cons v (v a) (Nil v)) r=> v a -> r
withNZip vx = spr (Cons vx (Nil :: Nil v) :: Cons v (v a) (Nil v))
