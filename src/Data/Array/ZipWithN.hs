{-# LANGUAGE MultiParamTypeClasses
           , FunctionalDependencies
           , FlexibleContexts
           , FlexibleInstances
           , OverlappingInstances
           , TypeFamilies
           , UndecidableInstances
           #-}

{-# OPTIONS_GHC -Wall -fwarn-tabs #-}

----------------------------------------------------------------
--                                                  ~ 2009.12.20
-- |
-- Module      :  Data.List.ZipWithN
-- Copyright   :  Copyright (c) 2009--2010 wren ng thornton
-- License     :  BSD3
-- Maintainer  :  wren@community.haskell.org
-- Stability   :  experimental
-- Portability :  non-portable (MPTCs, FunDeps,...)
--
-- Provides a polyvariadic 'map'/'zipWith' like the @map@ in Scheme.
-- For more details on this style of type hackery, see:
--
--    * Chung-chieh Shan, /A polyvariadic function of a non-regular/
--      /type (Int->)^N ([]^N e)->.../
--      <http://okmij.org/ftp/Haskell/polyvariadic.html#polyvartype-fn>
--
-- For alternative approaches to solving this problem, see also
--
--    * http://paczesiowa.blogspot.com/2010/03/generalized-zipwithn.html
--
--    * http://hackage.haskell.org/packages/archive/TypeCompose/latest/doc/html/Data-Zip.html
----------------------------------------------------------------
module Data.Array.ZipWithN (zipWithN, withNZip) where

import Data.Array.ZipWithN.Internal