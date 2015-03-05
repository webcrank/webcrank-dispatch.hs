{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Webcrank.Dispatch
  ( Dispatcher
  , Path
  , dispatch
  , (==>)
  , (</>)
  , var
  , renderPath
  , params
  , HVect(..)
  ) where

import Control.Monad.Identity
import qualified Data.HashMap.Strict as HM
import Data.HVect
import Data.Maybe
import Data.Monoid
import Data.Text (Text)
import Web.Routing.AbstractRouter
import Web.Routing.SafeRouting hiding (SafeRouter, SafeRouterReg, SafeRouterPath)

newtype Dispatcher a = Dispatcher (RegistryT (SafeRouter a) () () Identity ())

instance Monoid (Dispatcher a) where
  mempty = Dispatcher $ return ()
  mappend (Dispatcher x) (Dispatcher y) = Dispatcher $ x >> y

(==>) :: Path as -> HVectElim as a -> Dispatcher a
(==>) p r = Dispatcher $ hookRoute () (SafeRouterPath p) (HVectElim' r)
infixr 8 ==>

dispatch :: Dispatcher a -> [Text] -> Maybe a
dispatch (Dispatcher r) = case runIdentity $ runRegistry SafeRouter r of
  (_, f, _) -> fmap snd . listToMaybe . f ()

renderPath :: Path l -> HVect l -> [Text]
renderPath = renderRoute'

params :: (HBuild' '[] r) => r
params = hBuild' HNil

class HBuild' l r where
  hBuild' :: HVect l -> r

instance (l' ~ ReverseLoop l '[]) => HBuild' l (HVect l') where
  hBuild' l = hVectReverse l

instance HBuild' (a ': l) r => HBuild' l (a -> r) where
  hBuild' l x = hBuild' (HCons x l)

data SafeRouter a = SafeRouter

instance AbstractRouter (SafeRouter a) where
  newtype Registry (SafeRouter a) = SafeRouterReg (PathMap a, [[Text] -> a])
  newtype RoutePath (SafeRouter a) xs = SafeRouterPath (Path xs)
  type RouteAction (SafeRouter a) = HVectElim' a
  type RouteAppliedAction (SafeRouter a) = a
  subcompCombine (SafeRouterPath p1) (SafeRouterPath p2) =
    SafeRouterPath $ p1 </> p2
  emptyRegistry = SafeRouterReg (emptyPathMap, [])
  rootPath = SafeRouterPath Empty
  defRoute (SafeRouterPath path) action (SafeRouterReg (a, cAll)) =
    SafeRouterReg
      ( insertPathMap' path (hVectUncurry $ flipHVectElim action) a
      , cAll
      )
  fallbackRoute routeDef (SafeRouterReg (a, cAll)) =
    SafeRouterReg (a, cAll <> [routeDef])
  matchRoute (SafeRouterReg (a, cAll)) pathPieces =
    let matches = match a pathPieces
        matches' =
            if null matches
            then matches <> fmap (\f -> f pathPieces) cAll
            else matches
    in zip (replicate (length matches') HM.empty) matches'

