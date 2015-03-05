{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Webcrank.Dispatch
  ( Dispatcher
  , Path
  , dispatch
  , (==>)
  , (</>)
  , var
  , renderRoute
  , renderRoute'
  , (.*.)
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

(.*.) :: t -> HVect ts1 -> HVect (t ': ts1)
(.*.) = HCons
infixr 2 .*.

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

