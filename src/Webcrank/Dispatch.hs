{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Webcrank.Dispatch
-- Copyright   :  (C) 2015 Richard Wallace
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Richard Wallace <rwallace@thewallacepack.net>
-- Stability   :  provisional
-----------------------------------------------------------------------------

module Webcrank.Dispatch
  ( -- * Paths
    -- ** Building Paths
    root
  , (</>)
  , param
  , RR.Path
    -- ** Rendering Paths
  , renderPath
  , params
  , HBuild'(..)
    -- * Dispatching
  , (==>)
  , dispatch
  , Dispatcher
  ) where

import Control.Monad.Identity
import qualified Data.HashMap.Strict as HM
import Data.HVect
import Data.Maybe
import Data.Monoid
import Data.Text (Text)
import Data.Typeable (Typeable)
import Web.PathPieces
import Web.Routing.AbstractRouter
import qualified Web.Routing.SafeRouting as RR

-- | The simplest @'Path'@ is the @root@ path, which is equivalent to @/@.
root :: RR.Path '[]
root = RR.root

-- | Other routes can be built with @</>@:
--
-- @
-- docsPath = "package" \<\/> "webcrank-dispatch-0.1" \<\/> "docs"
-- @
(</>) :: RR.Path as -> RR.Path bs -> RR.Path (Append as bs)
(</>) = (RR.</>)

-- | Paths can contain parameters.  To create a parameterized path, use
-- @param@ as a path component:
--
-- @
-- docsPath :: Path '[String]
-- docsPath = "package" \<\/> param \<\/> "docs"
-- @
--
-- Paths can contain as many parameters of varying types as needed:
--
-- @
-- wat :: Path '[String, Int, Bool, Int, String]
-- wat :: "this" \<\/> param \<\/> param \<\/> "crazyness" \<\/> param \<\/> "ends" \<\/> param \<\/> param
-- @
--
-- Path parameters can be of any type that have instances for @'Typeable'@
-- and @'PathPiece'@.
param :: (Typeable a, PathPiece a) => RR.Path (a ': '[])
param = RR.var

-- | @Path@s can be rendered using @'renderPath'@ and
-- @'params'@.
--
-- >>> renderPath root params
-- ["/"]
--
-- >>> renderPath docsPath $ params "webcrank-dispatch-0.1"
-- ["package", "webcrank-dispatch-0.1", "docs"]
--
-- >>> renderPath wat $ params "down is up" 42 False 7 "up is down"
-- ["this", "down is up", "42", "crazyness", "False", "ends", "7", "up is down"]
--
-- Note in the last example that no encoding is done by @renderPath@.
renderPath :: RR.Path l -> HVect l -> [Text]
renderPath = RR.renderRoute'

params :: (HBuild' '[] r) => r
params = hBuild' HNil

class HBuild' l r where
  hBuild' :: HVect l -> r

instance (l' ~ ReverseLoop l '[]) => HBuild' l (HVect l') where
  hBuild' l = hVectReverse l

instance HBuild' (a ': l) r => HBuild' l (a -> r) where
  hBuild' l x = hBuild' (HCons x l)

-- | An elementary @'Dispatcher'@ can be built using @'==>'@.
--
-- @disp = root ==> \"Dispatched\"@
--
-- @Dispatcher@s form a @'Monoid'@, so more interesting dispatchers can
-- be built with @'<>'@ or @'mconcat'@.
--
-- @
-- disp = mconcat
--   [ root ==> "Welcome!"
--   , "echo" </> param ==> id
--   ]
-- @
(==>) :: RR.Path as -> HVectElim as a -> Dispatcher a
(==>) p r = Dispatcher $ hookRoute () (SafeRouterPath p) (RR.HVectElim' r)
infixr 8 ==>

-- | Dispatching requests is done with @'dispatch'@. It turns a
-- @Dispatcher@ into a function from a list of decoded path components
-- to a possible handler.
--
-- >>> dispatch (root ==> "Welcome!") [""]
-- Just "Welcome!"
--
-- >>> dispatch (root ==> "Welcome!") ["echo", "Goodbye!"]
-- Nothing
--
-- >>> dispatch (root ==> "Welcome!" <> "echo" </> param ==> id) ["echo", "Goodbye!"]
-- Just "Goodbye!"
dispatch :: Dispatcher a -> [Text] -> Maybe a
dispatch (Dispatcher r) = case runIdentity $ runRegistry SafeRouter r of
  (_, f, _) -> fmap snd . listToMaybe . f ()

newtype Dispatcher a = Dispatcher (RegistryT (SafeRouter a) () () Identity ())

instance Monoid (Dispatcher a) where
  mempty = Dispatcher $ return ()
  mappend (Dispatcher x) (Dispatcher y) = Dispatcher $ x >> y

data SafeRouter a = SafeRouter

instance AbstractRouter (SafeRouter a) where
  newtype Registry (SafeRouter a) = SafeRouterReg (RR.PathMap a, [[Text] -> a])
  newtype RoutePath (SafeRouter a) xs = SafeRouterPath (RR.Path xs)
  type RouteAction (SafeRouter a) = RR.HVectElim' a
  type RouteAppliedAction (SafeRouter a) = a
  subcompCombine (SafeRouterPath p1) (SafeRouterPath p2) =
    SafeRouterPath $ p1 </> p2
  emptyRegistry = SafeRouterReg (RR.emptyPathMap, [])
  rootPath = SafeRouterPath RR.Empty
  defRoute (SafeRouterPath path) action (SafeRouterReg (a, cAll)) =
    SafeRouterReg
      ( RR.insertPathMap' path (hVectUncurry $ RR.flipHVectElim action) a
      , cAll
      )
  fallbackRoute routeDef (SafeRouterReg (a, cAll)) =
    SafeRouterReg (a, cAll <> [routeDef])
  matchRoute (SafeRouterReg (a, cAll)) pathPieces =
    let matches = RR.match a pathPieces
        matches' =
            if null matches
            then matches <> fmap (\f -> f pathPieces) cAll
            else matches
    in zip (replicate (length matches') HM.empty) matches'

