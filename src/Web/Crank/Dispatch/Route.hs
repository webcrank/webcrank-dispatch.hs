{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}
module Web.Crank.Dispatch.Route
  ( PathRoute
  , (~>)
  ) where

import Data.Text (Text)
import Web.Crank.Dispatch.Types

newtype PathRoute r a b =
  PathRoute { runRoute :: [Text] -> b -> Maybe (a, [Text]) }

instance PathSpec (PathRoute r) where
  lit str = PathRoute $ \case
    h : t | h == str -> Just . (, t)
    _               -> const Nothing

  param (PathParam _ dec) = PathRoute $ \case
    h : t -> \f -> dec h >>= (\a -> Just (f a, t))
    _          -> const Nothing

  a </> b = PathRoute $ \ina f ->
    runRoute a ina f >>= (\(inb, g) -> runRoute b g inb)

  splat = PathRoute $ \xs f -> Just (f xs, [])

(~>) :: HasRequestPath req => PathRoute req (req -> m res) b -> b -> req -> Maybe (m res)
(~>) rt f rq = runRoute rt (rqPath rq) f >>= \case
  (g, rest) | null rest -> Just (g rq)
  _ -> Nothing
infixl 5 ~>

