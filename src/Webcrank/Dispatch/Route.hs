{-# LANGUAGE Rank2Types #-}
module Webcrank.Dispatch.Route
  ( (-->)
  ) where

import           Data.Text                           (Text)
import           Webcrank.Dispatch.Types

newtype PathRoute r a b = PathRoute ((r, [Text]) -> b -> Maybe (a, r, [Text]))

instance HasRequestPath r => PathSpec (PathRoute r) where
  lit str = PathRoute check where
    check (r, h:t) x = if h == str then Just (x, r, t) else Nothing
    check _ _ = Nothing
  param (PathParam _ dec) = PathRoute dec' where 
    dec' (r, h:t) f = dec h >>= (\a -> Just (f a, r, t))
    dec' _ _ = Nothing
  (PathRoute deca) </> (PathRoute decb) = PathRoute dec where 
    dec inp f = deca inp f >>= (\(f', r', inp') -> decb (r', inp') f') 
  splat = PathRoute $ \(r, xs) f -> Just (f xs, r, [])

(-->) :: HasRequestPath req => PathRoute req (req -> m res) b -> b -> req -> Maybe (m res)
(-->) (PathRoute rt) f rq = rt inp f >>= run where
  run (f', rq', inp') = if null inp' then Just (f' rq') else Nothing
  inp = (rq, rqPath rq)
infixl 5 -->

