{-# LANGUAGE Rank2Types #-}
module Webcrank.Dispatch.Route
  ( (-->)
  ) where

import           Control.Applicative                 (Const(..))
import           Data.Text                           (Text)
import           Unsafe.Coerce
import           Webcrank.Dispatch.Types

newtype PathRoute r a b = PathRoute ((r, [Text]) -> b -> Maybe (a, r, [Text]))

instance HasPathInfo r => PathSpec (PathRoute r) where
  lit str = PathRoute check where
    check (r, h:t) x = if h == str then Just (x, r, t) else Nothing
    check _ _ = Nothing
  param (PathParam _ dec) = PathRoute dec' where 
    dec' (r, h:t) f = dec h >>= (\a -> Just (f a, r, t))
    dec' _ _ = Nothing
  (PathRoute deca) </> (PathRoute decb) = PathRoute dec where 
    dec inp f = deca inp f >>= (\(f', r', inp') -> decb (r', inp') f') 
  splat = PathRoute $ \(r, xs) f -> Just (f xs, set dispPath xs r, [])

type Lens' a b = Functor f => (b -> f b) -> a -> f a

(^.) :: a -> Lens' a b -> b
s ^. l = getConst (l Const s)
{-# INLINE (^.) #-}

set :: Lens' a b -> b -> a -> a
set l b = getConst `unsafeCoerce` l (\_ -> Const b)
{-# INLINE set #-}

(-->) :: HasPathInfo req => PathRoute req (req -> m res) b -> b -> req -> Maybe (m res)
(-->) (PathRoute rt) f rq = rt inp f >>= run where
  run (f', rq', inp') = if null inp' then Just (f' rq') else Nothing
  inp = (rq, rq ^. path)
infixl 5 -->

