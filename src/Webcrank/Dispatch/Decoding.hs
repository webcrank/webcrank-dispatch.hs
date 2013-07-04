module Webcrank.Dispatch.Decoding
  ( decodePath
  ) where

import           Data.Functor                        ((<$>))
import           Data.Text                           (Text)
import           Webcrank.Dispatch.Types

newtype PathDec a b = PathDec ([Text] -> b -> Maybe (a, [Text]))

instance PathSpec PathDec where
  lit str = PathDec dec where 
    dec (h:t) x = if h == str then Just (x, t) else Nothing
    dec _ _ = Nothing
  param (PathParam _ dec) = PathDec dec' where 
    dec' (h:t) f = dec h >>= (\a -> Just (f a, t))
    dec' _ _ = Nothing
  (PathDec deca) </> (PathDec decb) = PathDec dec where 
    dec inp f = deca inp f >>= (\(f', inp') -> decb inp' f') 
  splat = PathDec $ \xs f -> Just (f xs, [])

decodePath :: [Text] -> PathDec a b -> b -> Maybe a
decodePath inp (PathDec pt) f = pt inp f >>= exactly where
  exactly (a, inp') = if null inp' then Just a else Nothing

