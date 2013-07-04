module Webcrank.Dispatch.Path
  ( injectPath
  , extractPath
  , pathParam
  , intParam
  , textParam
  ) where

import           Data.Monoid                         ((<>))
import           Data.Text                           (Text, pack)
import qualified Data.Text                           as T (null)
import           Data.Text.Read                      (signed, decimal)

import Webcrank.Dispatch.Types

newtype PathInj a b = PathInj (([Text] -> a) -> b)

instance PathSpec PathInj where
  lit str = PathInj $ \k -> k [str]
  param (PathParam subst _) = PathInj $ \k -> k . return . subst
  (PathInj a) </> (PathInj b) = PathInj $ \k -> a (\sa -> b (\sb -> k (sa <> sb)))
  splat = PathInj $ \k x -> k x

injectPath :: PathInj [Text] b -> b
injectPath (PathInj pt) = pt id

newtype PathExtr a b = PathExtr ([Text] -> b -> Maybe (a, [Text]))

instance PathSpec PathExtr where
  lit str = PathExtr dec where 
    dec (h:t) x = if h == str then Just (x, t) else Nothing
    dec _ _ = Nothing
  param (PathParam _ dec) = PathExtr dec' where 
    dec' (h:t) f = dec h >>= (\a -> Just (f a, t))
    dec' _ _ = Nothing
  (PathExtr deca) </> (PathExtr decb) = PathExtr dec where 
    dec inp f = deca inp f >>= (\(f', inp') -> decb inp' f') 
  splat = PathExtr $ \xs f -> Just (f xs, [])

extractPath :: [Text] -> PathExtr a b -> b -> Maybe a
extractPath inp (PathExtr pt) f = pt inp f >>= exactly where
  exactly (a, inp') = if null inp' then Just a else Nothing

pathParam :: PathSpec repr => (b -> Text) -> (Text -> Maybe b) -> repr a (b -> a)
pathParam = (param .) . PathParam

intParam :: PathSpec repr => repr a (Int -> a)
intParam = pathParam enc dec where
  enc = pack . show
  dec = noLeftOvers . signed decimal

noLeftOvers :: Either b (a, Text) -> Maybe a
noLeftOvers (Left _)       = Nothing
noLeftOvers (Right (a, b)) = if T.null b then Just a else Nothing

textParam :: PathSpec repr => repr a (Text -> a)
textParam = pathParam id Just

