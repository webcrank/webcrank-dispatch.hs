module Web.Crank.Dispatch.Path
  (
  -- * Path parameters
    intParam
  , textParam
  , pathParam
  -- * Type-safe path construction
  , toPath
  -- * Type-safe path extraction
  , fromPath
  , PathInj
  , PathExtr
  ) where

import Data.Monoid ((<>))
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Read as T
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.Builder as LT
import qualified Data.Text.Lazy.Builder.Int as LT

import Web.Crank.Dispatch.Types

-- $setup
-- >>> :set -XOverloadedStrings
-- >>> import Test.QuickCheck
-- >>> instance Arbitrary Text where arbitrary = fmap T.pack arbitrary

-- | Constructor for path parameters.
pathParam :: PathSpec repr => (b -> Text) -> (Text -> Maybe b) -> repr a (b -> a)
pathParam = (param .) . PathParam

noLeftOvers :: Either b (a, Text) -> Maybe a
noLeftOvers x = case x of
  Right (a, b) | T.null b -> Just a
  _ -> Nothing

-- | An `Int` path parameter.
--
-- >>> toPath intParam 8
-- ["8"]
--
-- >>> fromPath intParam ["8"] id
-- Just 8
--
-- prop> Just x == fromPath intParam (toPath intParam x) id
intParam :: PathSpec repr => repr a (Int -> a)
intParam = pathParam enc dec where
  enc = LT.toStrict . LT.toLazyText . LT.decimal
  dec = noLeftOvers . T.signed T.decimal

-- | A `Text` path parameter.
--
-- >>> toPath textParam "A new blog"
-- ["A new blog"]
--
-- >>> fromPath textParam ["A new blog"] id
-- Just "A new blog"
--
-- prop> Just x == fromPath textParam (toPath textParam x) id
textParam :: PathSpec repr => repr a (Text -> a)
textParam = pathParam id Just

-- | Defines a represenation of a @PathSpec@ which allows for
-- parameter substitution and path construction. Used through @toPath@.
newtype PathInj a b = PathInj (([Text] -> a) -> b)

instance PathSpec PathInj where
  lit str = PathInj $ \k -> k [str]
  param (PathParam subst _) = PathInj $ \k -> k . return . subst
  (PathInj a) </> (PathInj b) = PathInj $ \k -> a (\sa -> b (\sb -> k (sa <> sb)))
  splat = PathInj $ \k x -> k x

-- | Given a path specification, gives a function to build the path.
--
-- >>> toPath $ lit "blog"
-- ["blog"]
--
-- >>> toPath (lit "blog" </> intParam </> intParam </> textParam) 2014 8 "A post"
-- ["blog","2014","8","A post"]
toPath :: PathInj [Text] b -> b
toPath (PathInj pt) = pt id

-- | Defines a represenation of a @PathSpec@ which allows for
-- parameter extraction. Used through `fromPath`.
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

-- | Given a path specification, a path, and a function to process them,
-- extracts path parameters in a type-safe manner.
--
-- >>> fromPath (lit "blog" </> intParam </> intParam </> textParam) ["blog","2014","8","A post"] (,,)
-- Just (2014,8,"A post")
fromPath :: PathExtr a b -> [Text] -> b -> Maybe a
fromPath (PathExtr pt) inp f = pt inp f >>= \(a, inb) ->
  if null inb
    then Just a
    else Nothing


