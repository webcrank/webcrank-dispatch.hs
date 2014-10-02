module Webcrank.Dispatch.Types where

import Data.Text (Text)

-- |Encoding/decoding pair
data PathParam a = PathParam (a -> Text) (Text -> Maybe a)

class PathSpec repr where
  lit   :: Text -> repr a a
  param :: PathParam b -> repr a (b -> a)
  (</>) :: repr b c -> repr a b -> repr a c
  splat :: repr a ([Text] -> a)
infixl 7 </>

class HasRequestPath a where
  rqPath :: a -> [Text]

