{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import Data.Monoid
import Data.Text (Text)

import Webcrank.Dispatch

type Res = Text

packages :: Path '[]
packages = "packages"

package :: Path '[Text]
package = packages </> param

packageVer :: Path '[Text, Int]
packageVer = package </> param

packageVerDoc :: Path '[Text, Int]
packageVerDoc = packageVer </> "doc"

packagesResource :: Text
packagesResource = "Packages: webcrank-dispatch webcrank webcrank-wai"

packageResource :: Text -> Text
packageResource "webcrank-dispatch" = "webcrank-dispatch versions: 1 2 3"
packageResource _ = "Not found"

packageVerResource :: Text -> Int -> Text
packageVerResource "webcrank-dispatch" 1 = "webcrank-dispatch v1"
packageVerResource "webcrank-dispatch" 2 = "webcrank-dispatch v2"
packageVerResource "webcrank-dispatch" 3 = "webcrank-dispatch v3"
packageVerResource _ _ = "Not found"

dispatcher :: [Text] -> Maybe Text
dispatcher = dispatch $ mconcat
  [ packages      ==> packagesResource
  , package       ==> packageResource
  , packageVer    ==> packageVerResource
  , packageVerDoc ==> (\_ _ -> "some docs")
  ]

main :: IO ()
main = do
  -- route rendering
  print $ renderPath packages params -- ["packages"]
  print $ renderPath package $ params "webcrank-dispatch" -- ["packages", "webcrank-dispatch"]
  print $ renderPath packageVer $ params "webcrank-dispatch" 1 -- ["packages", "webcrank-dispatch", "1"]
  print $ renderPath packageVerDoc $ params "webcrank-dispatch" 1 -- ["packages", "webcrank-dispatch", "1", "doc"]

  -- dispatching
  print $ dispatcher ["packages"] -- Just "..."
  print $ dispatcher ["unknown"] -- Nothing

  print $ dispatcher ["packages", "webcrank-dispatch"] -- Just "..."
  print $ dispatcher ["packages", "nothing"] -- Just "Not found"

  print $ dispatcher ["packages", "webcrank-dispatch", "2"] -- Just "..."
  print $ dispatcher ["packages", "webcrank-dispatch", "5"] -- Just "Not found"
  print $ dispatcher ["packages", "webcrank-dispatch", "not-an-int"] -- Nothing

  print $ dispatcher ["packages", "webcrank-dispatch", "2", "doc"] -- Just "..."
  print $ dispatcher ["packages", "webcrank-dispatch", "2", "unknown"] -- Nothing

