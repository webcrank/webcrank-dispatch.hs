-- |Type-safe dispatching of requests for Webcrank, based on <http://okmij.org/ftp/typed-formatting/FPrintScan.html#DSL-FIn>
module Webcrank.Dispatch
  ( module X
  , dispatcher
  ) where

import Control.Arrow               (Kleisli(..), (<+>))
import Data.Foldable               (foldl')
import Webcrank.Dispatch.Types     as X
import Webcrank.Dispatch.Path      as X
import Webcrank.Dispatch.Route     as X

dispatcher :: [req -> Maybe (m res)] -> req -> Maybe (m res)
dispatcher = runKleisli . k where
  k = foldl' (\k' f -> k' <+> Kleisli f) (Kleisli $ const Nothing)

