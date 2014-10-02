-- |Type-safe dispatching of requests for Webcrank, based on <http://okmij.org/ftp/typed-formatting/FPrintScan.html#DSL-FIn>
module Webcrank.Dispatch
  ( dispatcher
  , toPath
  , fromPath
  -- * Route constructors
  , (~>)
  , (</>)
  -- ** Path parameters
  , lit
  , intParam
  , textParam
  , splat
  , pathParam
  , HasRequestPath(..)
  ) where

import Control.Monad.Trans.Reader  (ReaderT(..))
import Control.Monad               (mplus)
import Data.Foldable               (foldl')
import Webcrank.Dispatch.Path
import Webcrank.Dispatch.Route
import Webcrank.Dispatch.Types

dispatcher :: [req -> Maybe (m res)] -> req -> Maybe (m res)
dispatcher = runReaderT . r where
  r = foldl' ((. ReaderT) . mplus) (ReaderT $ const Nothing)

