{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings #-}
module Webcrank.Dispatch.Examples where

import           Control.Monad.Identity
import           Data.Functor                        ((<$>))
import           Data.Text                           (Text)
import           Webcrank.Dispatch

data Req = Req 
  { path :: [Text]
  } deriving (Eq, Show)

instance HasRequestPath Req where
  rqPath (Req p) = p

type Res = [[Text]]

packages      = lit "packages"
package       = packages </> textParam 
packageVer    = package </> intParam
packageVerDoc = packageVer </> lit "doc"

packagesSegs       = injectPath packages -- ["packages"]
packageSegs        = injectPath package "webcrank-dispatch" -- ["packages", "webcrank-dispatch"]
packageVerSegs     = injectPath packageVer "webcrank-dispatch" 1 -- ["packages", "webcrank-dispatch", "1"]
packageVerDocSegs  = injectPath packageVerDoc "webcrank-dispatch" 1 -- ["packages", "webcrank-dispatch", "1", "doc"]

pn = extractPath packageSegs package id -- Just "webcrank-dispatch"
pv = extractPath packageVerSegs packageVer (,) -- Just ("webcrank-dispatch", 1)

packagesResource _ = Just [injectPath package "webcrank-dispatch"] 

packageResource "webcrank-dispatch" _ = Just vs where
  vs = [ injectPath packageVer "webcrank-dispatch" 1
       , injectPath packageVer "webcrank-dispatch" 2
       , injectPath packageVer "webcrank-dispatch" 3
       ] 
packageResource _ _ = Nothing


packageVerResource "webcrank-dispatch" 1 _ = Just [ injectPath packageVerDoc "webcrank-dispatch" 1 ]
packageVerResource "webcrank-dispatch" 2 _ = Just [ injectPath packageVerDoc "webcrank-dispatch" 2 ]
packageVerResource "webcrank-dispatch" 3 _ = Just [ injectPath packageVerDoc "webcrank-dispatch" 3 ]
packageVerResource _ _ _ = Nothing

disp = dispatcher 
  [ packages      --> packagesResource
  , package       --> packageResource
  , packageVer    --> packageVerResource
  , packageVerDoc --> (\_ _ _ -> Just [])
  ] 

ex1a = disp (Req ["packages"]) -- Just (Just [...])
ex1b = disp (Req ["unknown"]) -- Nothing

ex2a = disp (Req ["packages", "webcrank-dispatch"]) -- Just (Just [...])
ex2b = disp (Req ["packages", "nothing"]) -- Just Nothing

ex3a = disp (Req ["packages", "webcrank-dispatch", "2"]) -- Just (Just [["packages","webcrank-dispatch","2","doc"]])
ex3b = disp (Req ["packages", "webcrank-dispatch", "5"])  -- Just Nothing
ex3c = disp (Req ["packages", "webcrank-dispatch", "not-an-int"]) -- Nothing

ex4a = disp (Req ["packages", "webcrank-dispatch", "2", "doc"]) -- Just (Just [])
ex4b = disp (Req ["packages", "webcrank-dispatch", "2", "unknown"]) -- Nothing

