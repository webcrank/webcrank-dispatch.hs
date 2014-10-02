
import Test.DocTest
main = doctest
  [ "-isrc"
  , "src/Webcrank/Dispatch.hs"
  , "src/Webcrank/Dispatch/Path.hs"
  , "src/Webcrank/Dispatch/Route.hs"
  , "src/Webcrank/Dispatch/Types.hs"
  ]
