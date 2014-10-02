
import Test.DocTest
main = doctest
  [ "-isrc"
  , "src/Web/Crank/Dispatch.hs"
  , "src/Web/Crank/Dispatch/Path.hs"
  , "src/Web/Crank/Dispatch/Route.hs"
  , "src/Web/Crank/Dispatch/Types.hs"
  ]
