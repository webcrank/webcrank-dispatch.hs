[Travis](https://travis-ci.org/webcrank/webcrank-dispatch.hs) [![Build Status](https://travis-ci.org/webcrank/webcrank-dispatch.hs.png)](https://travis-ci.org/webcrank/webcrank-dispatch.hs)

A type-safe request dispatcher and path renderer.  Based on [reroute](https://hackage.haskell.org/package/reroute).

```
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

import Webcrank.Dispatch
```

Given the following paths

```
-- a path that doesn't have any path parameters
index :: Path '[]
index = ""

-- a path that has a path parameter that matches any string
echo :: Path '[String]
echo = "echo" </> param
```

We can render them using

```
renderedIndex = renderPath index params 
-- [""]

renderedEcho = renderPath echo $ params "Hello"
-- ["echo", "Hello"]
```

And we can use them to create a dispatcher for request routing

```
dispatcher = dispatch $ mconcat
  [ index ==> "Welcome!"
  , echo ==> \a -> mconcat [ "*", a, "*" ]
  ]
```

then dispatch on request paths

```
res1 = dispatcher [""] 
-- Just "Welcome!"

res2 = dispatcher ["echo", "Thanks for coming!"]
-- Just "*Thanks for coming!*"

notFound = dispatcher ["something", "else"]
-- Nothing
```

For more examples see `examples/Main.hs`.
