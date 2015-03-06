[Travis](https://travis-ci.org/webcrank/webcrank-dispatch.hs) [![Build Status](https://travis-ci.org/webcrank/webcrank-dispatch.hs.png)](https://travis-ci.org/webcrank/webcrank-dispatch.hs)

A type-safe request dispatcher and path renderer.

```
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

import Webcrank.Dispatch
```

# Paths
## Building Paths

The simplest `Path` is `root`, which is equivalent to `/`.

Other routes can be built with `</>`:

```
docsPath = "package" \<\/> "webcrank-dispatch-0.1" \<\/> "docs"
```

Paths can contain parameters. To create a parameterized path, use
`param` as a path component:

```
docsPath :: Path '[String]
docsPath = "package" </> param </> "docs"
```

Paths can contain as many parameters of varying types as needed:

```
wat :: Path '[String, Int, Bool, Int, String]
wat :: "this" </> param </> param </> "crazyness" </> param </> "ends" </> param </> param
```

Path parameters can be of any type that have instances for `Typeable` and `PathPiece`.

## Rendering Paths

`Path`s can be rendered using `renderPath` and
`params`.

```
>>> renderPath root params
["/"]
```

```
>>> renderPath docsPath $ params "webcrank-dispatch-0.1"
["package", "webcrank-dispatch-0.1", "docs"]
```

```
>>> renderPath wat $ params "down is up" 42 False 7 "up is down"
["this", "down is up", "42", "crazyness", "False", "ends", "7", "up is down"]
```

Note in the last example that no encoding is done by @renderPath@.

# Dispatching

An elementary `Dispatcher` can be built using `==>`.

```
disp = root ==> \"Dispatched\"
```

`Dispatcher`s form a `Monoid`, so more interesting dispatchers can
be built with `<>` or `mconcat`.

```
disp = mconcat
  [ root ==> "Welcome!"
  , "echo" </> param ==> id
  ]
```

Dispatching requests is done with `dispatch`. It turns a
`Dispatcher` into a function from a list of decoded path components
to a possible handler.

```
>>> dispatch (root ==> "Welcome!") [""]
Just "Welcome!"
```

```
>>> dispatch (root ==> "Welcome!") ["echo", "Goodbye!"]
Nothing
```

```
>>> dispatch (root ==> "Welcome!" <> "echo" </> param ==> id) ["echo", "Goodbye!"]
Just "Goodbye!"
```

For more examples see `examples/Main.hs`.
