{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "arraybuffer-class"
, dependencies =
  [ "arraybuffer"
  , "console"
  , "effect"
  , "exceptions"
  , "foreign-object"
  , "ordered-collections"
  , "prelude"
  , "psci-support"
  , "quickcheck"
  , "sized-vectors"
  , "strings"
  , "unordered-collections"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
