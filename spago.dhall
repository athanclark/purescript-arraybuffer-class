{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "arraybuffer-class"
, dependencies =
  [ "arraybuffer"
  , "exceptions"
  , "foreign-object"
  , "ordered-collections"
  , "sized-vectors"
  , "strings"
  , "unordered-collections"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs" ]
, license = "BSD-3-Clause"
, repository = "https://github.com/athanclark/purescript-arraybuffer-class.git"
}
