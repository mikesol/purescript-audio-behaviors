{ name = "purescript-audio-behaviors"
, dependencies =
  [ "behaviors"
  , "canvas"
  , "console"
  , "debug"
  , "drawing"
  , "effect"
  , "foreign-object"
  , "heterogeneous"
  , "parseint"
  , "promises"
  , "psci-support"
  , "sized-vectors"
  , "typelevel-prelude"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs" ]
}
