{ name = "purescript-audio-behaviors"
, dependencies =
  [ "aff-promise"
  , "behaviors"
  , "canvas"
  , "console"
  , "debug"
  , "drawing"
  , "effect"
  , "foreign-object"
  , "heterogeneous"
  , "parseint"
  , "psci-support"
  , "sized-vectors"
  , "typelevel-graph"
  , "typelevel-prelude"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs" ]
}
