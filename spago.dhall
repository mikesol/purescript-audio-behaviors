{ name = "purescript-audio-behaviors"
, dependencies =
  [ "aff-promise"
  , "behaviors"
  , "console"
  , "debug"
  , "effect"
  , "foreign-object"
  , "heterogeneous"
  , "parseint"
  , "psci-support"
  , "sized-vectors"
  , "typelevel-prelude"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs" ]
}
