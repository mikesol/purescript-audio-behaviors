{ name = "purescript-audio-behaviors"
, dependencies =
  [ "aff-promise"
  , "behaviors"
  , "console"
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
