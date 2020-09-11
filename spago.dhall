{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "my-project"
, dependencies =
  [ "behaviors"
  , "console"
  , "effect"
  , "heterogeneous"
  , "psci-support"
  , "sized-vectors"
  , "typelevel-prelude"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs" ]
}
