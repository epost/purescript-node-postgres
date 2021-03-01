{ name = "purescript-node-postgres"
, dependencies =
  [ "console", "effect", "js-date", "psci-support", "simple-json", "spec" ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
