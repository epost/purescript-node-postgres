{ name = "purescript-node-postgres"
, dependencies =
  [ "aff"
  , "arrays"
  , "bifunctors"
  , "console"
  , "datetime"
  , "effect"
  , "either"
  , "enums"
  , "exceptions"
  , "foldable-traversable"
  , "foreign"
  , "integers"
  , "js-date"
  , "maybe"
  , "nullable"
  , "prelude"
  , "psci-support"
  , "simple-json"
  , "spec"
  , "transformers"
  , "unsafe-coerce"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
