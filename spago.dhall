{ name = "purescript-node-postgres"
, dependencies =
  [ "aff"
  , "arrays"
  , "datetime"
  , "effect"
  , "either"
  , "enums"
  , "foldable-traversable"
  , "foreign"
  , "integers"
  , "maybe"
  , "nullable"
  , "prelude"
  , "transformers"
  , "unsafe-coerce"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs" ]
}
