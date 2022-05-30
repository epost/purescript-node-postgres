let conf = ./spago.dhall

in      conf
    //  { sources = conf.sources # [ "test/**/*.purs" ]
        , dependencies =
              conf.dependencies
            # [ "spec"
              , "argonaut"
              , "bifunctors"
              , "js-date"
              , "exceptions"
              ]
        }
