let shared = ../config.dhall
      { name = "shared"
      , synopsis = "shared types and apis" 
      }
in
  shared //
    { dependencies =
        [ "base"
        , "pure"
        , "pure-elm"
        , "pure-conjurer"
        , "pure-json"
        , "pure-render"
        , "pure-sorcerer"
        , "pure-transform"
        , "pure-txt"
        , "pure-websocket"
        , "hashable"
        ]
    , library = 
        { source-dirs = ["src"]
        , other-modules = [] : List Text
        }
    }
