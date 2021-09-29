let backend = ../config.dhall
      { name = "backend"
      , synopsis = "backend server"
      }

let deps =
      [ "base"
      , "pure-admin"
      , "pure-conjurer"
      , "pure-json"
      , "pure-elm"
      , "pure-server"
      , "pure-tagsoup"
      , "pure-txt"
      , "pure-sorcerer"
      , "pure-websocket"
      , "shared"
      , "pandoc"
      , "yaml"
      ]

in
  backend //
    { dependencies = deps
    , executables =
        { backend =
          { source-dirs = [ "src" ]
          , main = "Main.hs"
          , dependencies = deps
          } 
        }
    }