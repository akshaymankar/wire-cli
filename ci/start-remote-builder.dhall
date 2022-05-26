let Concourse = ./deps/concourse.dhall

let Prelude = ./deps/prelude.dhall

let runTestsWith
    : Concourse.Types.Resource -> Concourse.Types.Step
    = \(repo : Concourse.Types.Resource) ->
        Concourse.helpers.taskStep
          Concourse.schemas.TaskStep::{
          , task = "start-remote-builder"
          , timeout = Some "2m"
          , config =
              Concourse.Types.TaskSpec.Config
                Concourse.schemas.TaskConfig::{
                , image_resource = Some Concourse.schemas.ImageResource::{
                  , type = "registry-image"
                  , source = Some
                      ( toMap
                          { repository = Prelude.JSON.string "nixos/nix"
                          , tag = Prelude.JSON.string "latest"
                          }
                      )
                  }
                , inputs = Some
                  [ Concourse.schemas.TaskInput::{ name = repo.name } ]
                , params = Some
                    ( toMap
                        { CACHIX_AUTH_TOKEN = Some "((cachix-token))"
                        , SCW_ACCESS_KEY = Some "((nix-builders-access-key))"
                        , SCW_SECRET_KEY = Some "((nix-builders-secret-key))"
                        , SCW_DEFAULT_PROJECT_ID = Some
                            "((nix-builders-project-id))"
                        }
                    )
                , run = Concourse.schemas.TaskRunConfig::{
                  , path = "sh"
                  , args = Some
                    [ "-c"
                    , ./start-remote-builder.sh as Text
                    , let dollarZero = "start-remote-builder.sh" in dollarZero
                    , repo.name
                    , "((nix-builder-1-id))"
                    , "((nix-builder-1-zone))"
                    , "((nix-builder-ssh-key))"
                    ]
                  }
                }
          }

in  runTestsWith
