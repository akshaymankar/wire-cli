let Concourse = ./deps/concourse.dhall

let Prelude = ./deps/prelude.dhall

let Git = ./deps/git.dhall

let mainBranch =
      Concourse.schemas.Resource::{
      , name = "main"
      , type = Concourse.Types.ResourceType.InBuilt "git"
      , icon = Some "git"
      , source =
          Git.Source.render
            Git.Source::{ uri = "https://github.com/akshaymankar/wire-cli" }
      }

let prResource =
      Concourse.schemas.CustomResourceType::{
      , name = "github-pr"
      , type = "registry-image"
      , source = Some
          ( toMap
              { repository = Prelude.JSON.string "teliaoss/github-pr-resource" }
          )
      }

let pr =
      Concourse.schemas.Resource::{
      , name = "pr"
      , type = Concourse.Types.ResourceType.Custom prResource
      , icon = Some "source-pull"
      , source = Some
          ( toMap
              { repository = Prelude.JSON.string "akshaymankar/wire-cli"
              , access_token = Prelude.JSON.string "((github-access-token))"
              , labels = Prelude.JSON.array [ Prelude.JSON.string "ok-to-test" ]
              }
          )
      }

let runTestsWith = ./run-tests-with.dhall

let mainBranchJob =
      Concourse.schemas.Job::{
      , name = "main"
      , plan =
        [ Concourse.helpers.getStep
            Concourse.schemas.GetStep::{
            , resource = mainBranch
            , trigger = Some True
            }
        , runTestsWith mainBranch
        ]
      }

let markCheckPending =
      λ(check : Text) →
        Concourse.helpers.putStep
          Concourse.schemas.PutStep::{
          , put = Some "mark-pending-${check}"
          , resource = pr
          , inputs = Some [ pr.name ]
          , params = Some
              ( toMap
                  { path = Prelude.JSON.string pr.name
                  , status = Prelude.JSON.string "PENDING"
                  , context = Prelude.JSON.string check
                  , description =
                      Prelude.JSON.string "waiting for tests to finsih"
                  }
              )
          }

let markCheckSuccess =
      λ(check : Text) →
        Concourse.helpers.putStep
          Concourse.schemas.PutStep::{
          , put = Some "mark-success-${check}"
          , resource = pr
          , inputs = Some [ pr.name ]
          , params = Some
              ( toMap
                  { path = Prelude.JSON.string pr.name
                  , status = Prelude.JSON.string "SUCCESS"
                  , context = Prelude.JSON.string check
                  }
              )
          }

let markCheckFailure =
      λ(check : Text) →
        Concourse.helpers.putStep
          Concourse.schemas.PutStep::{
          , put = Some "mark-failure-${check}"
          , resource = pr
          , inputs = Some [ pr.name ]
          , params = Some
              ( toMap
                  { path = Prelude.JSON.string pr.name
                  , status = Prelude.JSON.string "FAILURE"
                  , context = Prelude.JSON.string check
                  }
              )
          }

let runPRTestsWithHooks =
      let testStep = runTestsWith pr

      in  Concourse.helpers.addHooks
            testStep
            Concourse.schemas.StepHooks::{
            , on_success = Some (markCheckSuccess "test")
            , on_failure = Some (markCheckFailure "test")
            }

let prJob =
      Concourse.schemas.Job::{
      , name = "pull-requests"
      , plan =
        [ Concourse.helpers.getStep
            Concourse.schemas.GetStep::{ resource = pr, trigger = Some True }
        , markCheckPending "test"
        , runPRTestsWithHooks
        ]
      }

in  Concourse.render.pipeline [ mainBranchJob, prJob ]
