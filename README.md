# Haskell Client for Wire

[![CI](https://concourse.gdn/api/v1/teams/main/pipelines/wire-cli/jobs/main/badge)](https://concourse.gdn/teams/main/pipelines/wire-cli)

This is not just a CLI as the name of the repository suggest. However, the
project had started as a CLI.

**WARNING:** This is an unofficial client of Wire, please use it only with a
server which allows unofficial clients (or this client in particular) in its
terms and conditions.

## Goals

1. Provide an Electron free way to communicate using Wire on a desktop.
2. Provide an automation friendly way to use the Wire backend as a "user" (not
   "bot"/"service").
4. Provide a truly configurable client, so the user can be in control of where
   and how to store the data.
3. Discover problems with libraries/tooling in the Haskell eco-system and fix
   them.

## Railmap/Status

| Feature                                          | wire-cli           | wire-gui           |
|--------------------------------------------------|--------------------|--------------------|
| Registration                                     | :heavy_check_mark: | :x:                |
| Search another user                              | :heavy_check_mark: | :x:                |
| Connections                                      | :heavy_check_mark: | :x:                |
| Create conversations                             | :heavy_check_mark: | :x:                |
| Discover conversations                           | :heavy_check_mark: | :heavy_check_mark: |
| Manage conversations                             | :x:                | :x:                |
| Send text message                                | :heavy_check_mark: | :heavy_check_mark: |
| Receive text message                             | :heavy_check_mark: | :heavy_check_mark: |
| E2EE message exchange (Proteus)                  | :heavy_check_mark: | :heavy_check_mark: |
| Proteus session reset                            | :x:                | :x:                |
| E2EE messaging (MLS)                             | :x:                | :x:                |
| Mark as read/unread                              | :x:                | :x:                |
| Read reciepts                                    | :x:                | :x:                |
| Delivery receipts                                | :x:                | :x:                |
| Send/Recieve Assets                              | :x:                | :x:                |
| Voice Mesages                                    | :x:                | :x:                |
| Notifications                                    | :heavy_check_mark: | :x:                |
| Update profile data                              | :x:                | :x:                |
| Calling                                          | :x:                | :x:                |
| Storage: Directory full of JSON Files            | :heavy_check_mark: | :heavy_check_mark: |
| Storage: Sqlite                                  | :x:                | :x:                |
| Storage: Remote RDBMS (mysql/postgres)           | :x:                | :x:                |
| Storage: encryption at rest                      | :x:                | :x:                |
| Backup/Restore                                   | :x:                | :x:                |
| Restore from Android/iOS/Web backups             | :x:                | :x:                |
| Search local messages                            | :x:                | :x:                |
| Tag/tokenize images/videos/pdfs/links for search | :x:                | :x:                |
| Logging                                          | :x:                | :x:                |
| Legalhold                                        | :x:                | :x:                |
| Login with SSO                                   | :x:                | :x:                |
| Team management                                  | :x:                | :x:                |

## Contributing

### Development setup

#### Pre-requisites

1. nix-flakes
2. direnv (optional)

#### How to ...

**NOTE** All commands are to be run from root of this repo.

1. How to get a development shell with direnv?

   ```bash
   direnv allow
   ```

2. How to get a development shell without direnv?

    ```bash
    nix develop
    ```

3. How to run unit tests from development shell?

    ```bash
    make test-unit
    ```

4. How to run integration tests from development shell?

   When targetting "demo" setup defined in the wire-server repository:

   ```bash
   make test-int-demo demo_host=<ip_addr>
   ```

   The `demo_host` parameter defaults to `127.0.0.1` if not specified.

   When targetting a deployed wire-server, the tests can use the "backdoor"
   basic auth to talk to nginz. This requires the nginz helm chart deployed with
   `nginx_conf.env` set to `staging`. Once such an environment is available, the
   tests can be run like this:

   ```bash
   make test-int-kube \
       kube_nginz_host=nginz-https.<domain> \
       kube_nginz_port=443 \
       kube_backdoor_nginz_user=<name> \
       kube_backdoor_nginz_password=<password>
   ```

5. How to run haskell-language-server (HLS) so it uses dev-shell?

   To ensure that haskell-language-server executes in development environment
   [`lsp-wrapper.sh`](./lsp-warpper.sh) can be used. The script uses `direnv
   exec` to ensure that HLS runs inside the development environment. Without
   `direnv` this can be done using `nix develop`, but at this point it is not
   very clear how to do this.

   With emacs and direnv, this `.dir-local.el` file works:
   ```el
   ((haskell-mode . ((lsp-haskell-server-path . "/<path-to-wire-cli-repo>/lsp-wrapper.sh"))))
   ```

6. How to pin a haskell package to a particular hackage version?

   Add something like this to `hackagePins` array in
   [`./pins.yaml`](./pins.yaml):

   ```yaml
   - package: <package-name>
     version: <version>
   ```

7. How to pin a haskell package to a particular commit in git?

   Add something like this to `gitPins` array in [`./pins.yaml`](./pins.yaml):

   ```yaml
   - location: <https://server/owner/repo>
     commit: <sha>

     # Only required if the cabal file is not at the root of repo
     subdirs:
     - <some-subdir>
     - <another-subdir>
   ```

8. How to format code?

   Using [`ormolu`](https://github.com/tweag/ormolu/).
