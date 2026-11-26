# agent-workshop
Harness-independent skills, Haskell tools, and NixOS modules for agent workflows.

- [`agent-page`](tools/agent-page/README.md): build a self-contained HTML page that presents screenshots and video, asks questions, and turns the answers into text to paste back.
- [`sec`](tools/sec/README.md): run a project command with SecretSpec values from an interactive Bitwarden session.
- [`abrec`](tools/abrec/README.md): record yourself reading books aloud and publish the chapters as private podcast feeds, with a NixOS module (`nixosModules.abrec`).
- [`check-index`](tools/check-index/src/Workshop/CheckIndex.hs): validate the staged Git snapshot with `nix flake check`.

## Haskell layout

Every package is listed in `cabal.project`, and every tool has the same shape:

    tools/<name>/
      <name>.cabal   cabal.nix   package.nix   README.md
      app/Main.hs              # calls Workshop.<Name>.main, nothing else
      src/Workshop/<Name>/…    # the tool's logic
      test/                    # its test suite, run when the package builds

- `cabal.nix` is `cabal2nix . | nixfmt - > cabal.nix` output; `checks.cabal-nix`
  fails when it is stale.
- `package.nix` calls `wrapTool` from `nix/workshop.nix`, which hands each
  runtime program to the tool as an absolute path in an environment variable.
  Tools read these with `Workshop.Cli.Env.configuredExecutable`, never PATH.
- A test suite's extra programs and arguments go in `testSetup` in
  `nix/workshop.nix`.
- `lib/workshop-cli` and `lib/workshop-test` hold code that at least two tools
  use. Code moves there when a second tool needs it, not before.

`nix develop` provides GHC with every dependency, cabal, cabal2nix, hlint and
nixfmt. `nix flake check` builds and tests every tool and runs formatting,
hlint and the cabal2nix check.
