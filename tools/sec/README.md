# sec

From a trusted project with a `secretspec.toml` and a `default` profile:

    sec just dev

Run from the project root or a subdirectory. `sec` finds the manifest before
contacting Bitwarden and does not climb past a Git checkout boundary. The
command runs in your original directory with its original arguments and PATH.
Sign-in is opt-in if needed. Each invocation acquires its own unlock session;
the session is not cached or passed to the launched command. Missing required
values stop execution; use `secretspec set` separately to provision them.

Only use this launcher with manifests and commands you trust to access real
values. This is not an isolation boundary for untrusted code.

## Install

The flake exports `packages.<system>.sec` and `homeManagerModules.sec`.
For a self-hosted vault, configure the Bitwarden CLI for the same server once
with `bw config server https://vault.example.com` before signing in. The
launcher refuses to switch servers. Hosted Bitwarden is the default.

In Home Manager, import the module and use:

    programs.sec = {
      enable = true;
      vaultwardenUrl = "https://vault.example.com";
      # Set bwPackage and secretspecPackage to reuse host pins if needed.
    };

With the standalone package, set `SEC_VAULTWARDEN_URL` for a self-hosted server.
Neither installation nor activation logs in or unlocks the vault. Install this
on the human account only; agent runtimes should use mocks.
