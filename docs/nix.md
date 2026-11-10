# Nix packaging

`haskell-git` ships a Nix flake that provides the library + CLI as a package, a
development shell, an overlay, and NixOS / Home Manager modules.

Everything is defined in [`flake.nix`](../flake.nix); `flake.lock` pins nixpkgs.
The flake supports `x86_64-linux` and `aarch64-linux`.

## Requirements

A Nix with flakes enabled (Nix 2.18+ or Determinate Nix). Nothing else is
required: the package is built from this repository's cabal file with
`haskellPackages.callCabal2nix`, so binary caches from `cache.nixos.org` are
used for GHC and the Haskell dependencies.

## Quick start

Run the CLI without installing it:

```console
# from a checkout of this repository
$ nix run . -- --help

# or directly from the repository (after it is pushed)
$ nix run github:meillaya/haskell-git -- --help
```

Build it and run the binary:

```console
$ nix build .#default
$ ./result/bin/haskell-git --help
$ ./result/bin/haskell-git clone https://github.com/NixOS/nixpkgs
```

Install it into your profile (it then behaves like any other `git`-style CLI):

```console
$ nix profile install .#default
# Newer Nix versions spell this `nix profile add .#default`.
$ haskell-git --help
```

## Flake outputs

| Output | Description |
| --- | --- |
| `packages.<system>.default`, `packages.<system>.haskell-git` | The CLI/library (`haskellPackages.callCabal2nix "haskell-git" self { }`). |
| `overlays.default` | Adds `pkgs.haskell-git` and `pkgs.haskellPackages.haskell-git`. |
| `nixosModules.default`, `nixosModules.haskell-git` | NixOS module with `programs.haskell-git.{enable,package}`. |
| `homeModules.default`, `homeModules.haskell-git` | Home Manager module with `programs.haskell-git.{enable,package}`. |
| `devShells.<system>.default` | Development shell equivalent to `devenv.nix`. |
| `checks.<system>.default` | The package, so `nix flake check` builds it. |
| `formatter.<system>` | `nixfmt` for `nix fmt`. |

## Development shell

The dev shell is the flake equivalent of `devenv.nix`:

```console
$ nix develop
haskell-git dev shell: ghc 9.10.3, cabal 3.16.1.0
$ cabal build
$ cabal test
```

It provides GHC and the project's Haskell dependencies, plus
`cabal-install`, `git`, `python3`, `time`, `openssl`, `zlib` and `pkg-config`.
`PKG_CONFIG_PATH` is pointed at `zlib.dev`'s `share/pkgconfig` and
`LD_LIBRARY_PATH` contains `zlib`/`openssl`, because the `zlib` Haskell package
probes the C library through pkg-config and loads it at build time. `git`,
`python3` and `time` are needed by the test suite (`cabal test`).

`devenv.nix` is unchanged, so `devenv shell` / direnv keep working; the flake
dev shell is for people who do not use devenv.

## Overlay

`overlays.default` makes the package available inside a nixpkgs instance:

```nix
{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    haskell-git.url = "github:meillaya/haskell-git";
  };

  outputs = { self, nixpkgs, haskell-git }: {
    packages.x86_64-linux.default =
      (import nixpkgs {
        system = "x86_64-linux";
        overlays = [ haskell-git.overlays.default ];
      }).haskell-git;
  };
}
```

The overlay adds both `pkgs.haskell-git` (convenience) and
`pkgs.haskellPackages.haskell-git` (usable from Haskell package sets).

## NixOS module

Add the flake input and import the module in the flake that defines the
system:

```nix
{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    haskell-git.url = "github:meillaya/haskell-git";
  };

  outputs = { self, nixpkgs, haskell-git }: {
    nixosConfigurations.my-machine = nixpkgs.lib.nixosSystem {
      system = "x86_64-linux";
      modules = [
        haskell-git.nixosModules.default
        {
          programs.haskell-git.enable = true;
        }
      ];
    };
  };
}
```

If your `configuration.nix` receives the flake inputs (for example through
`specialArgs = { inherit inputs; }`), the equivalent is:

```nix
{ inputs, ... }:
{
  imports = [ inputs.haskell-git.nixosModules.default ];

  programs.haskell-git.enable = true;
}
```

The module has two options:

- `programs.haskell-git.enable` – boolean, default `false`.
- `programs.haskell-git.package` – package, default
  `haskell-git.packages.<system>.default` (the package built by this flake).

When enabled, the chosen package is appended to
`environment.systemPackages`; no services, daemons, or config files are
managed. To use a package from your own nixpkgs (for example built with the
overlay), override it:

```nix
{ pkgs, inputs, ... }:
{
  imports = [ inputs.haskell-git.nixosModules.default ];

  programs.haskell-git = {
    enable = true;
    package = pkgs.haskell-git; # or inputs.haskell-git.packages.${pkgs.stdenv.hostPlatform.system}.default
  };
}
```

## Home Manager module

The flake also exposes a plain Home Manager module (it only sets
`home.packages`, so it needs home-manager in the consumer; the flake does not
take a home-manager input):

```nix
{ inputs, ... }:
{
  imports = [ inputs.haskell-git.homeModules.default ];

  programs.haskell-git.enable = true;
}
```

## SSH remotes and keys

`haskell-git` does not implement SSH itself: for `ssh://` and scp-style remotes
it starts the system OpenSSH client, using the same conventions as git. It
honours `GIT_SSH_COMMAND`, then `GIT_SSH`, then `core.sshCommand` from the git
config, and falls back to `ssh` from `PATH`.

That means your personal SSH setup is used exactly as it is for git:

- keys in `~/.ssh/` (e.g. `~/.ssh/id_ed25519`) and `~/.ssh/config` host blocks,
- a running `ssh-agent` (`ssh-add ~/.ssh/id_ed25519`, `SSH_AUTH_SOCK`),
- hardware keys/`ssh -Y`/jump hosts configured through `~/.ssh/config`.

No Nix-side secret handling (agenix, sops, `ssh-agent` services, …) is needed
or done by this flake or its modules. On NixOS the OpenSSH client is part of
the default system path; if your configuration removes it, add it yourself:

```nix
environment.systemPackages = [ pkgs.openssh ];
```

The Nix package does not wrap `ssh`, so it keeps working with your existing
dotfiles and git configuration. HTTP(S) credentials likewise come from the
usual git configuration (for example `credential.helper`); the flake never
touches them.

## Flake maintenance

- Update the pinned nixpkgs: `nix flake update`.
- The package version comes from `haskell-git.cabal`; `callCabal2nix` picks up a
  new version or dependency bounds automatically, no flake edit needed.
- `nix flake check` builds `checks.<system>.default` (the package). The
  derivation sets `doCheck = false`, so the test suite is not built or run by
  Nix: it spawns the `git` CLI and Python HTTP fixture servers, so it depends
  on the environment rather than on the build sandbox. Run `cabal test` inside
  `nix develop` (or rely on CI) for the test suite.
