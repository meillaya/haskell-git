{
  description = "haskell-git: a Git implementation in Haskell (library + `haskell-git` CLI)";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
  };

  outputs =
    { self, nixpkgs }:
    let
      systems = [
        "x86_64-linux"
        "aarch64-linux"
      ];
      forAllSystems = nixpkgs.lib.genAttrs systems;

      # The package is defined exactly once and shared by `packages`,
      # `overlays.default`, `checks` and the modules.
      #
      # The test suite is intentionally not built/run by the derivation
      # (`doCheck = false`): it spawns the `git` CLI and Python HTTP fixture
      # servers, which are not available in the Nix build sandbox, and it is
      # covered by CI and by `cabal test` in `nix develop` instead.
      packageFor =
        system:
        let
          pkgs = nixpkgs.legacyPackages.${system};
        in
        pkgs.haskell.lib.overrideCabal (pkgs.haskellPackages.callCabal2nix "haskell-git" self { }) (drv: {
          doCheck = false;
        });

      # A nixpkgs instance with our overlay applied, for the dev shell and for
      # anything else that needs the package inside a Haskell package set.
      pkgsFor =
        system:
        import nixpkgs {
          inherit system;
          overlays = [ self.overlays.default ];
        };
    in
    {
      ##########################################################################
      # Packages
      ##########################################################################

      packages = forAllSystems (system: {
        default = packageFor system;
        haskell-git = packageFor system;
      });

      ##########################################################################
      # Overlay
      ##########################################################################

      # `nixpkgs.overlays = [ haskell-git.overlays.default ];` gives you both
      # `pkgs.haskell-git` and `pkgs.haskellPackages.haskell-git`.
      overlays.default = final: prev: {
        haskell-git = final.haskellPackages.haskell-git;
        haskellPackages = prev.haskellPackages.extend (
          hfinal: _hprev: {
            haskell-git = prev.haskell.lib.overrideCabal (hfinal.callCabal2nix "haskell-git" self { }) (drv: {
              doCheck = false;
            });
          }
        );
      };

      ##########################################################################
      # Development shell (mirrors devenv.nix)
      ##########################################################################

      devShells = forAllSystems (
        system:
        let
          pkgs = pkgsFor system;
          hs = pkgs.haskellPackages;
        in
        {
          default = hs.shellFor {
            packages = ps: [ ps.haskell-git ];

            nativeBuildInputs = with pkgs; [
              cabal-install
              ghc
              pkg-config
            ];

            # Test-time tools: `cabal test` needs git, python3 and time; the
            # C libraries are needed by the zlib package and http-client-tls.
            buildInputs = with pkgs; [
              git
              openssl
              python3
              time
              zlib
            ];

            shellHook = ''
              # The Haskell zlib package asks pkg-config for the C library.
              # nixpkgs installs zlib.pc under share/pkgconfig, which is not on
              # the default search path.
              export PKG_CONFIG_PATH="${pkgs.zlib.dev}/share/pkgconfig''${PKG_CONFIG_PATH:+:$PKG_CONFIG_PATH}"
              # cabal runs a helper binary during the zlib build that loads the
              # C library at run time.
              export LD_LIBRARY_PATH="${
                pkgs.lib.makeLibraryPath [
                  pkgs.zlib
                  pkgs.openssl
                ]
              }''${LD_LIBRARY_PATH:+:$LD_LIBRARY_PATH}"

              printf 'haskell-git dev shell: ghc %s, cabal %s\n' \
                "$(ghc --version | cut -d' ' -f8)" \
                "$(cabal --version | head -n 1 | cut -d' ' -f3)"
            '';
          };
        }
      );

      ##########################################################################
      # Checks: `nix flake check` builds the package.
      ##########################################################################

      checks = forAllSystems (system: {
        default = self.packages.${system}.default;
      });

      ##########################################################################
      # Formatter
      ##########################################################################

      formatter = forAllSystems (system: nixpkgs.legacyPackages.${system}.nixfmt);

      ##########################################################################
      # NixOS module
      ##########################################################################

      nixosModules = {
        default =
          {
            config,
            lib,
            pkgs,
            ...
          }:
          let
            cfg = config.programs.haskell-git;
            system = pkgs.stdenv.hostPlatform.system;
          in
          {
            options.programs.haskell-git = {
              enable = lib.mkEnableOption "haskell-git, a Git implementation in Haskell";

              package = lib.mkOption {
                type = lib.types.package;
                default = self.packages.${system}.default;
                defaultText = lib.literalExpression "haskell-git.packages.\${system}.default";
                description = ''
                  The haskell-git package to install. Defaults to the package
                  built by this flake; override it to use, for example,
                  `pkgs.haskellPackages.haskell-git` from your own nixpkgs with
                  the overlay applied.
                '';
              };
            };

            config = lib.mkIf cfg.enable {
              environment.systemPackages = [ cfg.package ];
            };
          };

        # Alias, so both `nixosModules.default` and `nixosModules.haskell-git`
        # work.
        haskell-git = self.nixosModules.default;
      };

      ##########################################################################
      # Home Manager module (optional; requires home-manager in the consumer)
      ##########################################################################

      homeModules = {
        default =
          {
            config,
            lib,
            pkgs,
            ...
          }:
          let
            cfg = config.programs.haskell-git;
            system = pkgs.stdenv.hostPlatform.system;
          in
          {
            options.programs.haskell-git = {
              enable = lib.mkEnableOption "haskell-git, a Git implementation in Haskell";

              package = lib.mkOption {
                type = lib.types.package;
                default = self.packages.${system}.default;
                defaultText = lib.literalExpression "haskell-git.packages.\${system}.default";
                description = "The haskell-git package to install.";
              };
            };

            config = lib.mkIf cfg.enable {
              home.packages = [ cfg.package ];
            };
          };

        haskell-git = self.homeModules.default;
      };
    };
}
