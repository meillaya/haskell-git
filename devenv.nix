# The development shell for this repository. Everything needed to build, test,
# and check the project is declared here. There is no second provisioning path.
{ pkgs, ... }:
{
  languages.haskell = {
    enable = true;
    package = pkgs.ghc;
    cabal.enable = true;
    stack.enable = false;
    lsp.enable = true;
  };

  packages = with pkgs; [
    git
    pkg-config
    python3
    openssl
    zlib
  ];

  env = {
    # The zlib Haskell package asks pkg-config for the C library. nixpkgs
    # installs zlib.pc under share/pkgconfig, which is not on the default
    # search path.
    PKG_CONFIG_PATH = "${pkgs.zlib.dev}/share/pkgconfig";

    # cabal runs a helper binary during the zlib build that loads the C
    # library at run time.
    LD_LIBRARY_PATH = pkgs.lib.makeLibraryPath [ pkgs.zlib pkgs.openssl ];
  };

  scripts = {
    build.exec = "cabal build";
    unit-test.exec = "cabal test";
    check.exec = "cabal check && cabal test";
  };

  enterShell = ''
    printf 'haskell-git dev shell\n'
    printf 'ghc   %s\n' "$(ghc --version | cut -d' ' -f8)"
    printf 'cabal %s\n' "$(cabal --version | head -1 | cut -d' ' -f3)"
    printf '\n'
    printf 'build      cabal build\n'
    printf 'unit-test  cabal test\n'
    printf 'check      cabal check and cabal test\n'
  '';
}
