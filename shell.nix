let
  pkgs = import (fetchTarball "https://github.com/NixOS/nixpkgs/archive/nixpkgs-unstable.tar.gz") {};
in
pkgs.mkShell {
  buildInputs = [
    pkgs.cabal-install
    pkgs.ghc
  ];
}
