{
  description = "Ithkuil V4 grammar toolkit";

  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";

  outputs = { self, nixpkgs }:
    let
      systems = [ "x86_64-linux" "aarch64-linux" "x86_64-darwin" "aarch64-darwin" ];
      forAllSystems = f: nixpkgs.lib.genAttrs systems (system: f nixpkgs.legacyPackages.${system});
    in
    {
      devShells = forAllSystems (pkgs: {
        default = pkgs.mkShell {
          packages = [
            pkgs.go
            pkgs.python3   # tools/build_db.py, tools/sync_lexicon.py
            pkgs.curl
            # Compressors, for measuring how well a serialization
            # format holds up under a general-purpose compressor.
            pkgs.gzip
            pkgs.zstd
            pkgs.xz
            # tools/build_wasm.sh. TinyGo compiles the browser build
            # (a third of what the standard toolchain emits), wasm-opt
            # takes another 3% off it, and brotli is what the wire size
            # is actually measured in.
            pkgs.tinygo
            pkgs.binaryen
            pkgs.brotli
          ];
          CGO_ENABLED = "0";
        };
      });
    };
}
