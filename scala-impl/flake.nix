{
  description = "Pascal-like compiler - Scala implementation";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
      in
      {
        devShells.default = pkgs.mkShell {
          buildInputs = with pkgs; [
            # Scala toolchain
            sbt
            scala_3
            jdk21

            # LLVM for JavaCPP native libraries (use llvm for latest version)
            llvm

            # Runtime and linking dependencies
            zlib
            libffi
            libxml2
            ncurses
            gcc
          ];

          shellHook = ''
            # Set up library paths for LLVM native libraries
            export LD_LIBRARY_PATH="${pkgs.lib.makeLibraryPath [
              pkgs.zlib
              pkgs.llvm.lib
              pkgs.libffi
              pkgs.libxml2
              pkgs.ncurses
            ]}:$LD_LIBRARY_PATH"

            echo "Pascal-like compiler development environment (Scala)"
            echo "Scala version: $(scala -version 2>&1 | head -1)"
            echo "SBT version: $(sbt --version | grep 'sbt version')"
            echo "LLVM version: $(llvm-config --version)"
          '';
        };
      });
}
