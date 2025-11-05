{
  description = "Pascal-like language compiler (Rust + Inkwell)";

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
            cargo
            rustc
            rust-analyzer
            llvm_18
            pkg-config
            gcc
            zlib
          ];

          shellHook = ''
            # LLVM environment variables for Inkwell
            export LLVM_SYS_180_PREFIX="${pkgs.llvm_18.dev}"
            export LIBCLANG_PATH="${pkgs.llvm_18.lib}/lib"

            # Set up library paths for runtime
            export LD_LIBRARY_PATH="${pkgs.lib.makeLibraryPath [
              pkgs.zlib
              pkgs.llvm_18.lib
            ]}:$LD_LIBRARY_PATH"

            echo "Rust development environment loaded"
            echo "Rust version: $(rustc --version)"
            echo "Cargo version: $(cargo --version)"
            echo "LLVM version: $(llvm-config --version)"
          '';
        };
      });
}
