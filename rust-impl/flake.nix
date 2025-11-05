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
        llvmPackages = pkgs.llvmPackages_18;

      in
      {
        devShells.default = pkgs.mkShell {
          buildInputs = with pkgs; [
            cargo
            rustc
            rust-analyzer
            clippy
            rustfmt

            # LLVM dependencies
            llvmPackages.llvm
            llvmPackages.libllvm

            # Build tools
            pkg-config
            gcc

            # Libraries
            zlib
            libxml2
          ];

          shellHook = ''
            # LLVM environment variables for Inkwell
            export LLVM_SYS_180_PREFIX="${llvmPackages.llvm.dev}"
            export LIBCLANG_PATH="${llvmPackages.libclang.lib}/lib"

            # Set up library paths for runtime
            export LD_LIBRARY_PATH="${pkgs.lib.makeLibraryPath [
              pkgs.zlib
              llvmPackages.libllvm
            ]}:$LD_LIBRARY_PATH"

            echo "Rust development environment loaded"
            echo "Rust version: $(rustc --version)"
            echo "Cargo version: $(cargo --version)"
            echo "LLVM version: $(llvm-config --version)"
          '';
        };

        packages.default = pkgs.rustPlatform.buildRustPackage {
          pname = "pascalc";
          version = "0.1.0";
          src = ./.;

          cargoLock = {
            lockFile = ./Cargo.lock;
          };

          nativeBuildInputs = with pkgs; [
            pkg-config
            llvmPackages.llvm
          ];

          buildInputs = with pkgs; [
            llvmPackages.libllvm
            zlib
            libxml2
          ];

          LLVM_SYS_180_PREFIX = "${llvmPackages.llvm.dev}";
          LIBCLANG_PATH = "${llvmPackages.libclang.lib}/lib";
        };
      }
    );
}
