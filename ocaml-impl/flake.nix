{
  description = "Pascal-like language compiler with LLVM";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
        ocamlPackages = pkgs.ocaml-ng.ocamlPackages_5_2;
      in
      {
        devShells.default = pkgs.mkShell {
          buildInputs = with pkgs; [
            ocamlPackages.ocaml
            ocamlPackages.dune_3
            ocamlPackages.findlib
            ocamlPackages.menhir
            ocamlPackages.ocaml-lsp
            ocamlPackages.ocamlformat
            llvm_18
            ocamlPackages.llvm

            # Runtime and linking dependencies
            zlib
            libffi
            libxml2
            ncurses
            gcc
          ];

          shellHook = ''
            # Set up library paths for runtime and linking
            export LD_LIBRARY_PATH="${pkgs.lib.makeLibraryPath [
              pkgs.zlib
              pkgs.llvm_18.lib
              pkgs.libffi
              pkgs.libxml2
              pkgs.ncurses
            ]}:$LD_LIBRARY_PATH"

            echo "Pascal-like compiler development environment"
            echo "OCaml version: $(ocaml -version)"
            echo "LLVM version: $(llvm-config --version)"
          '';
        };
      });
}
