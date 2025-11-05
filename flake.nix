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
          ];

          shellHook = ''
            echo "Pascal-like compiler development environment"
            echo "OCaml version: $(ocaml -version)"
            echo "LLVM version: $(llvm-config --version)"
          '';
        };
      });
}
