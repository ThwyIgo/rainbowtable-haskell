{
  description = "Ambiente de desenvolvimento Haskell com VSCodium e Cabal";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};

        haskellPkgs = pkgs.haskellPackages;

        haskellTools = [
          haskellPkgs.ghc
          haskellPkgs.cabal-install
          haskellPkgs.haskell-language-server
          haskellPkgs.fourmolu
          pkgs.ghcid
        ];

        vscodeExtensions = with pkgs.vscode-extensions; [
          haskell.haskell
          justusadam.language-haskell
        ];

        codiumComExtensoes = pkgs.vscode-with-extensions.override {
          vscode = pkgs.vscodium;
          vscodeExtensions = vscodeExtensions;
        };

      in
      {
        devShells.default = pkgs.mkShell {
          buildInputs = haskellTools ++ [ codiumComExtensoes ];
        };
      }
    );
}
