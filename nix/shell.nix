{ pkgs }:
pkgs.mkShell {
  buildInputs = with pkgs; [ 
    cachix
    postgresql

    haskell.compiler.ghc922

    haskellPackages.cabal-install
    haskellPackages.implicit-hie
    haskellPackages.fourmolu
    haskellPackages.haskell-language-server
    haskellPackages.cabal2nix
    haskellPackages.cabal-fmt
  ];
}

