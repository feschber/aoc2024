with import <nixpkgs> {};
with pkgs;

let
  haskell-packages = hp: with hp; [ base cabal-install regex-tdfa ];
  ghc-with-pkgs = haskellPackages.ghcWithPackages haskell-packages;
in mkShell {
  buildInputs = [
    ghc-with-pkgs
  ];
}
