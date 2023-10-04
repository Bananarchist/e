let 
  myNixPkgs = import <nixpkgs> {};
in
myNixPkgs.mkShell {
  nativeBuildInputs = with myNixPkgs; [
    cabal-install
    ghc
    haskellPackages.haskell-language-server
  ];
  shellHook = ''
  '';
}


