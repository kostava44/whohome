{
  inputs.nixpkgs.url = "github:nixos/nixpkgs";
  inputs.flake-utils.url = "github:numtide/flake-utils";

  outputs = { self, nixpkgs, flake-utils }:
    {
      overlay = (final: prev: {
        whohome = final.haskell.packages.ghc94.callCabal2nix "whohome" ./. { };
      });
      haskellOverrides = (self0: super0: {
        haskell = super0.haskell // {
          packageOverrides = self: super: {
            libtelnet = self0.haskell.lib.doJailbreak super.libtelnet;
          };
        };
      });
    } // flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs {
          inherit system;
          overlays = [ self.overlay self.haskellOverrides ];
        };
      in {
        packages = { whohome = pkgs.whohome; };
        defaultPackage = self.packages.${system}.whohome;
        devShell = let haskellPackages = pkgs.haskell.packages.ghc94;
        in haskellPackages.shellFor {
          packages = p: [ self.packages.${system}.whohome ];
          withHoogle = true;
          buildInputs = (with pkgs; [ ghcid cabal-install nixfmt ormolu hlint ])
            ++ (with haskellPackages; [ haskell-language-server ]);
        };
      });
}
