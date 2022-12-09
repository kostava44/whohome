{
  inputs.nixpkgs.url = "github:nixos/nixpkgs";
  inputs.flake-utils.url = "github:numtide/flake-utils";
  inputs.pre-commit-hooks = {
    url = "github:cachix/pre-commit-hooks.nix";
    inputs.nixpkgs.follows = "nixpkgs";
  };

  outputs = { self, nixpkgs, flake-utils, pre-commit-hooks }:
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
      in
      {
        packages = { whohome = pkgs.whohome; };
        defaultPackage = self.packages.${system}.whohome;
        checks = {
          pre-commit-check = pre-commit-hooks.lib.${system}.run {
            src = ./.;
            hooks = {
              nixpkgs-fmt.enable = true;
              ormolu.enable = true;
              hlint.enable = true;
              cabal-fmt.enable = true;
            };
            settings = {
              ormolu.defaultExtensions = [ "ImportQualifiedPost" ];
            };
          };
        };
        devShell =
          let haskellPackages = pkgs.haskell.packages.ghc94;
          in
          haskellPackages.shellFor {
            packages = p: [ self.packages.${system}.whohome ];
            withHoogle = true;
            buildInputs = (with pkgs; [ ghcid cabal-install ormolu hlint ])
            ++ (with haskellPackages; [ haskell-language-server ]);
            inherit (self.checks.${system}.pre-commit-check) shellHook;
          };
      });
}
