{
  inputs.nixpkgs.url = "github:nixos/nixpkgs";
  inputs.flake-utils.url = "github:numtide/flake-utils";

  outputs = { self, nixpkgs, flake-utils }:
    {
      overlay = (final: prev: {
        whohome = final.haskellPackages.callCabal2nix "whohome" ./. { };
      });
    } // flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs {
          inherit system;
          overlays = [ self.overlay ];
        };
      in {
        packages = { whohome = pkgs.whohome; };
        defaultPackage = self.packages.${system}.whohome;
        devShell = let haskellPackages = pkgs.haskellPackages;
        in haskellPackages.shellFor {
          packages = p: [ self.packages.${system}.whohome ];
          withHoogle = true;
          buildInputs = with haskellPackages; [
            haskell-language-server
            ghcid
            cabal-install
            nixfmt
          ];
          # Change the prompt to show that you are in a devShell
          shellHook = "export PS1='\\e[1;34mdev > \\e[0m'";
        };
      });
}
