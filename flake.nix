{
  inputs.nixpkgs.url = "github:nixos/nixpkgs";
  inputs.flake-utils.url = "github:numtide/flake-utils";
  inputs.pre-commit-hooks = {
    url = "github:cachix/pre-commit-hooks.nix";
    inputs.nixpkgs.follows = "nixpkgs";
  };

  outputs = { self, nixpkgs, flake-utils, pre-commit-hooks }:
    {
      overlay = (self: super:
        {
          libtelnet = self.callPackage
            (
              { lib, stdenv, fetchFromGitHub, pkg-config, cmake, zlib }:

              stdenv.mkDerivation {
                pname = "libtelnet";
                version = "0.30.0";

                src = fetchFromGitHub {
                  owner = "seanmiddleditch";
                  repo = "libtelnet";
                  rev = "5f5ecee776b9bdaa4e981e5f807079a9c79d633e";
                  sha256 = "sha256-4jsS34/aHHb7ojxIlIkEZqsKU11Z6xe9chxXccfa+Kk=";
                };

                patches = [ ./libtelnet.patch ];

                nativeBuildInputs = [ cmake ];
                buildInputs = [ zlib ];

                meta = {
                  description = "Simple RFC-complient TELNET implementation as a C library";
                  homepage = "https://github.com/seanmiddleditch/libtelnet";
                  license = lib.licenses.publicDomain;
                  maintainers = [ lib.maintainers.tomberek ];
                  platforms = lib.platforms.linux;
                };
              }
            )
            { };
          whohome =
            self.haskell.packages.ghc94.callCabal2nix "whohome" ./. { };
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
        packages = {
          whohome = pkgs.whohome;
          whohome_static = pkgs.pkgsStatic.whohome;
        };
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
