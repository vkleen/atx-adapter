{
  inputs = {
    nixpkgs = {
      type = "github";
      owner = "vkleen";
      repo = "nixpkgs";
      ref = "local";
    };

    clash-compiler = {
      type = "github";
      owner = "clash-lang";
      repo = "clash-compiler";
      ref = "master";
      flake = false;
    };
  };

  description = "Greenpak Gateware for an ATX power supply adapter/FPGA devboard";

  outputs = { self, ... }@inputs:
    let
      ghcVersion = "8104";

      inherit (inputs.nixpkgs) lib;
      overlays = system: [
        (final: prev: {
          haskell = prev.haskell // {
            packageOverrides = (hfinal: hprev: lib.mapAttrs (_: p: final.haskell.lib.disableLibraryProfiling p)
            {
              clash-lib = hfinal.callCabal2nix "clash-lib" "${inputs.clash-compiler}/clash-lib" {};
              clash-ghc = hfinal.callCabal2nix "clash-ghc" "${inputs.clash-compiler}/clash-ghc" {};
              clash-prelude = hfinal.callCabal2nix "clash-prelude" "${inputs.clash-compiler}/clash-prelude" {};
              clash-cores = hfinal.callCabal2nix "clash-cores" "${inputs.clash-compiler}/clash-cores" {};
            });
          };
        })
      ];


      pkgs = lib.mapAttrs (system: pkgs:
          import pkgs.path { inherit system; config = { allowBroken = true; }; overlays = overlays system;}
        ) inputs.nixpkgs.legacyPackages;
      forAllSystems = f: lib.mapAttrs f pkgs;

      pkg = _: p: {
        atx-gp = p.haskell.lib.disableLibraryProfiling (
          p.haskell.packages."ghc${ghcVersion}".callCabal2nix "atx-gp" "${self}/greenpak-fw/clash" {}
        );
      };

      shell = s: p: p.mkShell {
        packages = [
          (p.haskell-language-server.override { supportedGhcVersions = [ ghcVersion ]; })
          p.haskell.packages."ghc${ghcVersion}".cabal-install
        ];
        inputsFrom = [ self.packages."${s}".atx-gp.env ];
      };
    in {
      packages = forAllSystems pkg;
      devShell = forAllSystems shell;

      defaultPackage = forAllSystems (s: _: self.packages."${s}".atx-gp);
  };
}
