{
  inputs = {
    nixpkgs = {
      type = "github";
      owner = "nixos";
      repo = "nixpkgs";
      ref = "master";
    };

    clash-compiler = {
      type = "github";
      owner = "clash-lang";
      repo = "clash-compiler";
      ref = "master";
      flake = false;
    };

    flake-utils.url = "github:numtide/flake-utils";
    haskell-language-server = {
      type = "github";
      owner = "haskell";
      repo = "haskell-language-server";
      ref = "master";
      inputs.nixpkgs.follows = "nixpkgs";
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
            packageOverrides = hfinal: hprev: lib.mapAttrs (_: p: final.haskell.lib.disableLibraryProfiling p)
            {
              clash-lib = hfinal.callCabal2nix "clash-lib" "${inputs.clash-compiler}/clash-lib" {};
              clash-ghc = hfinal.callCabal2nix "clash-ghc" "${inputs.clash-compiler}/clash-ghc" {};
              clash-prelude = hfinal.callCabal2nix "clash-prelude" "${inputs.clash-compiler}/clash-prelude" {};
              clash-cores = hfinal.callCabal2nix "clash-cores" "${inputs.clash-compiler}/clash-cores" {};
              # clash-shake = hfinal.callCabal2nix "clash-shake" "${self}/greenpak-fw/nih/clash-shake" {};

              clash-topgen =  hfinal.callCabal2nix "clash-topgen" "${self}/greenpak-fw/clash/clash-topgen" {};
            };
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
         inputs.haskell-language-server.packages.${s}."haskell-language-server-${ghcVersion}"
          p.haskell.packages."ghc${ghcVersion}".cabal-install
          p.haskell.packages."ghc${ghcVersion}".ghcid
          p.hpack
          p.yosys p.xdot p.graphviz p.verilog
          p.fsatrace
        ];
        inputsFrom = [ self.legacyPackages."${s}".atx-gp.env ];
      };
    in {
      legacyPackages = forAllSystems pkg;
      devShell = forAllSystems shell;

      defaultPackage = forAllSystems (s: _: self.legacyPackages."${s}".atx-gp);
  };
}
