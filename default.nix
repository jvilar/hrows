let
  config = {
    packageOverrides = pkgs: rec {
      haskellPackages = pkgs.haskellPackages.override {
        overrides = haskellPackagesNew: haskellPackagesOld: {
          auto = haskellPackagesNew.callPackage ./auto.nix {};
          jmvOptions = haskellPackagesNew.callPackage ./jmvOptions.nix {};
          hrows = haskellPackagesNew.callPackage ./hrows.nix {};
        };
      };
    };
  };
  pkgs = import <nixpkgs> { inherit config; };
in
  {
    hrows = pkgs.haskellPackages.hrows;
  }
