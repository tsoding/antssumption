let
  nixos = import <nixpkgs> {};
in rec {
  antssumptionEnv = nixos.stdenv.mkDerivation {
      name = "antssumption-env";
      buildInputs = [ nixos.ghc
                      nixos.cabal-install
                      nixos.stack
                      nixos.mesa
                      nixos.freeglut
                    ];
      LD_LIBRARY_PATH="${nixos.mesa}/lib:${nixos.freeglut}/lib";
  };
}
