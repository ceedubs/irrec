{ pkgs ? import ../pkgs {} }:
let
  dev-env = import ../dev-env { inherit pkgs; };
in
  with pkgs;
  stdenv.mkDerivation {
    name = "irrec-release-env";

    buildInputs = [
      gnupg
    ] ++ dev-env.buildInputs;
  }
