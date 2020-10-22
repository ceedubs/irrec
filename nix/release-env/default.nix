{ pkgs ? import ../pkgs {} }:
let
  dev-env = import ../dev-env { inherit pkgs; };
in
  with pkgs;
  let
    pinentry = if (stdenv.isDarwin) then pinentry_mac else pkgs.pinentry;
  in
    stdenv.mkDerivation {
      name = "irrec-release-env";

      buildInputs = [
        gnupg
        pinentry
      ] ++ dev-env.buildInputs;
    }
