{ pkgs ? import ../pkgs {} }:
with pkgs;
stdenv.mkDerivation {
  name = "irrec-jvm-env";

  buildInputs = [
    sbt
    git # used by sbt-dynver
  ];
}
