{ pkgs ? import ../pkgs {} }:
with pkgs;
let
  jvm-env = import ../jvm-env { inherit pkgs; };
in
stdenv.mkDerivation {
  name = "irrec-dev-env";

  buildInputs = [
    graphviz # used for ScalaDoc diagrams
    nodejs # used by scala.js
    yarn # used by docusaurus for doc site
  ] ++ jvm-env.buildInputs;
}
