let

  # use a pinned version of nixpkgs for reproducability
  nixpkgs-version = "20.03";
  pkgs = import (builtins.fetchTarball {
    # Descriptive name to make the store path easier to identify
    name = "nixpkgs-${nixpkgs-version}";
    url = "https://github.com/nixos/nixpkgs/archive/${nixpkgs-version}.tar.gz";
    # Hash obtained using `nix-prefetch-url --unpack <url>`
    sha256 = "0182ys095dfx02vl2a20j1hz92dx3mfgz2a6fhn31bqlp1wa8hlq";
  }) {};

  sbt = with pkgs; symlinkJoin {
    name = "sbt-irrec";
    buildInputs = [ makeWrapper ];
    paths = [ pkgs.sbt ];
    postBuild = ''
      wrapProgram "$out/bin/sbt" \
      --add-flags "-mem 2048"
    '';
  };
in
  with pkgs;
  stdenv.mkDerivation {
    name = "irrec-dev-env";

    buildInputs = [
      sbt
      git # used by sbt-dynver
      graphviz # used for ScalaDoc diagrams
      nodejs # used by scala.js
      yarn # used by docusaurus for doc site
    ];
  }
