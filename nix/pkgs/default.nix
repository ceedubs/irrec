# use a pinned version of nixpkgs for reproducability
let
  nixpkgs-version = "20.03";
in
import (builtins.fetchTarball {
  # Descriptive name to make the store path easier to identify
  name = "nixpkgs-${nixpkgs-version}";
  url = "https://github.com/nixos/nixpkgs/archive/${nixpkgs-version}.tar.gz";
  # Hash obtained using `nix-prefetch-url --unpack <url>`
  sha256 = "0182ys095dfx02vl2a20j1hz92dx3mfgz2a6fhn31bqlp1wa8hlq";
})
