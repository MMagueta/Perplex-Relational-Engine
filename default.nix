{ pkgs, mlton }:

pkgs.stdenv.mkDerivation {
  name = "ExpressDB";

  src = ./.;

  buildPhase = ''
    mkdir $out
    # ${mlton}/bin/mlton -default-ann 'allowFFI true' -export-header $src/ExpressDB.h -stop tc -output $src/ExpressDB $src/ExpressDB.mlb
    pwd
    ${mlton}/bin/mlton -output $out/ExpressDB $src/ExpressDB.mlb
  '';

  installPhase = ''
    mkdir $out/bin
    mv $out/ExpressDB $out/bin/ExpressDB
  '';
}