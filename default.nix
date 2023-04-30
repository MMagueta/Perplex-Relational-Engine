{ lib, stdenv, clang-tools, llvmPackages_latest, llvm }:

stdenv.mkDerivation rec {
  name = "expressdb";

  src = ./src;

  buildInputs = [ clang-tools llvmPackages_latest.libstdcxxClang llvmPackages_latest.libcxx llvm ];

  dontConfigure = true;
  
  buildPhase = ''
    $CXX -Wall -Wextra $(llvm-config --cxxflags --ldflags --system-libs --libs core) $src/*.cpp -o expressdb
  '';

  installPhase = ''
    mkdir -p $out/bin
    mv expressdb $out/bin/expressdb
  '';
}
