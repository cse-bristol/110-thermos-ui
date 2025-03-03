{stdenv, fetchurl, gmp, zlib, cmake, ipopt, bliss, pkgconfig, openblas, readline, gsl, cliquer, hmetis}:

let

version = "7.0.1";

scip-src = fetchurl {
   url ="https://scip.zib.de/download/release/scipoptsuite-${version}.tgz";
   sha256 = "08nldbaidp05994xjdapd29700xmsapihk2mzawcic4nv3r646cp";
};

ampl-src =  fetchurl {
   url = "https://projects.coin-or.org/svn/BuildTools/ThirdParty/ASL/src/solvers-20130815.tar";
   sha256 = "1h0vjjin0p6yzfqk2zsh5qxv8va23npndshsdnlhap6lzj44f0lv";
};

scip = stdenv.mkDerivation rec {
  name = "scipoptsuite-${version}";
  
  src = scip-src;
  
  buildInputs = [cmake gmp zlib ipopt bliss pkgconfig openblas readline gsl cliquer hmetis];
  configurePhase = ":";
  buildPhase =
  ''
  mkdir build
  cd build
  cmake .. -DCMAKE_INSTALL_PREFIX=$out -DIPOPT=on -DSYM=bliss -DBLISS_DIR=${bliss}
  '';
};

in scip
