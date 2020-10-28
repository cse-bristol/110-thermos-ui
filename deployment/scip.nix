{stdenv, fetchurl, gmp, zlib, cmake, ipopt}:

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
  
  buildInputs = [cmake gmp zlib ipopt];
  configurePhase = ":";
  buildPhase =
  ''
  pushd scip/interfaces/ampl
  tar xf "${ampl-src}"
  pushd solvers
  make -f makefile.u CFLAGS='-w -O'
  popd; popd
  mkdir build
  cd build
  cmake .. -DCMAKE_INSTALL_PREFIX=$out
  make scipampl
  patchelf --set-rpath $out/lib bin/interfaces/ampl/scipampl
  install -m755 -D bin/interfaces/ampl/scipampl $out/bin/scipampl
  '';
};

in scip
