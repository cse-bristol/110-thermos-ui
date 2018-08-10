{glpk, fetchurl, python27, stdenv, buildEnv} :
let
pyutillib =
ps : ps.buildPythonPackage rec {
   name = "pyutillib";
   version = "5.6.3";
   src = fetchurl {
      url = "https://files.pythonhosted.org/packages/50/5d/f1091dcefbe9d7c1b92412e8f0b7ef7d8e6e155de4aa104aaab1171a74b1/PyUtilib-5.6.3.tar.gz";
      sha256 = "6a21fccfe691c39566c0bb19b5c9aa11bca8b076aa6f1dbf21c11711f5105191";
   };
   checkInputs = [ps.nose];
   propagatedBuildInputs = [ps.six ps.nose];
   doCheck = false;
};
pyomo =
ps : ps.buildPythonPackage rec {
  name = "pyomo";
  version = "5.5.0";
  src = fetchurl {
     url = "https://files.pythonhosted.org/packages/64/a6/b9dac955da402910166f9b46fd791b8a564d1ae935fa077ce168eecd8e41/Pyomo-5.5.0.tar.gz";
     sha256 = "6079a7a24d148b1c725d5364de1e0fa99e2e9c284ba223517e0955ef2b8f3ea1";
  };
  checkInputs = [ps.pytest];
  propagatedBuildInputs = [ps.ply ps.six (pyutillib ps) ps.appdirs];
  doCheck = false;
};

in
buildEnv rec {
  name = "run-solver-env";
  paths = [
    glpk
    (python27.withPackages( ps : [
      (pyomo ps)
      ps.pyyaml
    ] ))
  ];
}
