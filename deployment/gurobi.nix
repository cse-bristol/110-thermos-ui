{ stdenv, lib, autoPatchelfHook, python3 }:

stdenv.mkDerivation rec {
  pname = "gurobi";
  version = "13.0.1";

  # Replace fetchurl with a path to the local tar file
  src = /root/thermos-main/thermos-ui/resources/gurobi13.0.1_linux64.tar.gz;

  sourceRoot = "gurobi${builtins.replaceStrings ["."] [""] version}/linux64";

  nativeBuildInputs = [ autoPatchelfHook ];
  buildInputs = [ (python3.withPackages (ps: [ ps.gurobipy ])) ];

  strictDeps = true;

  makeFlags = [ "--directory=src/build" ];

  installPhase = ''
    mkdir -p $out/bin
    cp bin/* $out/bin/
    rm -f $out/bin/gurobi.sh
    rm -f $out/bin/python*

    [ -f lib/gurobi.py ] && cp lib/gurobi.py $out/bin/gurobi.sh || true

    mkdir -p $out/include
    cp include/gurobi*.h $out/include/

    mkdir -p $out/lib
    cp lib/*.jar $out/lib/
    cp lib/libGurobiJni*.so $out/lib/
    cp -P lib/libgurobi*.so* $out/lib/
    cp -P lib/libgurobi*.a $out/lib/ 2>/dev/null || true
    cp src/build/*.a $out/lib/

    mkdir -p $out/share/java
    ln -s $out/lib/gurobi.jar $out/share/java/
    ln -s $out/lib/gurobi-javadoc.jar $out/share/java/
  '';

  passthru.libSuffix = lib.replaceStrings [ "." ] [ "" ] (lib.versions.majorMinor version);

  meta = with lib; {
    description = "Optimization solver for mathematical programming";
    homepage = "https://www.gurobi.com";
    sourceProvenance = with sourceTypes; [
      binaryBytecode
      binaryNativeCode
    ];
    license = licenses.unfree;
    platforms = [ "x86_64-linux" ];
    maintainers = with maintainers; [ wegank ];
  };
}
