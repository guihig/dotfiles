{
  lib,
  stdenv,
  fetchFromGitHub,
  kernel,
}:
stdenv.mkDerivation rec {
  pname = "xpad";
  version = "3.2";

  src = fetchFromGitHub {
    owner = "paroj";
    repo = "xpad";
    rev = "ffc6910bf4e089288a500935bee49216a5017836";
    sha256 = "sha256-rkn8xs9G8YdDgOscEd0vclZVOYySGnsKWsg/NLzjYuI=";
  };

  setSourceRoot = ''
    export sourceRoot=$(pwd)/source
  '';

  nativeBuildInputs = kernel.moduleBuildDependencies;

  makeFlags =
    kernel.makeFlags
    ++ [
      "-C"
      "${kernel.dev}/lib/modules/${kernel.modDirVersion}/build"
      "M=$(sourceRoot)"
    ];

  buildFlags = ["modules"];
  installFlags = ["INSTALL_MOD_PATH=${placeholder "out"}"];
  installTargets = ["modules_install"];

  meta = with lib; {
    description = "xpad kernel module";
    homepage = "https://github.com/paroj/xpad";
    license = licenses.gpl2Plus;
    platforms = platforms.linux;
  };
}
