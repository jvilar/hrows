{ pkgs, mkDerivation, aeson, aeson-pretty, auto, base, brick, bv
, bytestring, containers, data-default, directory, filepath, gi-gdk
, gi-gtk, gi-gtk-hs, haskeline, haskell-gi-base, hspec, jmvOptions
, lens, lib, megaparsec, mtl, text, text-show, transformers, vector
, vty
}:
mkDerivation {
  pname = "hrows";
  version = "0.4.1.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  enableSeparateDataOutput = true;
  libraryHaskellDepends = [
    aeson aeson-pretty auto base brick bv bytestring containers
    data-default directory filepath gi-gdk gi-gtk haskell-gi-base
    jmvOptions lens megaparsec mtl text text-show transformers vector
    vty
  ];
  executableHaskellDepends = [
    auto base containers data-default gi-gtk gi-gtk-hs haskeline
    jmvOptions lens mtl text
  ];
  testHaskellDepends = [ aeson base bytestring hspec text ];
  executableSystemDepends = [ pkgs.makeWrapper ];
 
  postInstall = ''
    wrapProgram $out/bin/hrows --prefix XDG_DATA_DIRS : "${pkgs.gsettings-desktop-schemas}/share/gsettings-schemas/${pkgs.gsettings-desktop-schemas.name}:${pkgs.gtk3}/share/gsettings-schemas/${pkgs.gtk3.name}"
  '';
  description = "A program to handle data in form of rows";
  license = lib.licenses.gpl2Only;
}
