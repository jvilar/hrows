{ pkgs ? import <nixpkgs> {} }:

with pkgs;

mkShell {
  buildInputs = [
    gtk3
    gsettings-desktop-schemas
    haskell-language-server
    glib
  ];

  shellHook = ''
    # Forzamos la inclusión de los directorios de esquemas de los inputs
    # 'gsettings-desktop-schemas' y 'gtk3' exponen sus esquemas en share/gsettings-schemas/
    
    export XDG_DATA_DIRS="${pkgs.gsettings-desktop-schemas}/share/gsettings-schemas/${pkgs.gsettings-desktop-schemas.name}:${pkgs.gtk3}/share/gsettings-schemas/${pkgs.gtk3.name}:$XDG_DATA_DIRS"
    
    # También es vital que GLib sepa dónde buscar los esquemas compilados (gschemas.compiled)
    export GSETTINGS_SCHEMAS_PATH="${pkgs.gsettings-desktop-schemas}/share/gsettings-schemas/${pkgs.gsettings-desktop-schemas.name}/glib-2.0/schemas"
  '';
}


