{ pkgs ? import <nixpkgs> {} }:

with pkgs;

mkShell {
  buildInputs = [
    gtk3
    gsettings-desktop-schemas
    haskell-language-server
  ];

  shellHook = ''
      # Start with GSETTINGS_SCHEMA_PATH
      local _xdg_data_dirs="$GSETTINGS_SCHEMA_PATH"

      # If XDG_DATA_DIRS is already set, append it
      if [ -n "$XDG_DATA_DIRS" ]; then
        _xdg_data_dirs="$_xdg_data_dirs:$XDG_DATA_DIRS"
      fi

      export XDG_DATA_DIRS="$_xdg_data_dirs"
  '';
}


