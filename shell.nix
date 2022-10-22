{ pkgs ? import <nixpkgs> { } }:

pkgs.mkShell {
  buildInputs = with pkgs; 
    [ cmake
      xorg.libX11.dev
      xorg.libXinerama
      xorg.libXrandr
      xorg.libXcursor
      xorg.libXi
      xorg.libXext
      libGL
    ];

  nativeBuildInputs = [ pkgs.pkg-config ];

  shellHook = ''
    export PATH=$PATH:~/.pack/bin
  '';
}

