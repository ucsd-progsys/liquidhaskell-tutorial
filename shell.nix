{pkgs ? import (fetchTarball "https://github.com/tweag/nixpkgs/archive/befc83905c9.tar.gz") {}}:

let ghc = pkgs.haskell.packages.ghc924.ghcWithPackages (hpkgs: with hpkgs; [hpkgs.pandoc hpkgs.template hpkgs.hpack]);
    texlive = pkgs.texlive.combine {
      inherit (pkgs.texlive)
      amsmath
      appendix
      bera
      booktabs
      caption
      catchfile
      ccaption
      chngcntr
      cleveref
      comment
      enumitem
      etoolbox
      everysel
      filehook
      framed
      float
      fontspec
      fpl
      fvextra
      fancyvrb
      graphics
      hardwrap
      helvetic
      inconsolata
      latexmk
      lazylist
      libertine
      lineno
      listings
      mathpazo
      metafont
      microtype
      multirow
      minted
      newtxsf
      palatino
      pgf
      pgfopts
      polytable
      quotchap
      ragged2e
      scheme-basic
      stmaryrd
      setspace
      subfig
      tabulary
      textcase
      thmtools
      titlesec
      titling
      tocloft
      tufte-latex
      unicode-math
      upquote
      xcolor
      xstring
      xetex;
    };
in

pkgs.mkShell {
  buildInputs = [
    ghc
    pkgs.gnumake
    pkgs.git
    texlive
    # Needed to get correct locale for tests with encoding
    pkgs.glibcLocales
    pkgs.cacert
  ];

  shellHook = ''
    export PATH=$PWD/out/bin:$PATH
  '';
}
