{ config, pkgs, ... }:
let
  texlive-asabina = with pkgs; (texlive.combine {
    inherit (texlive)
      scheme-medium
      luatex

      atenddvi
      IEEEtran
      background
      bashful
      capt-of
      collection-basic
      collection-fontsrecommended
      collection-langeuropean
      collection-langgerman
      collection-latexrecommended
      datetime
      draftwatermark
      enumitem
      eso-pic
      etoolbox
      everypage
      fmtcount
      lastpage
      latexdiff
      mdframed
      needspace
      numprint
      paracol
      pdfcrop
      pgfgantt
      soul
      svg
      tableof
      titlepic
      tocloft
      ulem
      wrapfig
      xargs
      xetex
      xstring
      xtab
      ;
  });
in
{
  home.packages = with pkgs; [
    aspell
    aspellDicts.de
    aspellDicts.en
    aspellDicts.nl
    bibtex2html
    biber
    evince
    libreoffice
    okular
    texlive-asabina
    pandoc
    pdftk
    scim
    visidata
    xournal
    zathura
  ];
}
