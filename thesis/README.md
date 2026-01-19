## LaTeX — requirements and build order

### System requirements
To compile this project locally, you need **TeX Live** with support for **XeLaTeX** or **LuaLaTeX**.

- **macOS**:
  brew install --cask mactex  
  (alternative: basictex)

- **Linux**:
  - Debian / Ubuntu:
    sudo apt install texlive-xetex texlive-luatex texlive-fonts-recommended
  - Arch / Manjaro:
    sudo pacman -S texlive-most

Use **XeLaTeX** or **LuaLaTeX** for compilation (do not use pdflatex).

---

### Build order

1. Go to the title_page/ directory and compile the title page:  
   xelatex titlepage-en.tex  
   This will generate the title page PDF.

2. Copy the generated PDF file into the thesis/ directory.

3. Go to the thesis/ directory and compile the main thesis file:  
   xelatex thesis-en.tex  

The title page is included in the thesis as a precompiled PDF.
