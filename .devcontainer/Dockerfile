FROM mcr.microsoft.com/devcontainers/base:bullseye
# Install the xz-utils package
RUN apt-get update && \
  apt-get install -y \
    texlive \
    texlive-science \
    texlive-latex-recommended \
    texlive-extra-utils \
    chktex \
    latexmk \
    gdebi-core

RUN curl -LO https://quarto.org/download/latest/quarto-linux-amd64.deb
RUN gdebi --n quarto-linux-amd64.deb
