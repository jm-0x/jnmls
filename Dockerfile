FROM ocaml/opam:ubuntu-24.04-ocaml-5.2

USER root

RUN apt-get update -qq && apt-get install -y -qq \
    ffmpeg libcairo2-dev libpango1.0-dev \
    texlive texlive-latex-extra dvisvgm \
    python3-pip \
    && rm -rf /var/lib/apt/lists/*

RUN pip install --break-system-packages manim

ENV OPAMROOT=/home/opam/.opam
