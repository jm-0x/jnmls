FROM ocaml/opam:ubuntu-24.04-ocaml-5.2

USER root

# LaTeX + Manim deps
RUN apt-get update -qq && apt-get install -y -qq \
    ffmpeg libcairo2-dev libpango1.0-dev \
    texlive texlive-latex-extra dvisvgm \
    python3-pip \
    && rm -rf /var/lib/apt/lists/*

# Manim
RUN pip install --break-system-packages manim

USER opam