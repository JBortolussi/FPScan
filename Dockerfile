FROM ubuntu:24.04
WORKDIR /app
RUN apt update
RUN apt upgrade -y
RUN apt -y install opam
RUN opam init --disable-sandboxing --yes
RUN eval $(opam env)
RUN opam switch create 5.2.0
RUN eval $(opam env --switch=5.2.0)
RUN echo 'eval $(opam env --switch=5.2.0)' >> /root/.bashrc
RUN opam install dune
RUN apt install -y pkg-config
RUN apt install -y libmpfr-dev
RUN opam install --yes z3

COPY synasc.zip .
RUN unzip synasc.zip
RUN chmod u+x scripts/*
# FPChecker
RUN ./scripts/install_fpchecker_dependencies.sh
# RUN ./scripts/build_fpchecker.sh
# FPScan
RUN ./scripts/install_fpscan_dependencies.sh
# RUN ./scripts/build_fpscan.sh
# Report generator
# RUN ./scripts/install_report_generator_dependencies.sh