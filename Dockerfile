FROM rocker/rstudio:3.6.3

COPY packages.txt .

RUN apt-get update && apt-get install -y \
    zlib1g-dev

RUN Rscript -e 'withCallingHandlers(install.packages(trimws(readLines("packages.txt"))), warning = function(w) stop(w))'
