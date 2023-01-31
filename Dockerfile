FROM rocker/rstudio:4.1

RUN apt-get update --fix-missing && apt-get install -y apt-utils \
    procps \
    wget \
    bzip2 \
    ca-certificates \
    libglib2.0-0 \
    libxext6 \
    libsm6 \
    libxrender1 \
    subversion \
    build-essential \
    libcurl4-openssl-dev \
    libgsl-dev \
    libssl-dev \
    libxml2-dev \
    libhdf5-dev \
    graphviz


FROM bioconductor/bioconductor_docker:devel

RUN Rscript -e 'install.packages(c("rmarkdown", "tidyverse", "plyr", "pheatmap","ggpubr","gplots"), dependencies = TRUE, repos = "http://cran.us.r-project.org")'
RUN Rscript -e 'BiocManager::install(c("limma","Biobase", "illuminaHumanv4.db","GEOquery","sva","RankProd"))'
RUN Rscript -e 'BiocManager::install("GEOquery")'
WORKDIR /rstudio