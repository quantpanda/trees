FROM rocker/verse:4.3.2

# Install system dependencies
RUN apt-get update && apt-get install -y \
    gdal-bin libgdal-dev libgeos-dev libgeos++-dev \
    libicu-dev libpng-dev libproj-dev make pandoc zlib1g-dev \
    && rm -rf /var/lib/apt/lists/*

# Configure CRAN and parallel downloads
RUN mkdir -p /usr/local/lib/R/etc/ /usr/lib/R/etc/ \
    && echo "options(repos = c(CRAN = 'https://cran.rstudio.com/'), download.file.method = 'libcurl', Ncpus = 4)" \
       | tee /usr/local/lib/R/etc/Rprofile.site \
       | tee /usr/lib/R/etc/Rprofile.site

# Install R packages
RUN R -e 'install.packages("remotes")'

# Install fixed versions of your packages
RUN Rscript -e 'remotes::install_version("magrittr", version="2.0.3", upgrade="never")'
RUN Rscript -e 'remotes::install_version("shiny", version="1.8.1", upgrade="never")'
RUN Rscript -e 'remotes::install_version("leaflet", version="2.2.2", upgrade="never")'
RUN Rscript -e 'remotes::install_version("config", version="0.3.2", upgrade="never")'
RUN Rscript -e 'remotes::install_version("testthat", version="3.2.0", upgrade="never")'
RUN Rscript -e 'remotes::install_version("shinythemes", version="1.2.0", upgrade="never")'
RUN Rscript -e 'remotes::install_version("leaflet.extras", version="1.0.0", upgrade="never")'
RUN Rscript -e 'remotes::install_version("golem", version="0.4.1", upgrade="never")'


# Copy your app
RUN mkdir /build_zone
ADD . /build_zone
WORKDIR /build_zone

# Install your package locally
ARG GITHUB_PAT
ENV GITHUB_PAT=${GITHUB_PAT}  # optional if remotes need GitHub auth
RUN R -e 'remotes::install_local(upgrade="never")'

# Clean build directory
RUN rm -rf /build_zone

# Expose Shiny port
EXPOSE 80

# Run Shiny app
CMD ["R", "-e", "options('shiny.port'=80,shiny.host='0.0.0.0'); library(trees); trees::run_app()"]
