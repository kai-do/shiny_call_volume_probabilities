FROM rocker/r-base:latest
LABEL maintainer="Nakai Zemer <nakai.zemer@cobbcounty.org>"

USER root

# Install base linux packages
RUN apt-get update && apt-get install -y --no-install-recommends \
    libcurl4-gnutls-dev \
    libcairo2-dev \
    libxt-dev \
    libssl-dev \
    libssh2-1-dev \
    && rm -rf /var/lib/apt/lists/*
    
# Install R libraries
RUN Rscript -e "install.packages('shiny')" \
    && Rscript -e "install.packages('dplyr')" \
    && Rscript -e "install.packages('tidyr')" \
    && Rscript -e "install.packages('ggplot2')" \
    && Rscript -e "install.packages('data.table')" \
    && Rscript -e "install.packages('shinyWidgets')" \
    && Rscript -e "install.packages('lemon')" \
    && Rscript -e "install.packages('remotes')" \
    && Rscript -e "remotes::install_github('deepanshu88/shinyDarkmode')" 
    
# Configure port to shiny
RUN echo "local(options(shiny.port = 3838, shiny.host = '0.0.0.0'))" > /usr/lib/R/etc/Rprofile.site

RUN addgroup --system app \
    && adduser --system --ingroup app app \
    && mkdir /home/app \
    && chown app:app /home/app

WORKDIR /home/app
COPY app /home/app/
USER app

# Open port to shinyproxy
EXPOSE 3838
CMD ["R", "-e", "shiny::runApp('/home/app')"]



