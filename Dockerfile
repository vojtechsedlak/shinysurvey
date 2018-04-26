FROM rocker/shiny
MAINTAINER Vojtech Sedlak (vojtech@mozillafoundation.org)

# install R package dependencies
RUN apt-get update && apt-get install -y \
    sudo \
    gdebi-core \
    pandoc \
    pandoc-citeproc \
    libcurl4-gnutls-dev \
    libcairo2-dev/unstable \
    libxt-dev \
	libssl-dev \
    ## clean up
    && apt-get clean \ 
    && rm -rf /var/lib/apt/lists/
    
## Install packages from CRAN
RUN R -e "install.packages(c('tidyverse', 'stringr', 'ggthemes', 'scales','shinydashboard','markdown','shinycssloaders','lubridate'), repos='http://cran.rstudio.com/')" \
	## clean up
    && rm -rf /tmp/downloaded_packages/ /tmp/*.rds

COPY ./shinysurvey/ /srv/shiny-server/shinysurvey/