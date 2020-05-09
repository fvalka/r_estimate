# get shiny serves plus tidyverse packages image
FROM rocker/shiny-verse:latest

# system libraries of general use
#RUN apt-get update && apt-get install -y \
#    sudo \
#    pandoc \
#    pandoc-citeproc \
#    libcurl4-gnutls-dev \
#    libcairo2-dev \
#    libxt-dev \
#    libssl-dev \
#    libssh2-1-dev 
  

# install R packages required 
# (change it dependeing on the packages you need)
RUN R -e "install.packages('shiny', repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('lubridate', repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('ggplot2', repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('utils', repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('ggpubr', repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('ggnewscale', repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('latex2exp', repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('pracma', repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('shinycssloaders', repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('Cairo', repos='http://cran.rstudio.com/')"


# copy the app to the image
COPY r_estimate.Rproj /srv/shiny-server/
COPY r_estimate/ /srv/shiny-server/

# select port
EXPOSE 3838

# allow permission
RUN sudo chown -R shiny:shiny /srv/shiny-server

# run app
CMD ["/usr/bin/shiny-server.sh"]