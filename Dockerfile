FROM r-base:latest

# apt-get things we need for R packages
RUN apt-get update
RUN yes | apt-get install libssl-dev
RUN yes | apt-get install libcurl4-openssl-dev

# install package dependencies
RUN echo "r <- getOption('repos'); r['CRAN'] <- 'http://cran.us.r-project.org'; options(repos = r);" > ~/.Rprofile
RUN mkdir /usr/local/src/segml
COPY . /usr/local/src/segml/
WORKDIR /usr/local/src/segml

RUN Rscript -e "source('install.R')"

EXPOSE 1235
CMD ["Rscript", "-e", "source('app.R')"]
