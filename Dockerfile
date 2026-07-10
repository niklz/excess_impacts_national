FROM rocker/tidyverse

RUN apt update && apt -y install cron
RUN mkdir /root/utils

# Install system dependencies for openssl and textshaping
RUN apt-get update && apt-get install -y \
    libcurl4-openssl-dev \
    libxml2-dev \
    libzstd-dev \
    libssl-dev \
    libharfbuzz-dev \
    libfribidi-dev \
    default-libmysqlclient-dev \
    libcairo2-dev \
    libfreetype6-dev \
    libfontconfig1-dev


RUN apt-get update && apt-get install -y gnupg2 curl \
    && curl https://packages.microsoft.com/keys/microsoft.asc | apt-key add - \
    && curl https://packages.microsoft.com/config/debian/11/prod.list > /etc/apt/sources.list.d/mssql-release.list \
    && apt-get update \
    && ACCEPT_EULA=Y apt-get install -y msodbcsql17 unixodbc-dev

RUN R -e "options(repos = c(CRAN = 'https://packagemanager.posit.co/cran/__linux__/bullseye/latest')); \
    install.packages(c('xml2', 'rvest', 'here', 'readxl' , 'feasts', 'fable'))"


WORKDIR /root


# Copy the scripts
COPY utils/ utils/
COPY 00_libraries.R 00_libraries.R
COPY 01_utils.R 01_utils.R
COPY 02_scrape_data.R 02_scrape_data.R
COPY 03_estimate_impacts.R 03_estimate_impacts.R


# 1. Copy the crontab to a temporary location
COPY crontab /root/crontab_source

# 2. Force install it into the root user's crontab and fix permissions
RUN chmod 0644 /root/crontab_source && \
    crontab /root/crontab_source

# 3. Ensure cron is logging and has the right environment
# We use '&&' to ensure cron starts AFTER the environment is dumped
CMD printenv | grep -v "no_proxy" >> /etc/environment && cron -f -L 15
