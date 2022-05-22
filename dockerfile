FROM rocker/rstudio:4.1.0

#install linux packages
RUN apt-get update && apt-get install -y \
apt-utils \
apt-transport-https \
gnupg \
curl \
nano \
unixodbc \
unixodbc-dev \
tdsodbc \
libxml2-dev \
libz-dev 

# install dvc for debian/ubuntu (install from binary package to avoid any reproducibility issues) 
RUN sudo wget \
https://dvc.org/deb/dvc.list \
-O /etc/apt/sources.list.d/dvc.list \
&& wget -qO - https://dvc.org/deb/iterative.asc | sudo apt-key add - \
&& sudo apt update \
&& sudo apt install dvc

#install R packages
RUN R -e 'install.packages(pkgs = c("tidyverse"))'
RUN R -e 'install.packages(pkgs = c("tidymodels"))'
RUN R -e 'install.packages(pkgs = c("rpart"))'
RUN R -e 'install.packages(pkgs = c("rpart.plot"))'
RUN R -e 'install.packages(pkgs = c("caret"))'
RUN R -e 'install.packages(pkgs = c("doParallel"))'
RUN R -e 'install.packages(pkgs = c("xgboost"))'
RUN R -e 'install.packages(pkgs = c("e1071"))'

#set up project directory
RUN mkdir -m 777 /home/rstudio/project