FROM rocker/verse
MAINTAINER Matt Johnson <johnson.matt1818@gmail.com>
#For R
RUN R -e "install.packages('R.matlab')"
RUN R -e "install.packages('readxl')"
RUN R -e "install.packages('gridExtra')"
RUN R -e "install.packages('grid')"
RUN R -e "install.packages('glmnet')"
RUN R -e "install.packages('epiDisplay')"
