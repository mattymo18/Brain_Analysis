FROM rocker/verse
MAINTAINER Matt Johnson <johnson.matt1818@gmail.com>
#For R
RUN R -e "install.packages('R.matlab')"
RUN R -e "install.packages('readxl')"
RUN R -e "install.packages('gridExtra')"
RUN R -e "install.packages('grid')"
RUN R -e "install.packages('glmnet')"
RUN R -e "install.packages('epiDisplay')"
RUN R -e "install.packages('reshape2')"
RUN R -e "install.packages('MLeval')"
RUN R -e "install.packages('caret')"
RUN R -e "install.packages('ROSE')"
RUN R -e "install.packages('MLmetrics')"
RUN R -e "install.packages('ROCR')"
RUN R -e "install.packages('knitr')"
RUN R -e "install.packages('e1071', dependencies=TRUE)"
