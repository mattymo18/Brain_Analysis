Brain Analysis
==============

USAGE
-----
You'll need Docker and the ability to run Docker as your current user.

You'll need to build the container:

    > docker build . -t brain_env

This Docker container is based on rocker/verse. To run rstudio server:

    > docker run -v `pwd`:/home/rstudio -p 8787:8787 -e PASSWORD=mypass -t brain_env
      
Then connect to the machine on port 8787.

Username: rstudio \
Password: mypass

#### Make
Use Makefile as recipe book for building artifacts found in derived directories. 

##### Example:
In local project directory, to build artifact named Analysis.pdf:

    > make Analysis.pdf
    
Use artifacts before colon as make targets. Dependencies are listed after colon.

Introduction
------------

This project contains R code for brain connectome analysis. 