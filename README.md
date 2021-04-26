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

The human brain structural connectome, defined here as the collection of white matter fiber tracts connecting different regions of the brain (see Park and Friston 2013),(Fornito, Zalesky, and Breakspear 2013),(Craddock et al. 2013) and (Jones, Knosche, and Turner 2013), plays a crucial role in how the brain responds to everyday tasks and challenges. There has been a huge interest in studying connectomes and understanding how they vary for individuals in different groups according to traits and substance exposures.

Recent advances in noninvasive brain imaging and preprocessing have produced huge brain imaging datasets (e.g., the Human Connectome Project (Van Essen et al. 2013) and the UK Biobank (Miller et al. 2016)) along with sophisticated tools to routinely extract brain structural connectomes for different individuals. We will mainly focus on the Human Connectome Project. An overview of this project can be found [Here](https://www.humanconnectome.org/). 

#### Data:
The HCP aims to characterize human brain connectivity in about 1,200 healthy adults and to enable detailed comparisons between brain circuits, behavior and genetics at the level of individual subjects. Customized scanners were used to produce high-quality and consistent data to measure brain connectivity. 

Data were provided [in part] by the Human Connectome Project, WU-Minn Consortium (Principal Investigators: David Van Essen and Kamil Ugurbil; 1U54MH091657) funded by the 16 NIH Institutes and Centers that support the NIH Blueprint for Neuroscience Research; and by the McDonnell Center for Systems Neuroscience at Washington University.

#### Methodology:
