Enron Email Analysis
--------------------

USAGE
=====
You'll need Docker and the ability to run Docker as your current user.

You'll need to build the container:

    > docker build . -t enron_env

This Docker container is based on rocker/verse. To run rstudio server:

    > docker run -v `pwd`:/home/rstudio -p 8787:8787 -e PASSWORD=mypass -t enron-env
      
Then connect to the machine on port 8787.

Introduction
============
