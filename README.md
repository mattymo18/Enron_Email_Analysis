Enron Email Analysis
====================

### USAGE

You'll need Docker and the ability to run Docker as your current user.

You'll need to build the container:

    > docker build . -t enron_env

This Docker container is based on rocker/verse. To run rstudio server:

    > docker run -v `pwd`:/home/rstudio -p 8787:8787 -e PASSWORD=mypass -t enron-env
      
Then connect to the machine on port 8787.

#### Make
Use Makefile as recipe book for building artifacts found in derived directories. 

##### Example:
In local project directory, to build artifact named Raleigh.Clean.csv:

    > make derived_data/Analysis.pdf
    
Use artifacts before colon as make targets. Dependencies are listed after colon.

### Introduction

This dataset is named Enron Corpus, which was collected and prepared by the CALO Project (Cognitive Assistant that Learns and Organizes). It contains data from about 150 users, mostly senior management of Enron, organized into folders. The corpus contains a total of about 0.5M messages. This data was originally made public, and posted to the web, by the Federal Energy Regulatory Commission during its investigation.

The email dataset was later purchased by Leslie Kaelbling at MIT, and turned out to have a number of integrity problems. A number of folks at SRI, notably Melinda Gervasio, worked hard to correct these problems, and it is thanks to them that the dataset is available. The dataset here does not include attachments, and some messages have been deleted "as part of a redaction effort due to requests from affected employees." Invalid email addresses were converted to something of the form user@enron.com whenever possible (i.e., recipient is specified in some parse-able format like "Doe, John" or "Mary K. Smith") and to no_address@enron.com when no recipient was specified.

This dataset, along with a thorough
explanation of its origin, is available at [Here](http://www-2.cs.cmu.edu/~enron/)

### Methodology

This project will use natural language processing to analyze emails between Enron employees to gain deeper insights into the collapse of the company. 