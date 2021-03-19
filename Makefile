.PHONY: clean

clean:
	rm derived_data/*.csv
	rm derived_graphics/*.png
	rm README_graphics/*.png
	
#builds final report	
Analysis.pdf:\
 Analysis.Rmd
	R -e "rmarkdown::render('Analysis.Rmd')"
	
# Network Plots
derived_graphics/isolated.network.spectral.comunity.png\
derived_graphics/top.100.network.spectral.comunity.png\
README_graphics/top.100.network.png:\
 derived_data/Inbox.Outbox.csv\
 tidy_network_plot.R
	Rscript tidy_network_plot.R

# Barplots
README_graphics/Top.30.Send.Receive.Plot.png\
derived_graphics/Top.30.Send.Receive.Plot.png:\
 derived_data/Inbox.Outbox.csv\
 tidy_graphics.R
	Rscript tidy_graphics.R

# Data
derived_data/Inbox.Of.User.csv\
derived_data/Sent.By.User.csv\
derived_data/Inbox.Outbox.csv:\
 tidy_data.R
	Rscript tidy_data.R