.PHONY: clean

clean:
	rm derived_data/*.csv
	rm derived_graphics/*.png
	rm README_graphics/*.png
	
#builds final report	
Analysis.pdf:\
 Analysis.Rmd
	R -e "rmarkdown::render('Analysis.Rmd')"
	

README_graphics/Top.30.Send.Receive.Plot.png\
derived_graphics/Top.30.Send.Receive.Plot.png:\
 derived_data/Inbox.Outbox.csv\
 tidy_graphics.R
	Rscript tidy_graphics.R

derived_data/Inbox.Of.User.csv\
derived_data/Sent.By.User.csv\
derived_data/Inbox.Outbox.csv:\
 tidy_data.R
	Rscript tidy_data.R