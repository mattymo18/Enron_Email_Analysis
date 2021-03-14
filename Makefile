.PHONY: clean

clean:
	rm derived_data/*.csv
	rm derived_graphics/*.png
	rm README_graphics/*.png
	
#builds final report	
Analysis.pdf: Analysis.Rmd\
	R -e "rmarkdown::render('Analysis.Rmd')"