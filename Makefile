#This is a Makefile

#################################################

#Phony target for cleaning repository
.PHONY: clean
#cleans entire repository of derived elements
clean:
	rm derived_data/*.csv
	rm derived_graphics/*.png
	rm derived_graphics/*.rds
	rm derived_models/*.rds
	rm Analysis.pdf

################################################

#builds final report	
Analysis.pdf:\
 Analysis.Rmd
	R -e "rmarkdown::render('Analysis.Rmd')"
	
#clean traits
derived_data/Clean.Traits.csv:\
 Source_Data/traits/175traits/HCP_175Traits.mat\
 Source_Data/traits/175traits/Details_175_Traits.xls\
 tidy_traits.R
	Rscript tidy_traits.R