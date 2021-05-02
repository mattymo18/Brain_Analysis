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
 Source_Data/traits/table1_hcp.csv\
 Source_Data/traits/table2_hcp.csv\
 Source_Data/traits/HCP_S1200_DataDictionary_Sept_18_2017.xls\
 tidy_traits.R
	Rscript tidy_traits.R
	
#clean data
derived_data/SC.clean.csv\
derived_data/FC.clean.csv\
derived_data/SC.PCA.clean.csv\
derived_data/FC.PCA.clean.csv:\
 derived_data/Clean.Traits.csv\
 Source_Data/TNPCA_Result/TNPCA_Coeff_HCP_Structural_Connectome.mat\
 Source_Data/TNPCA_Result/TNPCA_Coeff_HCP_Functional_Connectome.mat\
 Source_Data/SC/HCP_cortical_DesikanAtlas_SC.mat\
 Source_Data/FC/HCP_cortical_DesikanAtlas_FC.mat\
 tidy_data.R
	Rscript tidy_data.R
	
#Coupling work
derived_data/Druguser.cp.csv:\
 derived_data/FC.clean.csv\
 derived_data/SC.clean.csv\
 Coupling_data.R
	Rscript Coupling_data.R
	
#prelim plots
prelim_graphics/Drug.Histograms.png:\
 derived_data/Clean.Traits.csv\
 derived_data/Druguser.cp.csv\
 tidy_eda_plots.R
	Rscript tidy_eda_plots.R