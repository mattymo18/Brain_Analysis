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

#phony target to run penalized_mods script for output
.PHONY: penalized_mods

penalized_mods:
	Rscript penalized_mods_FC.R
	Rscript penalized_mods_SC.R

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
derived_data/Coupling.csv:\
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
	
#modeling data cleaner
derived_data/model_data_TNPCA_FC.csv\
derived_data/model_data_TNPCA_SC.csv:\
 derived_data/FC.clean.csv\
 derived_data/SC.clean.csv\
 derived_data/FC.PCA.clean.csv\
 derived_data/SC.PCA.clean.csv\
 derived_data/Coupling.csv\
 tidy_model_data.R
	Rscript tidy_model_data.R
	
#GLM FC ROC cruves and results
derived_graphics/FC_ROC_Curve_Alc.png\
derived_graphics/FC_ROC_Curve_MJ.png\
derived_graphics/FC_ROC_Curve_Drug.png:\
 derived_data/model_data_TNPCA_FC.csv\
 glm_FC.R
	Rscript glm_FC.R
	
#GLM SC ROC cruves and results
derived_graphics/SC_ROC_Curve_Alc.png\
derived_graphics/SC_ROC_Curve_MJ.png\
derived_graphics/SC_ROC_Curve_Drug.png:\
 derived_data/model_data_TNPCA_SC.csv\
 glm_SC.R
	Rscript glm_SC.R