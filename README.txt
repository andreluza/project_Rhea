This project has the following folders:

"Arthur_shiny", with the codes R and shiny to create the webpage dedicated to collecting information from expert ornithologists
"bugs_code" folder with data integration models
"data",  all datasets used in the study, from Rhea detections in each database, environmental covariates, spatial data (rasters, shapefiles), and also R codes to extract the information from different sources (see Zenodo)
"output", with the results of the models (see ZENODO)
"R", key R code needed to run the project
"referencias_MS", some references and the MS text

# some files too

gitignore = github file
project_Rhea = Rproj of this project

# ------------------------------ #
# instructions to get the results
1-open Rproj
2-open the script "1_organize_data.R" within the "R" folder. This code only serves to call other functions to format the data.
a- when running this code, the data for modeling will be saved in "data/organized_data", and some maps will appear on the screen (ema detection and non-detection along the info sources)
b- if this code doesn't run in the (fake) R studio, try it in the R GUI
3-open the script "2_windata_model.R" (R folder) to see the models and run in winbugs. Note that in the middle there is a lapply, in order to run the same model for different cell sizes to characterize the neighborhood of the municipalities
4- open scripts "3_model_ranking.R" and "4_Interpretation_maps.R" to reproduce maps, graphics, and statistics