
*******************************************************************************************************************
      ._.           ._.       ._.         .____________.       .___________.         .___          .___
      | |           | |       | |         |  ______    |       |           |         \   \        /   /
      | |           | |       | |         |  |     |   |       |    .______|          \   \      /   /
      | |           | |       | |         |  |     |   |       |    |                  \   \    /   /
      | |           | |       | |         |  |_____|   /       |    |                   \   \  /   /
      | |           | |       | |         |  ._______./        |    |______.             \   \/   /
      | |           | |       | |         |  |                 |           |              \      /
      | |           | |       | |         |  |                 |           |               \    <
      | |           | |       | |         |  |                 |    .______|               /     \
      | |           | |       | |         |  |                 |    |                     /  /\   \
      | |           | |       | |         |  |                 |    |______.             /  /  \   \
      | \_____.     |  \_____/  |         |  |                 |           |            /  /    \   \
      |_______|     |___________|         |__|                 |___________|           /__/      \___\

*******************************************************************************************************************


This is the project for statistical analysis of the Type 1 interferon induced changes to exercise adaptations in systemic lupus erythematosus patients (nicknamed LUPEX for Lupus & Exercise) by Malte Lund Adamsen et al.

Much of the data is kept with Danish and English intertwined, as such, a non-Danish speaking reader would benefit immensely from having a translator ready or running it through a LLM for translating.

If you want to run this project, the scripts should be run in the following order:
	1. DataWrangling.qmd
	2. VO2maxdata.qmd
	3. Questionnaires.qmd
	4. CytokineScoreModel.qmd
	5. DietAnalysis.qmd
	6. Analysis_of_ActivityMeasures.qmd (not ActivityMeasures.qmd, that is mainly for generating the data from the accelerometer files)
	7. DifferentialExpression.qmd
	8. OtherExploratoryOutcomes.qmd
	9. ModelAggregation.qmd

This project encompassess the following subfolders:

~Main/
	Readme.txt
	StatistikLupex.Rproj

	~/Code/
		_targets.R - Deprecated, old code, decided not to use targets for this project. Although I probably should have.
		ActivityMeasures.qmd - The GGIR based code for extracting information from the accelerometer.
		Analysis_of_ActivityMeasures.qmd - The linear mixed models for analysing the activity measures 
		BloodSamplesAnalysis.qmd
		CytoKineScoreModel.qmd            
		DataWrangling.qmd
		DataWrangling_Genes_Deprecated.qmd
		DietAnalysis.qmd
		DifferentialExpression.qmd
		ModelAggregation.qmd
		OtherExploratoryOutcomes.qmd
		Questionnaires.qmd
		renv.lock - The renv lock file
		SampleData for Help.qmd
		SimulatedData.qmd
		SLEDAI_Analysis.qmd
		Transcriptomics.qmd
		UsefulCodes.qmd
		Verisense-Toolbox-master.zip
		VO2max_QC.qmd
		VO2maxdata.qmd
		~/R/          
			functions.R                      
		~/renv/
		~/Verisense-Toolbox-master/


~/Data/
	~/Old_Data/ - Contains older compilations of the raw data with various flaws. 

~/Documents/

~/Output/

~/Input/
	~ SF36USPOPMEANSD.xslx - Means and SD for US population for normalizing the SF-36


