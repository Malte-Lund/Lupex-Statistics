TODO Update this Readme to something more recent.
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

Note that this project is my first major coding experience, and I am basically the only person running it, as such, IT IS NOT VERY WELL ORDERED. Enter at your own peril or whatever. Amazingly, the code runs. But it is not clear, concise, well-documented, well-commented or logical. At all. But it works, and it is a thorough analysis of the outcomes of the project.

If you want to run this project, the scripts should be run in the following order:
	0. 	OPEN THE PROJECT - StatistikLupex.Rproj - Else the Here() function wont work.
	1. 	DataWrangling.qmd
	2. 	VO2maxdata.qmd
	3. 	Questionnaires.qmd
	4. 	SLEDAI_Analysis.qmd
	5. 	CytokineScoreModel.qmd
	6.  	Analysis_of_ActivityMeasures.qmd (not ActivityMeasures.qmd, that is mainly for generating the data from the accelerometer files)
	7.  	DifferentialExpression.qmd
			UPDATE 30/8-2025 for some unknown reason, this is REALLY buggy, it often crashes on load. I have corrected it (multiple times) by pulling out the code and reloading it into a new .r script and then running that, which WORKS FINE!!? I Honestly have no idea why differentialExpression works so horribly. 
	8.  	OtherExploratoryOutcomes.qmd
	9.  	ModelAggregation.qmd
	10.	TablesRUs.qmd

This project encompassess the following subfolders:

~Main/
	Readme.txt - This file
	StatistikLupex.Rproj - Start by opening this.

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
			UPDATE 30/8-2025 for some unknown reason, this is REALLY buggy, it often crashes on load. I have corrected it (multiple times) by pulling out the code and reloading it into a new .r script and then running that, which WORKS FINE!!? I Honestly have no idea why differentialExpression works so horribly. 
		ModelAggregation.qmd
		OtherExploratoryOutcomes.qmd
		Questionnaires.qmd
		renv.lock - The renv lock file
		SampleData for Help.qmd
		SimulatedData.qmd
		SLEDAI_Analysis.qmd
		Transcriptomics.qmd
		TablesRUs.qmd
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

~/Documents/ - Contains knits, reports from third parties (for example nanostring), interpretation documents as well as the pre-published statistical analysis plan.

~/Output/ - Contains most of the output from running the code, but is also used as intermediate storage between the documents so the code can be run in smaller chunks.

~/Input/
	~ SF36USPOPMEANSD.xslx - Means and SD for US population for normalizing the SF-36


