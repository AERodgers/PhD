# Uprising, the Phonology and Phonetics of Intonation in Derry City English
  GitHub repository for PhD Thesis by Antoin Eoin Rodgers. Phonetics and Speech Laboratory, Trinity College Dublin. September 2022
  
  This repository is divided into several sections, outlined below.
  
## 1. Praat Scripts
  
This contains three plugins and miscellaneous scripts which I wrote to aid my PhD analysis. The plugin can be install in Praat by copying the folders beginning with 
"plugin_" to your [Praat preferences folder](https://www.fon.hum.uva.nl/praat/manual/preferences_folder.html). If you do not have Praat or the scripts and plugins do not work on your version, the latest version can be downloaded [here](https://www.fon.hum.uva.nl/praat/). 

### a. 2017-2018 TextGrid Scripts

This includes two scripts, **create_textgrids.praat** and **create_more_tiers.praat**.

**create_textgrids.praat** generates textgrids with a syllable and comments tier for all sound files in a folder.

**create_more_tiers.praat** automatically genderates othrographic, rhythmic, and phonological tiers (among others) from a pre-annotated syllable tier. The pre-annotated tier must use allcaps for stressed syllables and hyphens between syllables of the same word.

### b. plugin_AERoPlot

This plugin contains a range of scripts which were developed, in part, for this thesis. It includes the C3P-o-gram script which draws f0 contours that also show the strength of periodicity via Cepstral Peak Prominence (CPP) (Thus, Cepstral Peak Prominence and Pitch or C3P.) The complete plugin with a manual can be also be found [here](https://github.com/AERodgers/plugin_AERoPlot).

### c. plugin_KMax

This is the plugin used for the secondary target / turning point analyses of pitch contours. A stand-alone repository is also available for this plugin [here](https://github.com/AERodgers/Praat-K-Max).

The 2020 Speech Prosody paper describing K-Max in detail can found [here](https://www.isca-speech.org/archive/SpeechProsody_2020/pdfs/287.pdf). 

### d. plugin_PhD

This plugin includes a set of tools developed specifically for this PhD Thesis. When installed, the tools can be found in the **Praat Objects Window >> [Praat] >> [PhD]**. The tools are:

1. Process TextGrids - used to process annotated TextGrids and pitch data to generate a csv database for analysis.  
2. Corpus Audit - used to trim invalid phrases from the A- and H-Corpora
3. Get Specific Data - a search function to find target phrases and textgfrids in the csv database.

There are also three separate scripts:

1. **adjustedSummaryByGender.praat**. A script called from R to help adjust raw data for to generate balanced representation of the data for visual presentation.
2. **make_anonymous_contours.praat**. A script which converts the original recordings into unidentifiable sound files using Praat inbuild Klaat synthesiser. The output contains resynthesised f0 and voice source components but no segmental / filter components of speech.
3. **run_praat**. A redundant R script used to call a redundant praat script.  
		
## 2. R Functions
	
This folder has two files.
`myFunctions.R` has all the functions written to facillitate statistical analysis in R.
`myColours.R` creates the colour code variables used for R plots.

## 3. Recordings

This folder contains the .wav, .TextGrids, and .Pitch files of the data used in the Thesis. NOTE: all original recordings have been made anonymous by removing any personal identifiable features via the **adjustedSummaryByGender.praat** script
	
## 4. Data

This folder contains all the dataframes used for the analyses in the thesis. 

## 5. Analysis of Form
	
This folder contains all the R markdown, HTML, and Excel files used to analyse the data in Chapter 6. Analysis of Form: Metrical and Lexical Effects. It also includes .csv and .png output from the analyses. 
	
## 6. Analysis of Sentence Mode
	
This folder contains all the R markdown, HTML, and Excel files used to analyse the data in Chapter 7. Analysis of Function: Sentence Modes. It also includes .csv and .png output from the analyses. 
	
		
