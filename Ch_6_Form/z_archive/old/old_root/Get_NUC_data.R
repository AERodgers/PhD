#123456789#123456789#123456789#123456789#123456789#123456789#123456789#123456789
# script creates a data frame for analysis of alignment of nuclear pitch accents

# load libraries
library(lmerTest)
library(optimx)
library(sjPlot)
library(ggplot2)

# set directory and file variables

root = "G:/My Drive/Phonetics and speech/Research/3 Data Analysis/"
dir = paste(root, "1 Alignment/2 PN NUC Statistical analysis/", sep ="")
praatTable = "NUC_A_IDEAL.Table"

# get Praat data table

NUC   <- read.delim(paste(dir, praatTable, sep = ""),
                        na.strings="--undefined--")

# CHANGE PITCH VALUES TO SEMITONES RE 1 HZ

NUC$E_fo <- log2(NUC$E_fo) * 12
NUC$L_fo <- log2(NUC$L_fo) * 12
NUC$H_fo <- log2(NUC$H_fo) * 12
NUC$R_fo <- NUC$H_fo - NUC$L_fo

# change H_t and L_t to time from V-on

NUC$L_t <- (NUC$L_t - NUC$ft_strt) - NUC$V_on  # NUC$ft_strt needed due to odd
NUC$H_t <- (NUC$H_t - NUC$ft_strt)  - NUC$V_on # data structure in Praat table
NUC$D_t <- NUC$H_t - NUC$L_t

# CONVERT TIME TO MS

NUC$L_t <- NUC$L_t * 1000
NUC$H_t <- NUC$H_t * 1000
NUC$E_t <- NUC$E_t * 1000
NUC$V_on <- NUC$V_on * 1000
NUC$V_off <- NUC$V_off * 1000

# GET SIMPLE STATISTICAL DAT
# NB $slope, $intercept, $mean from Praat table are based on a linear regression
#    of between two tonal targets, with the contour measured in ST re 1 Hz

NUC$middle <- (NUC$H_fo + NUC$L_fo) / 2
NUC$Delta <- (NUC$H_fo - NUC$L_fo)
NUC$range <- (NUC$H_t - NUC$L_t)
NUC$SimpSlope <- NUC$Delta/(NUC$H_t - NUC$L_t)*1000

#USE NUC_A_ANAC_needed.txt TO REMOVE UNNECESSARY COLUMNS FROM DATA FRAME

NUC_delete <- read.delim(paste(dir,"NUC_A_needed.txt", sep = ""),
                         na.strings="--undefined--")
NUC_delete <- NUC_delete[NUC_delete$needed == "0", ]
NUC_delete <- NUC_delete$parameter
NUC = NUC[,!(names(NUC) %in% NUC_delete)]

# REMOVE UNWANTED PHONOLOGY FROM DATA FRAME

#NUC = NUC[NUC$acc != "(*)",]
#NUC = NUC[NUC$acc != "L*",]
#NUC = NUC[NUC$acc != "H*",]
#NUC = NUC[NUC$acc != "H*L",]
#NUC = NUC[NUC$acc == "L*H",]

# SET FACTOR PARAMETERS (to be sure)

NUC$stim <- as.factor(NUC$stim)
NUC$speaker <- as.factor(NUC$speaker)
NUC$gender <- as.factor(NUC$gender)
NUC$nuc_Ons <- as.factor(NUC$nuc_Ons)
NUC$nuc_Rhy <- as.factor(NUC$nuc_Rhy)
NUC$rep <- as.factor(NUC$rep)
NUC$fin_phon <- as.factor(NUC$fin_phon)
NUC$PrN_unStr <- factor(NUC$PrN_unStr, levels = c("0", "1", "2", "3"))
NUC$ft_syls <- NUC$Nuc_unStr + rep(1, nrow(NUC))
NUC$ft_syls <- factor(NUC$ft_syls, levels = c("1", "2", "3", "4"))

NUC$acc <- as.factor(NUC$acc)
NUC$nuc_phon <- as.factor(NUC$nuc_phon)
NUC$L_syl <- factor(NUC$L_syl)
NUC$H_syl <- factor(NUC$H_syl)

# SAVE DATA FRAME

save(NUC, file = "data/NUC.RData")

# REMOVE VARIABLES FROM GLOBAL ENVIRONMENT

remove(dir, root, NUC_delete, praatTable)
remove(NUC)

# RUN STATISTICAL TESTS

# RUN TESTS TREATING EACH FOOT SIZE FACTOR AS THE INTERCEPT
#syls <- as.array(c("1", "2", "3", "4"))

#for(i in 4:4){
#  source("NUC_tests.R")
#  source("NUC_output.R")
#}




