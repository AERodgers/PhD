# script creates a data frame for analysis of alignment of PNs as a function
# of anacrusis

library(lmerTest)
library(optimx)
library(sjPlot)
library(ggplot2)

root = "G:/My Drive/Phonetics and speech/Research/3 Data Analysis/1 Alignment"
dir = paste(root, "/2 PN NUC Statistical analysis/", sep = "")


# READ IN PN DATABASE AND LIST OF PARAMETERS NEEDED
PN_anac   <- read.delim(paste(dir,"PN_A_ANAC.Table", sep = ""),
                   na.strings="--undefined--")

# add +/- anacrusis column
#PN_anac$hasAnac <- PN_anac$ana_syls != 0

# CHANGE PITCH VALUES TO SEMITONES RE 1 HZ
PN_anac$S_fo <- log2(PN_anac$S_fo) * 12
PN_anac$L_fo <- log2(PN_anac$L_fo) * 12
PN_anac$H_fo <- log2(PN_anac$H_fo) * 12
PN_anac$R_fo <- PN_anac$H_fo - PN_anac$L_fo

# CONVERT TIME TO MS
PN_anac$S_t <- PN_anac$S_t * 1000
PN_anac$L_t <- PN_anac$L_t * 1000
PN_anac$H_t <- PN_anac$H_t * 1000
PN_anac$wrd_end_t <- PN_anac$wrd_end_t * 1000
PN_anac$V_on <- PN_anac$V_on * 1000
PN_anac$V_off <- PN_anac$V_off * 1000

# change H_t and L_t to time from V-on
PN_anac$L_t <- PN_anac$L_t - PN_anac$V_on
PN_anac$H_t <- PN_anac$H_t - PN_anac$V_on
PN_anac$D_t <- PN_anac$H_t - PN_anac$L_t

# GET SIMPLE STATISTICAL DATA
#PN_anac$middle <- (PN_anac$H_fo + PN_anac$L_fo) / 2
#PN_anac$Delta <- (PN_anac$H_fo - PN_anac$L_fo)
#PN_anac$range <- (PN_anac$H_t - PN_anac$L_t)
#PN_anac$SimpSlope <- PN_anac$Delta/(PN_anac$H_t - PN_anac$L_t)*1000

#Remove unneeded columns and variables
PN_delete <- read.delim(paste(dir,"PN_A_ANAC_needed.txt", sep = ""),
                        na.strings="--undefined--")
PN_delete <- PN_delete[PN_delete$needed == "0", ]
PN_delete <- PN_delete$parameter
PN_anac = PN_anac[,!(names(PN_anac) %in% PN_delete)]
PN_anac = PN_anac[PN_anac$acc != "(*)",]
PN_anac = PN_anac[PN_anac$acc != "L*",]
PN_anac = PN_anac[PN_anac$acc != "H*",]
PN_anac = PN_anac[PN_anac$acc != "H*L",]
#PN_anac = PN_anac[PN_anac$acc == "L*H",]
remove(root, dir, PN_delete)

# SET FACTOR PARAMETERS (to be sure)
PN_anac$stim <- as.factor(PN_anac$stim)
PN_anac$speaker <- as.factor(PN_anac$speaker)
PN_anac$gender <- as.factor(PN_anac$gender)
PN_anac$pn_Ons <- as.factor(PN_anac$pn_Ons)
PN_anac$pn_Rhy <- as.factor(PN_anac$pn_Rhy)
PN_anac$rep <- as.factor(PN_anac$rep)
PN_anac$init_phon <- as.factor(PN_anac$init_phon)
PN_anac$ft_syls <- factor(PN_anac$ft_syls, levels = c("1", "2", "3", "4"))
PN_anac$ana_syls <- factor(PN_anac$ana_syls, levels = c("0", "1", "2", "3"))

PN_anac$wrd_end_syl <- as.factor(PN_anac$wrd_end_syl)
PN_anac$acc <- as.factor(PN_anac$acc)
PN_anac$L_syl <- factor(PN_anac$L_syl)
PN_anac$H_syl <- factor(PN_anac$H_syl)

save(PN_anac, file = "data/PN_anac.RData")
remove(PN_anac)

# RUN STATISTICAL TESTS
# RUN TESTS TREATING EACH FOOT SIZE FACTOR AS THE INTERCEPT
#syls <- as.array(c("1", "2", "3", "4"))

#for(i in 4:4){
#  source("PN_anac_tests.R")
#  source("PN_anac_output.R")
#}




