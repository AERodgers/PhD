# script creates a data frame for analysis of alignment of PNs as a function
# of foot size

library(lmerTest)
library(optimx)
library(sjPlot)
library(ggplot2)

dir = "G:/My Drive/Phonetics and speech/Research/3 Data Analysis/1 Alignment/2 PN NUC Statistical analysis/"

# READ IN PN DATABASE AND LIST OF PARAMETERS NEEDED
PN_foot   <- read.delim(paste(dir,"PN_A_FOOT.Table", sep = ""),
                   na.strings="--undefined--")

# CHANGE PITCH VALUES TO SEMITONES RE 1 HZ
PN_foot$S_fo <- log2(PN_foot$S_fo) * 12
PN_foot$L_fo <- log2(PN_foot$L_fo) * 12
PN_foot$H_fo <- log2(PN_foot$H_fo) * 12
PN_foot$R_fo <- PN_foot$H_fo - PN_foot$L_fo

# CONVERT TIME TO MS
PN_foot$S_t <- PN_foot$S_t * 1000
PN_foot$L_t <- PN_foot$L_t * 1000
PN_foot$H_t <- PN_foot$H_t * 1000
PN_foot$wrd_end_t <- PN_foot$wrd_end_t * 1000
PN_foot$V_on <- PN_foot$V_on * 1000
PN_foot$V_off <- PN_foot$V_off * 1000

# change H_t and L_t to time from V-on
PN_foot$L_t <- PN_foot$L_t - PN_foot$V_on
PN_foot$H_t <- PN_foot$H_t - PN_foot$V_on
PN_foot$D_t <- PN_foot$H_t - PN_foot$L_t

# REMOVE UNWANTED COLUMNS AND VARIABLES
PN_delete <- read.delim(paste(dir,"PN_A_FOOT_needed.txt", sep = ""),
                        na.strings="--undefined--")
PN_delete <- PN_delete[PN_delete$needed == "0", ]
PN_delete <- PN_delete$parameter
PN_foot = PN_foot[,!(names(PN_foot) %in% PN_delete)]

#PN_foot = PN_foot[PN_foot$acc != "(*)",]
#PN_foot = PN_foot[PN_foot$acc != "L*",]
#PN_foot = PN_foot[PN_foot$acc != "H*",]
#PN_foot = PN_foot[PN_foot$acc != "H*L",]
#PN_foot = PN_foot[PN_foot$acc != "L*H",]

#PN_foot = PN_foot[PN_foot$acc == "L*H",]
#PN_foot = PN_foot[PN_foot$acc == "H*",]
#PN_foot = PN_foot[PN_foot$acc == ">H*",]

remove(dir, PN_delete)

# SET FACTOR PARAMETERS (to be sure)
PN_foot$stim <- as.factor(PN_foot$stim)
PN_foot$speaker <- as.factor(PN_foot$speaker)
PN_foot$gender <- as.factor(PN_foot$gender)
PN_foot$pn_Ons <- as.factor(PN_foot$pn_Ons)
PN_foot$pn_Rhy <- as.factor(PN_foot$pn_Rhy)
PN_foot$rep <- as.factor(PN_foot$rep)
PN_foot$init_phon <- as.factor(PN_foot$init_phon)

PN_foot$ft_syls <- factor(PN_foot$ft_syls, levels = c("1", "2", "3", "4"))

#PN_foot$acc <- factor(PN_foot$acc, levels = c("H*", ">H*", "L*H", "L*", "(*)"))

PN_foot$ana_syls <- factor(PN_foot$ft_syls)
PN_foot$wrd_end_syl <- as.factor(PN_foot$wrd_end_syl)
PN_foot$acc <- as.factor(PN_foot$acc)
PN_foot$L_syl <- factor(PN_foot$L_syl)
PN_foot$H_syl <- factor(PN_foot$H_syl)


save(PN_foot, file = "data/PN_foot.RData")
remove(PN_foot)

