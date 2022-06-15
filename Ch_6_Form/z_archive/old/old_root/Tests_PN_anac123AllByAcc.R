# SCRIPT RUNS LMER (Linear Mixed Model) ANALYSIS ON PN_ANAC DATA FRAME ( - ana0)

# load libraries
library(lmerTest)
library(optimx)
library(sjPlot)
library(ggplot2)


# CREATE DIRECTORIES (IF DON'T EXIST)
folder = "Output_PN_model/ANAC123allByAcc/"
dir.create("Output_PN_model", showWarnings = FALSE)
dir.create(folder, showWarnings = FALSE)


# GET DATA FRAME FOR SUBSECTION OF CORPUS WITH PRECEDING COUNT AS MAIN FACTOR
load("~/github/R-Alignment_Analysis_One/data/PN_anac.RData")
PN_anac = PN_anac[PN_anac$ana_syls != 0,]
# remove anac0
#PN_anac <- PN_anac[(PN_anac$stim != "A0423"),]
PN_Type = "PAs"
# L*H only
#PN_anac <- PN_anac[(PN_anac$acc %in% "L*H"),]
#PN_Type = "LH"

# >H* only
#PN_anac <- PN_anac[(PN_anac$acc %in% ">H*"),]                                   # >H* only
#PN_Type = "dH"

write.table(PN_anac, paste("../data/PN_anac_", PN_Type, ".txt", sep = ""), sep = "\t")

# SET INTERCEPT PERMUTATION ARRAY
syls <- as.array(c("3", "1", "2"))

# RUN ANALYSIS LOOP
for(i in 1:3){
    # resort order of factors in ana_syls
    syls <- c(syls[2:4], syls[1])

    PN_anac$ana_syls <- factor(PN_anac$ana_syls, levels = syls)

    # RunModels ---------------------------------------------------------------
    PN.anac.mdl.H_t = lmer(H_t ~ ana_syls + acc * gender  + (1 + ana_syls | speaker),
                           data = PN_anac,
                           subset = abs(scale(resid(PN.anac.mdl.H_t)))<2.5,
                           control = lmerControl(
                               optimizer = "optimx",
                               calc.derivs = FALSE,
                               optCtrl = list(method = "nlminb",
                                              starttests = FALSE,
                                              kkt = FALSE)
                                                 )
                           )

    PN.anac.mdl.H_fo = lmer(
        H_fo ~ ana_syls + acc * gender  + (1 + ana_syls | speaker),
        data = PN_anac,
        subset = abs(scale(resid(PN.anac.mdl.H_fo)))<2.5,
        control = lmerControl(optimizer = "optimx",
                              calc.derivs = FALSE,
                              optCtrl = list(method = "nlminb",
                                             starttests = FALSE,
                                             kkt = FALSE))
                            )


    # AnalyseAndSaveModels -----------------------------------------------------------

    PN.anac.mdl.H_t.sum = summary(PN.anac.mdl.H_t)
    PN.anac.mdl.H_t.ano = anova(PN.anac.mdl.H_t)
    PN.anac.mdl.H_t.step = step(PN.anac.mdl.H_t)
    fileName = paste(folder, "PN.anac.mdl.H_t_", PN_Type, "_", syls[1], "syl.txt", sep = "")
    capture.output(PN.anac.mdl.H_t.sum, file = fileName)
    capture.output(PN.anac.mdl.H_t.ano, file = fileName, append =  TRUE)
    capture.output(PN.anac.mdl.H_t.step, file = fileName, append =  TRUE)

    PN.anac.mdl.H_fo.ano = anova(PN.anac.mdl.H_fo)
    PN.anac.mdl.H_fo.sum = summary(PN.anac.mdl.H_fo)
    PN.anac.mdl.H_fo.step = step(PN.anac.mdl.H_fo)
    fileName = paste(folder, "PN.anac.mdl.H_fo_", PN_Type, "_", syls[1], "syl.txt", sep = "")
    capture.output(PN.anac.mdl.H_fo.sum, file = fileName)
    capture.output(PN.anac.mdl.H_fo.ano, file = fileName, append =  TRUE)
    capture.output(PN.anac.mdl.H_fo.step, file = fileName, append =  TRUE)


    }
