# SCRIPT RUNS LMER (Linear Mixed Model) ANALYSIS ON PN_ANAC DATA FRAME
# Model assumes that the presence or absence of anacrusis has an effect

# load libraries
library(lmerTest)
library(optimx)
library(sjPlot)
library(ggplot2)


# CREATE DIRECTORIES (IF DON'T EXIST)
folder = "Output_PN_model/ANAC_TF_AllByAcc/"
dir.create("Output_PN_model", showWarnings = FALSE)
dir.create(folder, showWarnings = FALSE)


# GET DATA FRAME FOR SUBSECTION OF CORPUS WITH PRECEDING COUNT AS0 MAIN FACTOR
load("~/github/R-Alignment_Analysis_One/data/PN_anac.RData")
PN_Type = "all"

# SET INTERCEPT PERMUTATION ARRAY
syls <- as.array(c("TRUE", "FALSE"))
PN_anac$hasAnac <- factor(PN_anac$ana_syls != 0)
PN_anac$hasAnac <- factor(PN_anac$hasAnac, levels = syls)

# RunModels ---------------------------------------------------------------
PN.anac.mdl.H_t = lmer(H_t ~ hasAnac + acc*gender + (1 + hasAnac | speaker),
                       data = PN_anac,
                       control = lmerControl(
                           optimizer = "optimx",
                           calc.derivs = FALSE,
                           optCtrl = list(method = "nlminb",
                                          starttests = FALSE,
                                          kkt = FALSE)
                                             )
                       )


PN.anac.mdl.H_fo = lmer(
    H_fo ~ hasAnac + acc*gender + (1 + hasAnac | speaker),
    data = PN_anac,
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


