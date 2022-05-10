# SCRIPT RUNS LMER (Linear Mixed Model) ANALYSIS ON PN_ANAC DATA FRAME
# Model assumes that the presence or absence of anacrusis has an effect

# load libraries
library(lmerTest)
library(optimx)
library(sjPlot)
library(ggplot2)


# CREATE DIRECTORIES (IF DON'T EXIST)
folder = "Output_PN_model/ANAC_TF_All/"
dir.create("Output_PN_model", showWarnings = FALSE)
dir.create(folder, showWarnings = FALSE)


# GET DATA FRAME FOR SUBSECTION OF CORPUS WITH PRECEDING COUNT AS0 MAIN FACTOR
load("~/github/R-Alignment_Analysis_One/data/PN_anac.RData")
PN_anac <- PN_anac[(PN_anac$acc %in% "L*H"),]                                   # L*H only
PN_Type = "LH"
#PN_Type = "all"
#PN_anac <- PN_anac[(PN_anac$acc %in% ">H*"),]                                   # >H* only
#PN_Type = "dH"


#write.table(PN_anac, paste("data/PN_anac_", PN_Type, ".txt", sep =), sep = "\t")

# SET INTERCEPT PERMUTATION ARRAY
syls <- as.array(c("TRUE", "FALSE"))
PN_anac$hasAnac <- factor(PN_anac$ana_syls != 0)
PN_anac$hasAnac <- factor(PN_anac$hasAnac, levels = syls)

# RunModels ---------------------------------------------------------------
PN.anac.mdl.L_t = lmer(L_t ~ hasAnac + gender + (1 + hasAnac | speaker),
                       data = PN_anac,
                       control = lmerControl(
                           optimizer = "optimx",
                           calc.derivs = FALSE,
                           optCtrl = list(method = "nlminb",
                                          starttests = FALSE,
                                          kkt = FALSE)
                                             )
                       )

PN.anac.mdl.H_t = lmer(H_t ~ hasAnac + gender + (1 + hasAnac | speaker),
                       data = PN_anac,
                       control = lmerControl(
                           optimizer = "optimx",
                           calc.derivs = FALSE,
                           optCtrl = list(method = "nlminb",
                                          starttests = FALSE,
                                          kkt = FALSE)
                                             )
                       )

PN.anac.mdl.D_t = lmer(D_t ~ hasAnac + gender + (1 + hasAnac | speaker),
                                    data = PN_anac,
                                    control = lmerControl(
                                        optimizer = "optimx",
                                        calc.derivs = FALSE,
                                        optCtrl = list(method = "nlminb",
                                                       starttests = FALSE,
                                                       kkt = FALSE)
                                                          )
                                    )

PN.anac.mdl.L_fo = lmer(
    L_fo  ~ hasAnac + gender + (1 + hasAnac | speaker),
    data = PN_anac,
    control = lmerControl(optimizer = "optimx",
                          calc.derivs = FALSE,
                          optCtrl = list(method = "nlminb",
                                         starttests = FALSE,
                                         kkt = FALSE))
                        )

PN.anac.mdl.H_fo = lmer(
    H_fo ~ hasAnac + gender + (1 + hasAnac | speaker),
    data = PN_anac,
    control = lmerControl(optimizer = "optimx",
                          calc.derivs = FALSE,
                          optCtrl = list(method = "nlminb",
                                         starttests = FALSE,
                                         kkt = FALSE))
                        )


PN.anac.mdl.R_fo = lmer(
    R_fo ~ hasAnac + gender + (1 + hasAnac | speaker),
    data = PN_anac,
    control = lmerControl(optimizer = "optimx",
                          calc.derivs = FALSE,
                          optCtrl = list(method = "nlminb",
                                         starttests = FALSE,
                                         kkt = FALSE))
                        )

PN.anac.mdl.slope = lmer(
    slope  ~ hasAnac + gender + (1 + hasAnac | speaker),
    data = PN_anac,
    control = lmerControl(optimizer = "optimx",
                          calc.derivs = FALSE,
                          optCtrl = list(method = "nlminb",
                                         starttests = FALSE,
                                         kkt = FALSE))
                         )

PN.anac.mdl.med = lmer(
    med  ~ hasAnac + gender + (1 + hasAnac | speaker),
    data = PN_anac,
    control = lmerControl(optimizer = "optimx",
                          calc.derivs = FALSE,
                          optCtrl = list(method = "nlminb",
                                         starttests = FALSE,
                                         kkt = FALSE))
                         )

PN.anac.mdl.mean = lmer(
    mean  ~ hasAnac + gender + (1 + hasAnac | speaker),data = PN_anac,
    control = lmerControl(optimizer = "optimx",
                          calc.derivs = FALSE,
                          optCtrl = list(method = "nlminb",
                                         starttests = FALSE,
                                         kkt = FALSE))
                        )


# AnalyseAndSaveModels -----------------------------------------------------------
PN.anac.mdl.L_t.sum = summary(PN.anac.mdl.L_t)
PN.anac.mdl.L_t.ano = anova(PN.anac.mdl.L_t)
PN.anac.mdl.L_t.step = step(PN.anac.mdl.L_t)
fileName = paste(folder, "PN.anac.mdl.L_t_", PN_Type, "_", syls[1], "syl.txt", sep = "")
capture.output(PN.anac.mdl.L_t.sum, file = fileName)
capture.output(PN.anac.mdl.L_t.ano, file = fileName, append =  TRUE)
capture.output(PN.anac.mdl.L_t.step, file = fileName, append =  TRUE)

PN.anac.mdl.H_t.sum = summary(PN.anac.mdl.H_t)
PN.anac.mdl.H_t.ano = anova(PN.anac.mdl.H_t)
PN.anac.mdl.H_t.step = step(PN.anac.mdl.H_t)
fileName = paste(folder, "PN.anac.mdl.H_t_", PN_Type, "_", syls[1], "syl.txt", sep = "")
capture.output(PN.anac.mdl.H_t.sum, file = fileName)
capture.output(PN.anac.mdl.H_t.ano, file = fileName, append =  TRUE)
capture.output(PN.anac.mdl.H_t.step, file = fileName, append =  TRUE)

PN.anac.mdl.D_t.sum = summary(PN.anac.mdl.D_t)
PN.anac.mdl.D_t.ano = anova(PN.anac.mdl.D_t)
PN.anac.mdl.D_t.step = step(PN.anac.mdl.D_t)
fileName = paste(folder, "PN.anac.mdl.D_t_", PN_Type, "_", syls[1], "syl.txt", sep = "")
capture.output(PN.anac.mdl.D_t.sum, file = fileName)
capture.output(PN.anac.mdl.D_t.ano, file = fileName, append =  TRUE)
capture.output(PN.anac.mdl.D_t.step, file = fileName, append =  TRUE)

PN.anac.mdl.L_fo.ano = anova(PN.anac.mdl.L_fo)
PN.anac.mdl.L_fo.sum = summary(PN.anac.mdl.L_fo)
PN.anac.mdl.L_fo.step = step(PN.anac.mdl.L_fo)
fileName = paste(folder, "PN.anac.mdl.L_fo_", PN_Type, "_", syls[1], "syl.txt", sep = "")
capture.output(PN.anac.mdl.L_fo.sum, file = fileName)
capture.output(PN.anac.mdl.L_fo.ano, file = fileName, append =  TRUE)
capture.output(PN.anac.mdl.L_fo.step, file = fileName, append =  TRUE)


PN.anac.mdl.H_fo.ano = anova(PN.anac.mdl.H_fo)
PN.anac.mdl.H_fo.sum = summary(PN.anac.mdl.H_fo)
PN.anac.mdl.H_fo.step = step(PN.anac.mdl.H_fo)
fileName = paste(folder, "PN.anac.mdl.H_fo_", PN_Type, "_", syls[1], "syl.txt", sep = "")
capture.output(PN.anac.mdl.H_fo.sum, file = fileName)
capture.output(PN.anac.mdl.H_fo.ano, file = fileName, append =  TRUE)
capture.output(PN.anac.mdl.H_fo.step, file = fileName, append =  TRUE)

PN.anac.mdl.R_fo.ano = anova(PN.anac.mdl.R_fo)
PN.anac.mdl.R_fo.sum = summary(PN.anac.mdl.R_fo)
PN.anac.mdl.R_fo.step = step(PN.anac.mdl.R_fo)
fileName = paste(folder, "PN.anac.mdl.R_fo_", PN_Type, "_", syls[1], "syl.txt", sep = "")
capture.output(PN.anac.mdl.R_fo.sum, file = fileName)
capture.output(PN.anac.mdl.R_fo.ano, file = fileName, append =  TRUE)
capture.output(PN.anac.mdl.R_fo.step, file = fileName, append =  TRUE)

PN.anac.mdl.mean.ano = anova(PN.anac.mdl.mean)
PN.anac.mdl.mean.sum = summary(PN.anac.mdl.mean)
PN.anac.mdl.mean.step = step(PN.anac.mdl.mean)
fileName = paste(folder, "PN.anac.mdl.mean_", PN_Type, "_", syls[1], "syl.txt", sep = "")
capture.output(PN.anac.mdl.mean.sum, file = fileName)
capture.output(PN.anac.mdl.mean.ano, file = fileName, append =  TRUE)
capture.output(PN.anac.mdl.mean.step, file = fileName, append =  TRUE)

PN.anac.mdl.med.ano = anova(PN.anac.mdl.med)
PN.anac.mdl.med.sum = summary(PN.anac.mdl.med)
PN.anac.mdl.med.step = step(PN.anac.mdl.med)
fileName = paste(folder, "PN.anac.mdl.med_", PN_Type, "_", syls[1], "syl.txt", sep = "")
capture.output(PN.anac.mdl.med.sum, file = fileName)
capture.output(PN.anac.mdl.med.ano, file = fileName, append =  TRUE)
capture.output(PN.anac.mdl.med.step, file = fileName, append =  TRUE)

PN.anac.mdl.slope.ano = anova(PN.anac.mdl.slope)
PN.anac.mdl.slope.sum = summary(PN.anac.mdl.slope)
PN.anac.mdl.slope.step = step(PN.anac.mdl.slope)
fileName = paste(folder, "PN.anac.mdl.slope_", PN_Type, "_", syls[1], "syl.txt", sep = "")
capture.output(PN.anac.mdl.slope.sum, file = fileName)
capture.output(PN.anac.mdl.slope.ano, file = fileName, append =  TRUE)
capture.output(PN.anac.mdl.slope.step, file = fileName, append =  TRUE)

