# SCRIPT RUNS LMER (Linear Mixed Model) ANALYSIS ON PN_ANAC DATA FRAME

# load libraries
library(lmerTest)
library(optimx)
library(sjPlot)
library(ggplot2)


# CREATE DIRECTORIES (IF DON'T EXIST)
folder = "Output_PN_model/ANAC0123all/"
dir.create("Output_PN_model", showWarnings = FALSE)
dir.create(folder, showWarnings = FALSE)

PN_anac <- pn_anac

PN_Type = "all"                                                                # all

write.table(PN_anac, paste("data/PN_anac_", PN_Type, ".txt", sep =), sep = "\t")

# SET INTERCEPT PERMUTATION ARRAY
syls <- as.array(c(3, 0, 1, 2))

# RUN ANALYSIS LOOP
for(i in 1:1){
    # resort order of factors in ana_syls
    syls <- c(syls[2:4], syls[1])

    PN_anac$ana_syls <- factor(PN_anac$ana_syls, levels = syls)

    # RunModels ---------------------------------------------------------------
    PN.anac.mdl.H_t = lmer(H_t ~ ana_syls + gender + (1 + ana_syls  | speaker),
                           data = PN_anac,
                           control = lmerControl(
                               optimizer = "optimx",
                               calc.derivs = FALSE,
                               optCtrl = list(method = "nlminb",
                                              starttests = FALSE,
                                              kkt = FALSE)
                           )
    )


    # only works with subset when i == 1 in L*H
    PN.anac.mdl.H_fo = lmer(
        H_fo ~ ana_syls + gender + (1 + ana_syls  | speaker),
        data = PN_anac,
        control = lmerControl(optimizer = "optimx",
                              calc.derivs = FALSE,
                              optCtrl = list(method = "nlminb",
                                             starttests = FALSE,
                                             kkt = FALSE))
    )

    if (PN_Type == "LH"){
    PN.anac.mdl.L_t = lmer(L_t ~ ana_syls + gender + (1 + ana_syls  | speaker),
                           data = PN_anac,
                           control = lmerControl(
                               optimizer = "optimx",
                               calc.derivs = FALSE,
                               optCtrl = list(method = "nlminb",
                                              starttests = FALSE,
                                              kkt = FALSE)
                                                 )
                           )


    # only works with intercepts only model when i == 2 for L*H
    PN.anac.mdl.D_t = lmer(D_t ~ ana_syls + gender + (1 + ana_syls  | speaker),
                                        data = PN_anac,
                                        control = lmerControl(
                                            optimizer = "optimx",
                                            calc.derivs = FALSE,
                                            optCtrl = list(method = "nlminb",
                                                           starttests = FALSE,
                                                           kkt = FALSE)
                                                              )
                                        )

#   if (i == 2){
#               PN.anac.mdl.D_t = lmer(
#                   D_t ~ ana_syls + gender + (1 + ana_syls  | speaker),
#                   data = PN_anac,
#                   subset = abs(scale(resid(PN.anac.mdl.D_t)))<2.5,
#                   control = lmerControl(optimizer = "optimx",
#                                         calc.derivs = FALSE,
#                                         optCtrl = list(method = "nlminb",
#                                                        starttests = FALSE,
#                                                        kkt = FALSE))
#                                         )
#                }


    PN.anac.mdl.L_fo = lmer(
        L_fo  ~ ana_syls + gender + (1 + ana_syls  | speaker),
        data = PN_anac,
        control = lmerControl(optimizer = "optimx",
                              calc.derivs = FALSE,
                              optCtrl = list(method = "nlminb",
                                             starttests = FALSE,
                                             kkt = FALSE))
                            )



        # only works with intercepts only model when i == 2 for L*H
        PN.anac.mdl.R_fo = lmer(
        R_fo ~ ana_syls + gender + (1 + ana_syls  | speaker),
        data = PN_anac,
        control = lmerControl(optimizer = "optimx",
                              calc.derivs = FALSE,
                              optCtrl = list(method = "nlminb",
                                             starttests = FALSE,
                                             kkt = FALSE))
                            )

    # only works with subset when i == 1 and i-cept only mode when i = 2 in L*H
    PN.anac.mdl.lh_slope = lmer(
        lh_slope  ~ ana_syls + gender + (1 + ana_syls  | speaker),
        data = PN_anac,
        control = lmerControl(optimizer = "optimx",
                              calc.derivs = FALSE,
                              optCtrl = list(method = "nlminb",
                                             starttests = FALSE,
                                             kkt = FALSE))
                             )

    PN.anac.mdl.med = lmer(
        med  ~ ana_syls + gender + (1 + ana_syls  | speaker),
        data = PN_anac,
        control = lmerControl(optimizer = "optimx",
                              calc.derivs = FALSE,
                              optCtrl = list(method = "nlminb",
                                             starttests = FALSE,
                                             kkt = FALSE))
                             )

    PN.anac.mdl.mean = lmer(
        mean  ~ ana_syls + gender + (1 + ana_syls  | speaker),data = PN_anac,
        control = lmerControl(optimizer = "optimx",
                              calc.derivs = FALSE,
                              optCtrl = list(method = "nlminb",
                                             starttests = FALSE,
                                             kkt = FALSE)))}


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

    if (PN_Type == "LH"){
    PN.anac.mdl.L_t.sum = summary(PN.anac.mdl.L_t)
    PN.anac.mdl.L_t.ano = anova(PN.anac.mdl.L_t)
    PN.anac.mdl.L_t.step = step(PN.anac.mdl.L_t)
    fileName = paste(folder, "PN.anac.mdl.L_t_", PN_Type, "_", syls[1], "syl.txt", sep = "")
    capture.output(PN.anac.mdl.L_t.sum, file = fileName)
    capture.output(PN.anac.mdl.L_t.ano, file = fileName, append =  TRUE)
    capture.output(PN.anac.mdl.L_t.step, file = fileName, append =  TRUE)


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

    PN.anac.mdl.lh_slope.ano = anova(PN.anac.mdl.lh_slope)
    PN.anac.mdl.lh_slope.sum = summary(PN.anac.mdl.lh_slope)
    PN.anac.mdl.lh_slope.step = step(PN.anac.mdl.lh_slope)
    fileName = paste(folder, "PN.anac.mdl.lh_slope_", PN_Type, "_", syls[1], "syl.txt", sep = "")
    capture.output(PN.anac.mdl.lh_slope.sum, file = fileName)
    capture.output(PN.anac.mdl.lh_slope.ano, file = fileName, append =  TRUE)
    capture.output(PN.anac.mdl.lh_slope.step, file = fileName, append =  TRUE)}
    }
