# SCRIPT RUNS LMER (Linear Mixed Model) ANALYSIS ON PN_FOOT DATA FRAME

# load libraries
library(lmerTest)
library(optimx)
library(sjPlot)
library(ggplot2)

# CREATE DIRECTORIES (IF DON'T EXIST)
folder = "Output_PN_model/FOOT/"
dir.create("Output_PN_model", showWarnings = FALSE)
dir.create(folder, showWarnings = FALSE)


# GET DATA FRAME FOR SUBSECTION OF CORPUS WITH PRECEDING COUNT AS MAIN FACTOR
load("~/github/R-Alignment_Analysis_One/data/PN_foot.RData")
write.table(PN_foot, "data/PN_foot.txt", sep = "\t")

# SET INTERCEPT PERMUTATION ARRAY
syls <- as.array(c("4", "1", "2", "3"))

# RUN ANALYSIS LOOP
for(i in 1:4){
    # resort order of factors in ft_syls
    syls <- c(syls[2:4], syls[1])

    PN_foot$ft_syls <- factor(PN_foot$ft_syls, levels = syls)

    # RunModels ---------------------------------------------------------------

    PN.ft.mdl.L_t = lmer(L_t ~ ft_syls + gender + (1 + ft_syls | speaker),
                         data = PN_foot,
                         control = lmerControl(
                             optimizer = "optimx",
                             calc.derivs = FALSE,
                             optCtrl = list(method = "nlminb",
                                            starttests = FALSE,
                                            kkt = FALSE)
                             )
                         )
    # i = 4 --> data needs trimming
    if (i == 4){
                PN.ft.mdl.L_t = lmer(
                    L_t ~ ft_syls + gender + (1 + ft_syls | speaker),
                    data = PN_foot,
                    subset = abs(scale(resid(PN.ft.mdl.L_t)))<2.5,
                    control = lmerControl(
                        optimizer = "optimx",
                        calc.derivs = FALSE,
                        optCtrl = list(method = "nlminb",
                                       starttests = FALSE,
                                       kkt = FALSE)
                    )
                    )
                }

    PN.ft.mdl.H_t = lmer(H_t ~ ft_syls + gender + (1 + ft_syls | speaker),
                         data = PN_foot,
                         control = lmerControl(
                                           optimizer = "optimx",
                                           calc.derivs = FALSE,
                                           optCtrl = list(method = "nlminb",
                                                          starttests = FALSE,
                                                          kkt = FALSE)
                                           )
                         )

    PN.ft.mdl.D_t = lmer(D_t ~ ft_syls + gender + (1 + ft_syls | speaker),
                         data = PN_foot,
                         control = lmerControl(
                             optimizer = "optimx",
                             calc.derivs = FALSE,
                             optCtrl = list(method = "nlminb",
                                            starttests = FALSE,
                                            kkt = FALSE)
                                            )
                         )

    PN.ft.mdl.L_fo = lmer(L_fo  ~ ft_syls + gender + (1 + ft_syls | speaker),
                          data = PN_foot,
                          control = lmerControl(optimizer = "optimx",
                                                calc.derivs = FALSE,
                                                optCtrl = list(
                                                    method = "nlminb",
                                                    starttests = FALSE,
                                                    kkt = FALSE)
                                                )
                          )

# i = 1 == problem for non-trimmed set
    PN.ft.mdl.H_fo = lmer(H_fo ~ ft_syls + gender + (1 + ft_syls | speaker),
                          data = PN_foot,
                          control = lmerControl(optimizer = "optimx",
                                                calc.derivs = FALSE,
                                                optCtrl = list(
                                                    method = "nlminb",
                                                    starttests = FALSE,
                                                    kkt = FALSE)
                                                )
                          )
    if (i == 1){
        PN.ft.mdl.H_fo = lmer(H_fo ~ ft_syls + gender + (1 + ft_syls | speaker),
                              data = PN_foot,
                              subset = abs(scale(resid(PN.ft.mdl.H_fo)))<2.5,
                              control = lmerControl(optimizer = "optimx",
                                                    calc.derivs = FALSE,
                                                    optCtrl = list(
                                                        method = "nlminb",
                                                        starttests = FALSE,
                                                        kkt = FALSE)
                                                    )
                            )
                }

    # i = 3 == problem for untrimmed version
    PN.ft.mdl.R_fo = lmer(R_fo ~ ft_syls + gender + (1 + ft_syls | speaker),
                          data = PN_foot,
                          control = lmerControl(optimizer = "optimx",
                                                calc.derivs = FALSE,
                                                optCtrl = list(
                                                    method = "nlminb",
                                                    starttests = FALSE,
                                                    kkt = FALSE)
                                                )
                          )
    if (i == 3){
        PN.ft.mdl.R_fo = lmer(R_fo ~ ft_syls + gender + (1 + ft_syls | speaker),
                          data = PN_foot,
                          subset = abs(scale(resid(PN.ft.mdl.R_fo)))<2.5,
                          control = lmerControl(optimizer = "optimx",
                                                calc.derivs = FALSE,
                                                optCtrl = list(
                                                    method = "nlminb",
                                                    starttests = FALSE,
                                                    kkt = FALSE)
                                                )
                          )
                }

    # i = 3 == problem for untrimmed version
    PN.ft.mdl.lh_slope = lmer(lh_slope  ~ ft_syls + gender + (1 + ft_syls | speaker),
                           data = PN_foot,
                           control = lmerControl(optimizer = "optimx",
                                                 calc.derivs = FALSE,
                                                 optCtrl = list(
                                                     method = "nlminb",
                                                     starttests = FALSE,
                                                     kkt = FALSE)
                                                 )
                           )
    if (i == 3){
                PN.ft.mdl.lh_slope = lmer(
                    lh_slope  ~ ft_syls + gender + (1 + ft_syls | speaker),
                    data = PN_foot,
                    subset = abs(scale(resid(PN.ft.mdl.lh_slope)))<2.5,
                    control = lmerControl(optimizer = "optimx",
                                          calc.derivs = FALSE,
                                          optCtrl = list(
                                              method = "nlminb",
                                              starttests = FALSE,
                                              kkt = FALSE))
                                       )
                }

    PN.ft.mdl.med = lmer(med  ~ ft_syls + gender + (1 + ft_syls | speaker),
                         data = PN_foot,
                         control = lmerControl(optimizer = "optimx",
                                               calc.derivs = FALSE,
                                               optCtrl = list(
                                                   method = "nlminb",
                                                   starttests = FALSE,
                                                   kkt = FALSE))

                         )

    PN.ft.mdl.mean = lmer(mean  ~ ft_syls + gender + (1 + ft_syls | speaker),
                          data = PN_foot,
                          control = lmerControl(optimizer = "optimx",
                                                calc.derivs = FALSE,
                                                optCtrl = list(
                                                    method = "nlminb",
                                                    starttests = FALSE,
                                                    kkt = FALSE)
                                                )
                          )


    # AnalyseAndSaveModels -----------------------------------------------------------
    PN.ft.mdl.L_t.sum = summary(PN.ft.mdl.L_t)
    PN.ft.mdl.L_t.ano = anova(PN.ft.mdl.L_t)
    fileName = paste(folder, "PN.ft.mdl.L_t_", syls[i], "syl.txt", sep = "")
    capture.output(PN.ft.mdl.L_t.sum, file = fileName)
    capture.output(PN.ft.mdl.L_t.ano, file = fileName, append =  TRUE)

    PN.ft.mdl.H_t.sum = summary(PN.ft.mdl.H_t)
    PN.ft.mdl.H_t.ano = anova(PN.ft.mdl.H_t)
    fileName = paste(folder, "PN.ft.mdl.H_t_", syls[i], "syl.txt", sep = "")
    capture.output(PN.ft.mdl.H_t.sum, file = fileName)
    capture.output(PN.ft.mdl.H_t.ano, file = fileName, append =  TRUE)

    PN.ft.mdl.D_t.sum = summary(PN.ft.mdl.D_t)
    PN.ft.mdl.D_t.ano = anova(PN.ft.mdl.D_t)
    fileName = paste(folder, "PN.ft.mdl.D_t_", syls[i], "syl.txt", sep = "")
    capture.output(PN.ft.mdl.D_t.sum, file = fileName)
    capture.output(PN.ft.mdl.D_t.ano, file = fileName, append =  TRUE)

    PN.ft.mdl.L_fo.ano = anova(PN.ft.mdl.L_fo)
    PN.ft.mdl.L_fo.sum = summary(PN.ft.mdl.L_fo)
    fileName = paste(folder, "PN.ft.mdl.L_fo_", syls[i], "syl.txt", sep = "")
    capture.output(PN.ft.mdl.L_fo.sum, file = fileName)
    capture.output(PN.ft.mdl.L_fo.ano, file = fileName, append =  TRUE)


    PN.ft.mdl.H_fo.ano = anova(PN.ft.mdl.H_fo)
    PN.ft.mdl.H_fo.sum = summary(PN.ft.mdl.H_fo)
    fileName = paste(folder, "PN.ft.mdl.H_fo_", syls[i], "syl.txt", sep = "")
    capture.output(PN.ft.mdl.H_fo.sum, file = fileName)
    capture.output(PN.ft.mdl.H_fo.ano, file = fileName, append =  TRUE)

    PN.ft.mdl.R_fo.ano = anova(PN.ft.mdl.R_fo)
    PN.ft.mdl.R_fo.sum = summary(PN.ft.mdl.R_fo)
    fileName = paste(folder, "PN.ft.mdl.R_fo_", syls[i], "syl.txt", sep = "")
    capture.output(PN.ft.mdl.R_fo.sum, file = fileName)
    capture.output(PN.ft.mdl.R_fo.ano, file = fileName, append =  TRUE)

    PN.ft.mdl.mean.ano = anova(PN.ft.mdl.mean)
    PN.ft.mdl.mean.sum = summary(PN.ft.mdl.mean)
    fileName = paste(folder, "PN.ft.mdl.mean_", syls[i], "syl.txt", sep = "")
    capture.output(PN.ft.mdl.mean.sum, file = fileName)
    capture.output(PN.ft.mdl.mean.ano, file = fileName, append =  TRUE)

    PN.ft.mdl.med.ano = anova(PN.ft.mdl.med)
    PN.ft.mdl.med.sum = summary(PN.ft.mdl.med)
    fileName = paste(folder, "PN.ft.mdl.med_", syls[i], "syl.txt", sep = "")
    capture.output(PN.ft.mdl.med.sum, file = fileName)
    capture.output(PN.ft.mdl.med.ano, file = fileName, append =  TRUE)

    PN.ft.mdl.lh_slope.ano = anova(PN.ft.mdl.lh_slope)
    PN.ft.mdl.lh_slope.sum = summary(PN.ft.mdl.lh_slope)
    fileName = paste(folder, "PN.ft.mdl.lh_slope_", syls[i], "syl.txt", sep = "")
    capture.output(PN.ft.mdl.lh_slope.sum, file = fileName)
    capture.output(PN.ft.mdl.lh_slope.ano, file = fileName, append =  TRUE)

}
