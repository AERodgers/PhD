dir.create("PN_anac_modelOutput", showWarnings = FALSE)
#get PN-foot data
source("PN_anac_data_retrieval.R")

syls <- as.array(c("0", "1", "2", "3"))

i = 1
  # change intercept
  PN_anac$ana_syls <- factor(PN_anac$ana_syls, levels = c(syls[i], syls[-i]))
  # RUN STATISTICAL TESTS
  source("PN_anac_tests.R")

  PN.anac.model.L_t.sum = summary(PN.anac.model.L_t)
  PN.anac.model.L_t.ano = anova(PN.anac.model.L_t)
  fileName = paste("PN_anac_model6_1_output/1_PN.anac.model.L_t_", syls[i], "syl.txt", sep = "")
  capture.output(PN.anac.model.L_t.sum, file = fileName)
  capture.output(PN.anac.model.L_t.ano, file = fileName, append =  TRUE)

  tab_model(PN.anac.model.L_t,
            title = paste("L target time (ms),", syls[i], "as intercept"),
            show.icc = FALSE,
            file = paste("PN_anac_model6_1_output/1_PN.L_t.regression_", syls[i], "syl.html", sep = ""))
  plot_model(PN.anac.model.L_t,
             title = paste("L target time (ms),", syls[i], "as intercept"),
             show.intercept = FALSE,
             show.values = TRUE,
             #order.terms = c(2,3,4),
             vline.color = "red",
             colors = "Black")
  ggsave(file = paste("PN_anac_model6_1_output/1_PN.L_t.regression.plot_", syls[i], ".png", sep = ""),
         width = 15, height = 8, units = "cm")



  PN.anac.model.H_t.sum = summary(PN.anac.model.H_t)
  PN.anac.model.H_t.ano = anova(PN.anac.model.H_t)
  fileName = paste("PN_anac_model6_1_output/2_PN.anac.model.H_t_", syls[i], "syl.txt", sep = "")
  capture.output(PN.anac.model.H_t.sum, file = fileName)
  capture.output(PN.anac.model.H_t.ano, file = fileName, append =  TRUE)

  tab_model(PN.anac.model.H_t,
            title = paste("H target time (ms),", syls[i], "as intercept"),
            show.icc = FALSE,
            file = paste("PN_anac_model6_1_output/2_PN.H_t.regression_", syls[i], "syl.html", sep = ""))
  plot_model(PN.anac.model.H_t,
             title = paste("H target time (ms),", syls[i], "as intercept"),
             show.intercept = FALSE,
             show.values = TRUE,
             #order.terms = c(2,3,4),
             vline.color = "red",
             colors = "Black")
  ggsave(file = paste("PN_anac_model6_1_output/2_PN.H_t.regression.plot_", syls[i], ".png", sep = ""),
         width = 15, height = 8, units = "cm")



  PN.anac.model.L_fo.sum = summary(PN.anac.model.L_fo)
  PN.anac.model.L_fo.ano = anova(PN.anac.model.L_fo)
  fileName = paste("PN_anac_model6_1_output/3_PN.anac.model.L_fo_", syls[i], "syl.txt", sep = "")
  capture.output(PN.anac.model.L_fo.sum, file = fileName)
  capture.output(PN.anac.model.L_fo.ano, file = fileName, append =  TRUE)

  tab_model(PN.anac.model.L_fo,
            title = paste("L target fo (ST),", syls[i], "as intercept"),
            show.icc = FALSE,
            file = paste("PN_anac_model6_1_output/3_PN.L_fo.regression_", syls[i], "syl.html", sep = ""))
  plot_model(PN.anac.model.L_fo,
             title = paste("L target fo (ST),", syls[i], "as intercept"),
             show.intercept = FALSE,
             show.values = TRUE,
             #order.terms = c(2,3,4),
             vline.color = "red",
             colors = "Black")
  ggsave(file = paste("PN_anac_model6_1_output/3_PN.L_fo.regression.plot_", syls[i], ".png", sep = ""),
         width = 15, height = 8, units = "cm")


  PN.anac.model.H_fo.sum = summary(PN.anac.model.H_fo)
  PN.anac.model.H_fo.ano = anova(PN.anac.model.H_fo)
  fileName = paste("PN_anac_model6_1_output/4_PN.anac.model.H_fo_", syls[i], "syl.txt", sep = "")
  capture.output(PN.anac.model.H_fo.sum, file = fileName)
  capture.output(PN.anac.model.H_fo.ano, file = fileName, append =  TRUE)

  tab_model(PN.anac.model.H_fo,
            title = paste("H target fo (ST),", syls[i], "as intercept"),
            show.icc = FALSE,
            file = paste("PN_anac_model6_1_output/4_PN.H_fo.regression_", syls[i], "syl.html", sep = ""))
  plot_model(PN.anac.model.H_fo,
             title = paste("H target fo (ST),", syls[i], "as intercept"),
             show.intercept = FALSE,
             show.values = TRUE,
             #order.terms = c(2,3,4),
             vline.color = "red",
             colors = "Black")
  ggsave(file = paste("PN_anac_model6_1_output/4_PN.H_fo.regression.plot_", syls[i], ".png", sep = ""),
         width = 15, height = 8, units = "cm")



  PN.anac.model.med.sum = summary(PN.anac.model.med)
  PN.anac.model.med.ano = anova(PN.anac.model.med)
  fileName = paste("PN_anac_model6_1_output/5_PN.anac.model.med_", syls[i], "syl.txt", sep = "")
  capture.output(PN.anac.model.med.sum, file = fileName)
  capture.output(PN.anac.model.med.ano, file = fileName, append =  TRUE)


  tab_model(PN.anac.model.med,
            title = paste("Median fo (ST),", syls[i], "as intercept"),
            show.icc = FALSE,
            file = paste("PN_anac_model6_1_output/5_PN.med.regression_", syls[i], "syl.html", sep = ""))
  plot_model(PN.anac.model.med,
             title = paste("Median fo (ST),", syls[i], "as intercept"),
             show.intercept = FALSE,
             show.values = TRUE,
             #order.terms = c(2,3,4),
             vline.color = "red",
             colors = "Black")
  ggsave(file = paste("PN_anac_model6_1_output/5_PN.med.regression.plot_", syls[i], ".png", sep = ""),
         width = 15, height = 8, units = "cm")


  PN.anac.model.mean.sum = summary(PN.anac.model.mean)
  PN.anac.model.mean.ano = anova(PN.anac.model.mean)
  fileName = paste("PN_anac_model6_1_output/6_PN.anac.model.mean_", syls[i], "syl.txt", sep = "")
  capture.output(PN.anac.model.mean.sum, file = fileName)
  capture.output(PN.anac.model.mean.ano, file = fileName, append =  TRUE)

  tab_model(PN.anac.model.mean,
            title = paste("Mean fo (ST),", syls[i], "as intercept"),
            show.icc = FALSE,
            file = paste("PN_anac_model6_1_output/6_PN.mean.regression_", syls[i], "syl.html", sep = ""))
  plot_model(PN.anac.model.mean,
             title = paste("Mean fo (ST),", syls[i], "as intercept"),
             show.intercept = FALSE,
             show.values = TRUE,
             #order.terms = c(2,3,4),
             vline.color = "red",
             colors = "Black")
  ggsave(file = paste("PN_anac_model6_1_output/6_PN.mean.regression.plot_", syls[i], ".png", sep = ""),
         width = 15, height = 8, units = "cm")


  PN.anac.model.lh_slope.sum = summary(PN.anac.model.lh_slope)
  PN.anac.model.lh_slope.ano = anova(PN.anac.model.lh_slope)
  fileName = paste("PN_anac_model6_1_output/7_PN.anac.model.lh_slope_", syls[i], "syl.txt", sep = "")
  capture.output(PN.anac.model.lh_slope.sum, file = fileName)
  capture.output(PN.anac.model.lh_slope.ano, file = fileName, append =  TRUE)

  tab_model(PN.anac.model.lh_slope,
            title = paste("Slope (ST/sec),", syls[i], "as intercept"),
            show.icc = FALSE,
            file = paste("PN_anac_model6_1_output/7_PN.lh_slope.regression_", syls[i], "syl.html", sep = ""))
  plot_model(PN.anac.model.lh_slope,
             title = paste("Slope (ST/sec),", syls[i], "as intercept"),
             show.intercept = FALSE,
             show.values = TRUE,
             #order.terms = c(2,3,4),
             vline.color = "red",
             colors = "Black")
  ggsave(file = paste("PN_anac_model6_1_output/7_PN.lh_slope.regression.plot_", syls[i], ".png", sep = ""),
         width = 15, height = 8, units = "cm")



  PN.anac.model.R_fo.sum = summary(PN.anac.model.R_fo)
  PN.anac.model.R_fo.ano = anova(PN.anac.model.R_fo)
  fileName = paste("PN_anac_model6_1_output/7_PN.anac.model.R_fo_", syls[i], "syl.txt", sep = "")
  capture.output(PN.anac.model.R_fo.sum, file = fileName)
  capture.output(PN.anac.model.R_fo.ano, file = fileName, append =  TRUE)

  tab_model(PN.anac.model.R_fo,
            title = paste("Range (ST),", syls[i], "as intercept"),
            show.icc = FALSE,
            file = paste("PN_anac_model6_1_output/8_PN.R_fo.regression_", syls[i], "syl.html", sep = ""))
  plot_model(PN.anac.model.R_fo,
             title = paste("Range (ST),", syls[i], "as intercept"),
             show.intercept = FALSE,
             show.values = TRUE,
             #order.terms = c(2,3,4),
             vline.color = "red",
             colors = "Black")
  ggsave(file = paste("PN_anac_model6_1_output/8_PN.R_fo.regression.plot_", syls[i], ".png", sep = ""),
         width = 15, height = 8, units = "cm")
