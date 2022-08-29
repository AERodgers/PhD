dir <- "PN_foot_modelOutput"
dir.create("PN_foot_modelOutput", showWarnings = FALSE)
#get PN-foot data
source("PN_foot_data_retrieval.R")

syls <- as.array(c("1", "2", "3", "4"))

i = 4
  # change intercept
  PN_foot$ft_syls <- factor(PN_foot$ft_syls, levels = c(syls[i], syls[-i]))
  # RUN STATISTICAL TESTS
  source("PN_foot_tests.R")

  PN.ft.model.L_t.sum = summary(PN.ft.model.L_t)
  PN.ft.model.L_t.ano = anova(PN.ft.model.L_t)
  fileName = paste("PN_foot_model6_1_output/1_PN.ft.model.L_t_", syls[i], "syl.txt", sep = "")
  capture.output(PN.ft.model.L_t.sum, file = fileName)
  capture.output(PN.ft.model.L_t.ano, file = fileName, append =  TRUE)
  
  tab_model(PN.ft.model.L_t,
            title = paste("L target time (ms),", syls[i], "as intercept"),
            show.icc = FALSE,
            file = paste("PN_foot_model6_1_output/1_PN.L_t.regression_", syls[i], "syl.html", sep = ""))
  plot_model(PN.ft.model.L_t,
             title = paste("L target time (ms),", syls[i], "as intercept"),
             show.intercept = FALSE,
             show.values = TRUE,
             #order.terms = c(2,3,4),
             vline.color = "red",
             colors = "Black")
  ggsave(file = paste("PN_foot_model6_1_output/1_PN.L_t.regression.plot_", syls[i], ".png", sep = ""),
         width = 15, height = 8, units = "cm")
  
  
  
  PN.ft.model.H_t.sum = summary(PN.ft.model.H_t)
  PN.ft.model.H_t.ano = anova(PN.ft.model.H_t)
  fileName = paste("PN_foot_model6_1_output/2_PN.ft.model.H_t_", syls[i], "syl.txt", sep = "")
  capture.output(PN.ft.model.H_t.sum, file = fileName)
  capture.output(PN.ft.model.H_t.ano, file = fileName, append =  TRUE)
  
  tab_model(PN.ft.model.H_t,
            title = paste("H target time (ms),", syls[i], "as intercept"),
            show.icc = FALSE,
            file = paste("PN_foot_model6_1_output/2_PN.H_t.regression_", syls[i], "syl.html", sep = ""))
  plot_model(PN.ft.model.H_t,
             title = paste("H target time (ms),", syls[i], "as intercept"),
             show.intercept = FALSE,
             show.values = TRUE,
             #order.terms = c(2,3,4),
             vline.color = "red",
             colors = "Black")
  ggsave(file = paste("PN_foot_model6_1_output/2_PN.H_t.regression.plot_", syls[i], ".png", sep = ""),
         width = 15, height = 8, units = "cm")
  
  
  
  PN.ft.model.L_fo.sum = summary(PN.ft.model.L_fo)
  PN.ft.model.L_fo.ano = anova(PN.ft.model.L_fo)
  fileName = paste("PN_foot_model6_1_output/3_PN.ft.model.L_fo_", syls[i], "syl.txt", sep = "")
  capture.output(PN.ft.model.L_fo.sum, file = fileName)
  capture.output(PN.ft.model.L_fo.ano, file = fileName, append =  TRUE)
  
  tab_model(PN.ft.model.L_fo,
            title = paste("L target fo (ST),", syls[i], "as intercept"),
            show.icc = FALSE,
            file = paste("PN_foot_model6_1_output/3_PN.L_fo.regression_", syls[i], "syl.html", sep = ""))
  plot_model(PN.ft.model.L_fo,
             title = paste("L target fo (ST),", syls[i], "as intercept"),
             show.intercept = FALSE,
             show.values = TRUE,
             #order.terms = c(2,3,4),
             vline.color = "red",
             colors = "Black")
  ggsave(file = paste("PN_foot_model6_1_output/3_PN.L_fo.regression.plot_", syls[i], ".png", sep = ""),
         width = 15, height = 8, units = "cm")
  
  
  PN.ft.model.H_fo.sum = summary(PN.ft.model.H_fo)
  PN.ft.model.H_fo.ano = anova(PN.ft.model.H_fo)
  fileName = paste("PN_foot_model6_1_output/4_PN.ft.model.H_fo_", syls[i], "syl.txt", sep = "")
  capture.output(PN.ft.model.H_fo.sum, file = fileName)
  capture.output(PN.ft.model.H_fo.ano, file = fileName, append =  TRUE)
  
  tab_model(PN.ft.model.H_fo,
            title = paste("H target fo (ST),", syls[i], "as intercept"),
            show.icc = FALSE,
            file = paste("PN_foot_model6_1_output/4_PN.H_fo.regression_", syls[i], "syl.html", sep = ""))
  plot_model(PN.ft.model.H_fo,
             title = paste("H target fo (ST),", syls[i], "as intercept"),
             show.intercept = FALSE,
             show.values = TRUE,
             #order.terms = c(2,3,4),
             vline.color = "red",
             colors = "Black")
  ggsave(file = paste("PN_foot_model6_1_output/4_PN.H_fo.regression.plot_", syls[i], ".png", sep = ""),
         width = 15, height = 8, units = "cm")
  
  
  
  PN.ft.model.med.sum = summary(PN.ft.model.med)
  PN.ft.model.med.ano = anova(PN.ft.model.med)
  fileName = paste("PN_foot_model6_1_output/5_PN.ft.model.med_", syls[i], "syl.txt", sep = "")
  capture.output(PN.ft.model.med.sum, file = fileName)
  capture.output(PN.ft.model.med.ano, file = fileName, append =  TRUE)
  
  
  tab_model(PN.ft.model.med,
            title = paste("Median fo (ST),", syls[i], "as intercept"),
            show.icc = FALSE,
            file = paste("PN_foot_model6_1_output/5_PN.med.regression_", syls[i], "syl.html", sep = ""))
  plot_model(PN.ft.model.med,
             title = paste("Median fo (ST),", syls[i], "as intercept"),
             show.intercept = FALSE,
             show.values = TRUE,
             #order.terms = c(2,3,4),
             vline.color = "red",
             colors = "Black")
  ggsave(file = paste("PN_foot_model6_1_output/5_PN.med.regression.plot_", syls[i], ".png", sep = ""),
         width = 15, height = 8, units = "cm")
  
  
  PN.ft.model.mean.sum = summary(PN.ft.model.mean)
  PN.ft.model.mean.ano = anova(PN.ft.model.mean)
  fileName = paste("PN_foot_model6_1_output/6_PN.ft.model.mean_", syls[i], "syl.txt", sep = "")
  capture.output(PN.ft.model.mean.sum, file = fileName)
  capture.output(PN.ft.model.mean.ano, file = fileName, append =  TRUE)
  
  tab_model(PN.ft.model.mean,
            title = paste("Mean fo (ST),", syls[i], "as intercept"),
            show.icc = FALSE,
            file = paste("PN_foot_model6_1_output/6_PN.mean.regression_", syls[i], "syl.html", sep = ""))
  plot_model(PN.ft.model.mean,
             title = paste("Mean fo (ST),", syls[i], "as intercept"),
             show.intercept = FALSE,
             show.values = TRUE,
             #order.terms = c(2,3,4),
             vline.color = "red",
             colors = "Black")
  ggsave(file = paste("PN_foot_model6_1_output/6_PN.mean.regression.plot_", syls[i], ".png", sep = ""),
         width = 15, height = 8, units = "cm")
  
  
  PN.ft.model.lh_slope.sum = summary(PN.ft.model.lh_slope)
  PN.ft.model.lh_slope.ano = anova(PN.ft.model.lh_slope)
  fileName = paste("PN_foot_model6_1_output/7_PN.ft.model.lh_slope_", syls[i], "syl.txt", sep = "")
  capture.output(PN.ft.model.lh_slope.sum, file = fileName)
  capture.output(PN.ft.model.lh_slope.ano, file = fileName, append =  TRUE)
  
  tab_model(PN.ft.model.lh_slope,
            title = paste("Slope (ST/sec),", syls[i], "as intercept"),
            show.icc = FALSE,
            file = paste("PN_foot_model6_1_output/7_PN.lh_slope.regression_", syls[i], "syl.html", sep = ""))
  plot_model(PN.ft.model.lh_slope,
             title = paste("Slope (ST/sec),", syls[i], "as intercept"),
             show.intercept = FALSE,
             show.values = TRUE,
             #order.terms = c(2,3,4),
             vline.color = "red",
             colors = "Black")
  ggsave(file = paste("PN_foot_model6_1_output/7_PN.lh_slope.regression.plot_", syls[i], ".png", sep = ""),
         width = 15, height = 8, units = "cm")

  
  
  PN.ft.model.R_fo.sum = summary(PN.ft.model.R_fo)
  PN.ft.model.R_fo.ano = anova(PN.ft.model.R_fo)
  fileName = paste("PN_foot_model6_1_output/8_PN.ft.model.R_fo_", syls[i], "syl.txt", sep = "")
  capture.output(PN.ft.model.R_fo.sum, file = fileName)
  capture.output(PN.ft.model.R_fo.ano, file = fileName, append =  TRUE)
  
  tab_model(PN.ft.model.R_fo,
            title = paste("Range (ST),", syls[i], "as intercept"),
            show.icc = FALSE,
            file = paste("PN_foot_model6_1_output/8_PN.R_fo.regression_", syls[i], "syl.html", sep = ""))
  plot_model(PN.ft.model.R_fo,
             title = paste("Range (ST),", syls[i], "as intercept"),
             show.intercept = FALSE,
             show.values = TRUE,
             #order.terms = c(2,3,4),
             vline.color = "red",
             colors = "Black")
  ggsave(file = paste("PN_foot_model6_1_output/8_PN.R_fo.regression.plot_", syls[i], ".png", sep = ""),
         width = 15, height = 8, units = "cm")
  
  
  
  PN.ft.model.D_t.sum = summary(PN.ft.model.D_t)
  PN.ft.model.D_t.ano = anova(PN.ft.model.D_t)
  fileName = paste("PN_foot_model6_1_output/9_PN.ft.model.D_t_", syls[i], "syl.txt", sep = "")
  capture.output(PN.ft.model.D_t.sum, file = fileName)
  capture.output(PN.ft.model.D_t.ano, file = fileName, append =  TRUE)
  
  tab_model(PN.ft.model.D_t,
            title = paste("L to H time (ms),", syls[i], "as intercept"),
            show.icc = FALSE,
            file = paste("PN_foot_model6_1_output/9_PN.D_t.regression_", syls[i], "syl.html", sep = ""))
  plot_model(PN.ft.model.D_t,
             title = paste("L to H time (ms),", syls[i], "as intercept"),
             show.intercept = FALSE,
             show.values = TRUE,
             #order.terms = c(2,3,4),
             vline.color = "red",
             colors = "Black")
  ggsave(file = paste("PN_foot_model6_1_output/9_PN.D_t.regression.plot_", syls[i], ".png", sep = ""),
         width = 15, height = 8, units = "cm")
  
  
