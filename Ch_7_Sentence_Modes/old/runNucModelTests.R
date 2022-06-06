library(lmerTest)
library(optimx)
library(sjPlot)
library(ggplot2)
source('getNucTable.R')
order <- as.array(c("DEC", "WHQ", "YNQ", "DCQ", "WHQ", "YNQ", "DCQ", "DEC", "YNQ", "DCQ", "DEC", "WHQ", "DCQ", "DEC", "WHQ",  "YNQ"))
dim(order) <- c(4, 4)

for(i in 1:4)
  {
  nucs$mode <- factor(nucs$mode, levels = order[,i])
  curIcpt = order[1,i]

  nuc.model.L   = lmer(L ~ mode + (1 + mode | speaker) + (1| fin_phon) + (1 | frame), data = nucs,
                       control = lmerControl(optimizer = "optimx", calc.derivs = FALSE,
                       optCtrl = list(method = "nlminb", starttests = FALSE, kkt = FALSE)))
  nuc.model.L_t = lmer(L_t ~ mode + (1 + mode | speaker) + (1| fin_phon) + (1 | frame), data = nucs,
                       control = lmerControl(optimizer = "optimx", calc.derivs = FALSE,
                       optCtrl = list(method = "nlminb", starttests = FALSE, kkt = FALSE)))
  nuc.model.X   = lmer(X ~ mode + (1 + mode | speaker) + (1| fin_phon) + (1 | frame), data = nucs,
                       control = lmerControl(optimizer = "optimx", calc.derivs = FALSE,
                       optCtrl = list(method = "nlminb", starttests = FALSE, kkt = FALSE)))
  nuc.model.H   = lmer(H ~ mode + (1 + mode | speaker) + (1| fin_phon) + (1 | frame), data = nucs,
                       control = lmerControl(optimizer = "optimx", calc.derivs = FALSE,
                       optCtrl = list(method = "nlminb", starttests = FALSE, kkt = FALSE)))

  #trim nuc.model.H if DEC is intercept
  if(i == 1)
    {
    print("intercept == DEC, output = H; fails to converge, trimming data")
    nuc.model.H = lmer(H ~ mode + (1 + mode | speaker) + (1| fin_phon) + (1 | frame), data = nucs,
                               subset = abs(scale(resid(nuc.model.H)))<2.5,
                               control = lmerControl(optimizer = "optimx", calc.derivs = FALSE,
                               optCtrl = list(method = "nlminb", starttests = FALSE, kkt = FALSE)))
    }

  nuc.model.H_t = lmer(H_t ~ mode + (1 + mode | speaker) + (1| fin_phon) + (1 | frame), data = nucs,
                       control = lmerControl(optimizer = "optimx", calc.derivs = FALSE,
                       optCtrl = list(method = "nlminb", starttests = FALSE, kkt = FALSE)))
  nuc.model.S   = lmer(S ~ mode + (1 + mode | speaker) + (1| fin_phon) + (1 | frame), data = nucs,
                       control = lmerControl(optimizer = "optimx", calc.derivs = FALSE,
                       optCtrl = list(method = "nlminb", starttests = FALSE, kkt = FALSE)))
  nuc.model.D   = lmer(D ~ mode + (1  | speaker) + (1| fin_phon) + (1 | frame), data = nucs,
                       control = lmerControl(optimizer = "optimx", calc.derivs = FALSE,
                       optCtrl = list(method = "nlminb", starttests = FALSE, kkt = FALSE)))
  nuc.model.K   = lmer(K ~ mode + (1 + mode | speaker) + (1| fin_phon) + (1 | frame), data = nucs,
                       control = lmerControl(optimizer = "optimx", calc.derivs = FALSE,
                       optCtrl = list(method = "nlminb", starttests = FALSE, kkt = FALSE)))


  source('printModelTestSummaries.R')
  tab_model(nuc.model.L,
            title = paste("L-target F0 (semitones),", curIcpt, "as intercept"),
            show.icc = FALSE,
            file = paste("tables/1NUC_L_regression_table_", curIcpt, ".html", sep = ""))
  tab_model(nuc.model.H,
            title = paste("H-target F0 (semitones),", curIcpt, "as intercept"),
            show.icc = FALSE,
            file = paste("tables/2NUC_H_regression_table_", curIcpt, ".html", sep = ""))
  tab_model(nuc.model.X,
            title = paste("F0 excursion size (semitones), ", curIcpt, " as intercept"),
            show.icc = FALSE,
            file = paste("tables/3NUC_X_regression_table_", curIcpt, ".html", sep = ""))
  tab_model(nuc.model.L_t,
            title = paste("L-target time (ms),", curIcpt, "as intercept"),
            show.icc = FALSE,
            file = paste("tables/5NUC_L_t_regression_table_", curIcpt, ".html", sep = ""))
  tab_model(nuc.model.H_t,
            title = paste("H-target time (ms),", curIcpt, "as intercept"),
            show.icc = FALSE,
            file = paste("tables/6NUC_H_t_regression_table_", curIcpt, ".html", sep = ""))
  tab_model(nuc.model.S,
            title = paste("Slope (semitones/sec),", curIcpt, "as intercept"),
            show.icc = FALSE,
            file = paste("tables/8NUC_S_regression_table_", curIcpt, ".html", sep = ""))
  tab_model(nuc.model.D,
            title = paste("Distance from L to H target (secs),", curIcpt, "as intercept"),
            show.icc = FALSE,
            file = paste("tables/7NUC_D_t_regression_table_", curIcpt, ".html", sep = ""))
  tab_model(nuc.model.K,
            title = paste("Mean L and H F0 (semitones),", curIcpt, "as intercept"),
            show.icc = FALSE,
            file = paste("tables/4NUC_K_regression_table_", curIcpt, ".html", sep = ""))

  plot_model(nuc.model.L,
             title = paste("L-target F0 (semitones),", curIcpt, "as intercept"),
             show.intercept = FALSE,
             show.values = TRUE,
             order.terms = c(2,3,4),
             vline.color = "red",
             colors = "Black")
  ggsave(file = paste("tables/1NUC_L_regression_plot_", curIcpt, ".png", sep = ""),
         width = 15, height = 8, units = "cm")


  plot_model(nuc.model.H,
             title = paste("H-target F0 (semitones),", curIcpt, "as intercept"),
             show.intercept = FALSE,
             show.values = TRUE,
             order.terms = c(2,3,4),
             vline.color = "red",
             colors = "Black")
  ggsave(file = paste("tables/2NUC_H_regression_plot_", curIcpt, ".png", sep = ""),
         width = 15, height = 8, units = "cm")


  plot_model(nuc.model.X,
             title = paste("F0 excursion size (semitones),", curIcpt, "as intercept"),
             show.intercept = FALSE,
             show.values = TRUE,
             order.terms = c(2,3,4),
             vline.color = "red",
             colors = "Black")
  ggsave(file = paste("tables/3NUC_X_regression_plot_", curIcpt, ".png", sep = ""),
         width = 15, height = 8, units = "cm")

  plot_model(nuc.model.L_t,
             title = paste("L-target time (ms),", curIcpt, "as intercept"),
             show.intercept = FALSE,
             show.values = TRUE,
             order.terms = c(2,3,4),
             vline.color = "red",
             colors = "Black")
  ggsave(file = paste("tables/5NUC_L_t_regression_plot_", curIcpt, ".png", sep = ""),
         width = 15, height = 8, units = "cm")

  plot_model(nuc.model.H_t,
             title = paste("H-target time (ms),", curIcpt, "as intercept"),
             show.intercept = FALSE,
             show.values = TRUE,
             order.terms = c(2,3,4),
             vline.color = "red",
             colors = "Black")
  ggsave(file = paste("tables/6NUC_H_t_regression_plot_", curIcpt, ".png", sep = ""),
         width = 15, height = 8, units = "cm")

  plot_model(nuc.model.S,
             title = paste("Slope (semitones/sec),", curIcpt, "as intercept"),
             show.intercept = FALSE,
             show.values = TRUE,
             order.terms = c(2,3,4),
             vline.color = "red",
             colors = "Black")
  ggsave(file = paste("tables/8NUC_S_regression_plot_", curIcpt, ".png", sep = ""),
         width = 15, height = 8, units = "cm")

  plot_model(nuc.model.D,
             title = paste("Distance from L to H target (ms),", curIcpt, "as intercept"),
             show.intercept = FALSE,
             show.values = TRUE,
             order.terms = c(2,3,4),
             vline.color = "red",
             colors = "Black")
  ggsave(file = paste("tables/7NUC_D_regression_plot_", curIcpt, ".png", sep = ""),
         width = 15, height = 8, units = "cm")

  plot_model(nuc.model.K,
             title = paste("Mean L and H F0 (semitones),", curIcpt, "as intercept"),
             show.intercept = FALSE,
             show.values = TRUE,
             order.terms = c(2,3,4),
             vline.color = "red",
             colors = "Black")
  ggsave(file = paste("tables/4NUC_K_regression_plot_", curIcpt, ".png", sep = ""),
         width = 15, height = 8, units = "cm")
  }
