library(sjPlot)
library(ggplot2)
dir.create("modelOutput", showWarnings = FALSE)
#i =1
nucs$mode <- factor(nucs$mode, levels = order[,i])
curIcpt = order[1,i]
#curSlps = cat("\"", order[2,i],"\",\"",  order[3,i],"\",\"",  order[4,i],"\"", sep = "")

#source('runNucModelTests.R')

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
          file = paste("tables/4NUC_L_t_regression_table_", curIcpt, ".html", sep = ""))
tab_model(nuc.model.H_t,
          title = paste("H-target time (ms),", curIcpt, "as intercept"),
          show.icc = FALSE,
          file = paste("tables/5NUC_H_t_regression_table_", curIcpt, ".html", sep = ""))
tab_model(nuc.model.S,
          title = paste("Slope (semitones/sec),", curIcpt, "as intercept"),
          show.icc = FALSE,
          file = paste("tables/6NUC_S_regression_table_", curIcpt, ".html", sep = ""))
tab_model(nuc.model.D,
          title = paste("Distance from L to H target (secs),", curIcpt, "as intercept"),
          show.icc = FALSE,
          file = paste("tables/7NUC_D_t_regression_table_", curIcpt, ".html", sep = ""))
tab_model(nuc.model.K,
          title = paste("Mean L and H F0 (semitones),", curIcpt, "as intercept"),
          show.icc = FALSE,
          file = paste("tables/8NUC_K_regression_table_", curIcpt, ".html", sep = ""))

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
ggsave(file = paste("tables/4NUC_L_t_regression_plot_", curIcpt, ".png", sep = ""),
     width = 15, height = 8, units = "cm")

plot_model(nuc.model.H_t,
           title = paste("H-target time (ms),", curIcpt, "as intercept"),
           show.intercept = FALSE,
           show.values = TRUE,
           order.terms = c(2,3,4),
           vline.color = "red",
           colors = "Black")
ggsave(file = paste("tables/5NUC_H_t_regression_plot_", curIcpt, ".png", sep = ""),
     width = 15, height = 8, units = "cm")

plot_model(nuc.model.S,
           title = paste("Slope (semitones/sec),", curIcpt, "as intercept"),
           show.intercept = FALSE,
           show.values = TRUE,
           order.terms = c(2,3,4),
           vline.color = "red",
           colors = "Black")
ggsave(file = paste("tables/6NUC_S_regression_plot_", curIcpt, ".png", sep = ""),
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
ggsave(file = paste("tables/8NUC_K_regression_plot_", curIcpt, ".png", sep = ""),
     width = 15, height = 8, units = "cm")
