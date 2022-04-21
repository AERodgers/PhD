# Get all packages, functions, and default gg settings
if (!"datawizard" %in% rownames(installed.packages())){
  remotes::install_github("easystats/datawizard")
  }

my_packages <- c("datawizard",
                 "tidyverse",
                 "broom",
                 "styler",
                 "lmerTest",
                 "optimx",
                 "sjPlot",
                 "sjmisc",
                 "ggplot2",
                 "hablar",
                 "ggpubr",
                 "MuMIn",
                 "mefa4",
                 "formattable",
                 "codingMatrices",
                 "hablar",
                 "lme4",
                 "sjPlot",
                 "speakr")

already_installed <- my_packages %in% rownames(installed.packages())

if (any(already_installed == FALSE)) {
    install.packages(my_packages[!already_installed])
}

invisible(lapply(my_packages, library, character.only = TRUE))

options(ggplot2.palette="Set2")

theme_set(theme_minimal(base_size = 18))
