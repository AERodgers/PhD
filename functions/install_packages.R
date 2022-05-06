# Antoin Eoin Rodgers
# Phonetics and Speech Laboratory, TCD
# April 2022

# Load packages needed for analysis

packages <-
  c(
    "ggplot2",
    "readxl",
    "dplyr",
    "tidyr",
    "ggfortify",
    "DT",
    "reshape2",
    "knitr",
    "lubridate",
    "pwr",
    "psy",
    "car",
    "doBy",
    "imputeMissings",
    "RcmdrMisc",
    "questionr",
    "vcd",
    "multcomp",
    "KappaGUI",
    "rcompanion",
    "FactoMineR",
    "factoextra",
    "corrplot",
    "ltm",
    "goeveg",
    "corrplot",
    "FSA",
    "MASS",
    "scales",
    "nlme",
    "psych",
    "ordinal",
    "lmtest",
    "ggpubr",
    "dslabs",
    "stringr",
    "assist",
    "ggstatsplot",
    "forcats",
    "styler",
    "remedy",
    "snakecaser",
    "addinslist",
    "esquisse",
    "here",
    "summarytools",
    "magrittr",
    "tidyverse",
    "funModeling",
    "pander",
    "cluster",
    "abind"
  )

# Install packages not yet installed
installed_packages <- packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(packages[!installed_packages])
}

# Packages loading
invisible(lapply(packages, library, character.only = TRUE))
