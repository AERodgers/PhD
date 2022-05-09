p <- c(
  "tidyverse",
  "knitr",
  "speakr",
  "codingMatrices",
  "broomExtra",
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
  "speakr"
)
for (i in 1:length(p))
     {print(paste(p[i],packageVersion(p[i])))}