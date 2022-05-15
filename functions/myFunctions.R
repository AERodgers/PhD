# author: "Antoin Eoin Rodgers"
# date: '2022-05-06'
# Bank of personal functions

installMissingPackages <- function (package_list)
  # installs packages from  package_list which are not already installed in.
{
  installed_packages <-
    package_list %in% rownames(installed.packages())
  
  if (any(installed_packages == FALSE))
  {
    install.packages(package_list[!installed_packages])
  }
  
  # Packages loading
  invisible(lapply(package_list, library, character.only = TRUE))
  
}


balancedData <- function(data_set,
                         treatment_col,
                         response_col,
                         gender_filter,
                         num_speakers,
                         num_reps,
                         use_pa_hierarchy = TRUE)
  # Returns an data matrix of phonological data of a projected balanced dataset.
  #    This function takes a subset of the data, calculates each token count
  #     per speaker per condition as a ratio of the total tokens for that
  #     condition and multiplies it by a constant reflecting the target token
  #     for that utterance. It then calculates the total number of tokens per
  #     condition as a ratio of the total number of speakers represented for
  #     that condition. Again, this is multiplied by a constant to reflect the
  #     intended number of speakers. The values are then rounded to an integer
  #     to approximate the ideal total tokens for the dataset.
  #
  #     data_set ....... target corpus data set
  #     treatment_col .. treatment column / independent variable)
  #     response_col ... response column / dependent variable (phonology)
  #     gender_filter .. "F" for female only, "M" for male only, "" for all
  #     num_speakers ... number of speakers in ideal corpus
  #     num_reps ....... number of repetitions per speaker in ideal corpus.
{
  if (gender_filter == "M") {
    data_set <- data_set %>% filter(gender == "M")
  }
  else {
    if (gender_filter == "F") {
      data_set <- data_set %>% filter(gender == "F")
    }
  }
  
  treatment_col = enquo(arg = treatment_col)
  response_col = enquo(arg = response_col)
  
  # Get number of speakers per target.
  speakers_per_target = data_set %>%
    select(speaker,!!treatment_col) %>%
    group_by(!!treatment_col) %>%
    summarise(speakers = n_distinct(speaker))
  
  # Get number of reps per speaker per target.
  pn_foot_reps <- data_set %>%
    group_by(speaker,!!treatment_col) %>%
    summarise(acc_count = n())
  kable(pn_foot_reps)
  
  # Get number of PA tokens per speaker per target
  pn_foot_summary <- data_set %>%
    group_by(speaker,!!treatment_col,!!response_col) %>%
    summarise(acc_count = n()) %>%
    spread(!!response_col, acc_count, is.na <- 0)
  
  balanced <- left_join(pn_foot_summary, pn_foot_reps)
  
  pa_columns <-
    colnames(balanced)[3:(length(colnames(balanced)) - 1)]
  
  # Convert tokens to ratios of tokens per speaker per target
  balanced <- balanced %>%
    
    mutate(across(pa_columns,  ~  (.x / acc_count * num_reps))) %>%
    group_by(!!treatment_col) %>%
    select(-speaker,-acc_count)
  
  num_cols <- length(colnames(balanced))
  
  balanced <- balanced %>%
    gather(!!response_col, mod_count, 2:num_cols)
  
  # Arrange PA levels according to hypothesized hierarchy.
  if (use_pa_hierarchy) {
    balanced <- balanced %>%
      mutate(acc_phon = factor(
        !!response_col, levels = c("(*)", "L*", "H*", ">H*", "L*H")
        ))
  }
  
  balanced <- balanced %>%
    group_by(!!treatment_col,!!response_col) %>%
    summarise(mod_sum = sum(mod_count)) %>%
    spread(!!response_col, mod_sum) %>%
    # Adjust token count re number of speakers per target condition.
    left_join(speakers_per_target) %>%
    mutate(across(pa_columns,  ~  round(.x / speakers * num_speakers))) %>%
    select(-speakers)
  
  return(balanced)
}

drawResiduals <- function(myModel) {
  myResiduals <- residuals(myModel)
  par(mfrow = c(1, 3))
  hist(myResiduals,
       xlab = "Residuals",
       main = "(a) Histogram of residuals")
  qqnorm(myResiduals,
         main = "(b) Q-Q Plot of residuals")
  qqline(myResiduals,
         xlab = "Fitted values",
         ylab = "Residuals")
  plot(fitted(myModel),
       myResiduals,
       main = "(c) Residual plot")
}

AdjustPToBonferroni <- function(myTibble,
                      excludeTerms,
                      bonferroniMultiplier) {
  myTibble <- mutate(
    myTibble,
    p.adjusted = if_else(
      term %in% excludeTerms,
      p.value,
      if_else(
        p.value * bonferroniMultiplier >= 1,
        0.9999999999,
        p.value * bonferroniMultiplier
      )
      )
    )

  return(myTibble)
}


sigCodesTidy <- function(my_tibble, incl_marginal_sig = FALSE) {
  my_tibble <- mutate(my_tibble,
                      "signif. (adj.)" =
                        if_else(
                          p.adjusted < 0.001,
                          'p<0.001',
                          if_else(
                            p.adjusted < 0.01,
                            'p<0.01',
                            if_else(
                              p.adjusted < 0.05,
                              'p<0.05',
                              if_else(p.adjusted < 0.1 &
                                        incl_marginal_sig,
                                      '(p<0.1)',
                                      '')
                            )
                          )
                        ))
  return(my_tibble)
}

