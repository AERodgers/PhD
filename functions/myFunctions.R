# author: "Antoin Eoin Rodgers"
# date: '2022-05-06'
# Bank of personal functions


# formatter
p_color <- . ~ style(color =
                       if_else(. < 0.05,
                               "green",
                               if_else(. < 0.01,
                                       "orange",
                                       "red")))

###  Install Missing Packages ##################################################
installMissingPackages <- function (package_list)
{
  # installs packages from  package_list which are not already installed in.
  installed_packages <-
    package_list %in% rownames(installed.packages())

  if (any(installed_packages == FALSE))
  {
    install.packages(package_list[!installed_packages])
  }

  # Packages loading
  invisible(lapply(
    package_list,
    suppressPackageStartupMessages(library),
    character.only = TRUE
  ))

}


###  Balance data  #############################################################
balancedData <- function(data_set,
                         treatment_col,
                         response_col,
                         gender_filter,
                         num_speakers,
                         num_reps,
                         use_pa_hierarchy = TRUE)
{
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
    select(speaker, !!treatment_col) %>%
    group_by(!!treatment_col) %>%
    summarise(speakers = n_distinct(speaker), .groups = "keep")

  # Get number of reps per speaker per target.
  pn_foot_reps <- data_set %>%
    group_by(speaker, !!treatment_col) %>%
    summarise(acc_count = n(), .groups = "keep")
  kable(pn_foot_reps)

  # Get number of PA tokens per speaker per target
  pn_foot_summary <- data_set %>%
    group_by(speaker, !!treatment_col, !!response_col) %>%
    summarise(acc_count = n(), .groups = "keep") %>%
    spread(!!response_col, acc_count, is.na <- 0)

  balanced <- left_join(pn_foot_summary, pn_foot_reps)

  pa_columns <-
    colnames(balanced)[3:(length(colnames(balanced)) - 1)]

  # Convert tokens to ratios of tokens per speaker per target
  balanced <- balanced %>%

    mutate(across(pa_columns,  ~  (.x / acc_count * num_reps))) %>%
    group_by(!!treatment_col) %>%
    select(-speaker, -acc_count)

  num_cols <- length(colnames(balanced))

  balanced <- balanced %>%
    gather(!!response_col, mod_count, 2:num_cols)

  # Arrange PA levels according to hypothesized hierarchy.
  if (use_pa_hierarchy) {
    balanced <- balanced %>%
      mutate(acc_phon = factor(!!response_col,
                               levels = c("(*)", "L*", "H*", ">H*", "L*H")))
  }

  balanced <- balanced %>%
    group_by(!!treatment_col, !!response_col) %>%
    summarise(mod_sum = sum(mod_count), .groups = "keep") %>%
    spread(!!response_col, mod_sum) %>%
    # Adjust token count re number of speakers per target condition.
    left_join(speakers_per_target) %>%
    mutate(across(pa_columns,  ~  round(.x / speakers * num_speakers))) %>%
    select(-speakers)

  return(balanced)
}


###  Draw Residuals  #########################################################
drawResiduals <- function(myModel)
{
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


###  Bonferonni Adjustment  ####################################################
bonferroniAdjust <- function(myTibble,
                             bonferroniMultiplier=0,
                             exclude_terms = NULL,
                             p.adj="p.adj")
  {
  if (bonferroniMultiplier)
    {
    p.adj = enquo(p.adj)

    myTibble <- mutate(myTibble,
                       !!p.adj := if_else(
                         !is_null(exclude_terms) & term %in% exclude_terms,
                         NA_real_,
                         if_else(
                           p.value * bonferroniMultiplier >= 1,
                           0.9999,
                           p.value * bonferroniMultiplier
                           )
                         )
                       )
    }
    return(myTibble)
  }


###  Significance Code Tidy  ###################################################
sigCodesTidy <-
  function(my_tibble,
           p_value = "p.adj",
           incl_marginal_sig = TRUE)
    # Create significance column in tibble using p_value
  {
    p_value <- enquo(p_value)
    my_tibble <- mutate(my_tibble,
                        `signif.` =
                          if_else(
                            !!p_value < 0.001,
                            'p<0.001',
                            if_else(
                              !!p_value < 0.01,
                              'p<0.01',
                              if_else(
                                !!p_value < 0.05,
                                'p<0.05',
                                if_else(!!p_value < 0.1 &
                                          incl_marginal_sig,
                                        '(p<0.1)',
                                        '')
                              )
                            )
                          )) %>%
      mutate(`signif.` =
               if_else(
                 !is.na(!!p_value),
                 `signif.`,
                 if_else(
                   p.value < 0.001,
                   "p<0.001",
                   if_else(
                     p.value < 0.01,
                     "p<0.01",
                     if_else(
                       p.value < 0.05,
                       "p<0.05",
                       if_else(p.value < 0.1 &
                                 incl_marginal_sig,
                               "(p<0.1)",
                               "")
                     )
                   )
                 )
               ))



    return(my_tibble)
  }


###  Parameter Summary ##########################################################
param_summary <-
  function(df, treatment, phonology, is_nucleus = FALSE)
  {
    # Summarise dataset by speaker, treatment variable, and phonology
    treatment <- enquo(treatment)
    phonology  <- enquo(phonology)

    if (is_nucleus == TRUE)
    {
      ans <- df %>%
        group_by(speaker, !!treatment, !!phonology) %>%
        summarise(
          l_t = round(mean(l_t), 2),
          h_t = round(mean(h_t), 2),
          lh_dur = round(mean(lh_dur), 2),
          l_f0 = round(mean(l_f0), 2),
          l_f0_z = round(mean(l_f0_z), 3),
          h_f0 = round(mean(h_f0), 2),
          h_f0_z = round(mean(h_f0_z), 3),
          e_f0 = round(mean(e_f0, 2)),
          e_f0_z = round(mean(e_f0_z, 3)),
          e_f0_exc_z = round(mean(e_f0_exc_z, 3)),
          e_t = round(mean(e_t, 2)),
          he_dur = round(mean(he_dur, 2)),
          f0_exc = round(mean(f0_exc), 2),
          f0_exc_z = round(mean(f0_exc_z), 3),
          lh_slope = round(mean(lh_slope), 2),
          .groups = "keep"
        )
    } else {
      ans <- df %>%
        group_by(speaker, !!treatment, !!phonology) %>%
        summarise(
          s_t = round(mean(s_t, 2)),
          s_f0_z = round(mean(s_f0_z, 3)),
          l_t = round(mean(l_t), 2),
          h_t = round(mean(h_t), 2),
          lh_dur = round(mean(lh_dur), 2),
          l_f0 = round(mean(l_f0), 2),
          l_f0_z = round(mean(l_f0_z), 3),
          h_f0 = round(mean(h_f0), 2),
          h_f0_z = round(mean(h_f0_z), 3),
          f0_exc = round(mean(f0_exc), 2),
          f0_exc_z = round(mean(f0_exc_z), 3),
          lh_slope = round(mean(lh_slope), 2),
          .groups = "keep"
        )
    }

    return(ans)

  }


###  Get Parameter Means  ######################################################
param_means <-
  function(df,
           phonet_param,
           hor_param,
           vert_param,
           rounding = 2)
  {
    # Return a horizontal x vertical tibble summarising mean parameter phonetic.

    phonet_param <- enquo(phonet_param)
    hor_param  <- enquo(hor_param)
    vert_param  <- enquo(vert_param)

    return(
      df %>%
        group_by(!!hor_param, !!vert_param) %>%
        summarise(new_means = round(mean(!!phonet_param), rounding),
                  .groups = "keep") %>%
        pivot_wider(names_from = !!hor_param, values_from = new_means)
    )
  }


###  Get M_Corpus #############################################################
get_m_corpus <- function(file_address)

  # Include package for %in% / %notin% syntactic notation
  {
  require("mefa4")
  return(
    as_tibble(read.csv(file_address)) %>%
      # Only keep pertinent columns!
      select(
        speaker,
        gender,
        stim,
        cur_foot,
        tot_feet,
        init_phon,
        phr_phon,
        fin_phon,
        foot_syls,
        tot_syls,
        acc_phon,
        foot_start_t,
        foot_end_t,
        v_onset_t,
        l_t,
        h_t,
        s_t,
        e_t,
        v_grand_mean_t,
        l_grand_mean_t,
        h_grand_mean_t,
        s_grand_mean_t,
        e_grand_mean_t,
        l_f0,
        h_f0,
        s_f0,
        e_f0,
        phr_end_t,
        lh_mean_f0,
        lh_slope,
        lh_slope_z,
        utt_slope,
        utt_mean_f0,
        utt_slope_z,
        spkr_f0_mean,
        spkr_f0_SD
      ) %>%
      mutate(
        # create composite parameters for continuous data.
        foot_dur = foot_end_t - foot_start_t,
        speech_rate = round(tot_syls / phr_end_t * 1000, 3),
        f0_exc = h_f0 - l_f0,
        e_f0_exc = e_f0 - h_f0,
        lh_dur = h_t - l_t,
        he_dur = e_t - h_t,
        l_f0_z = (l_f0 - spkr_f0_mean) / spkr_f0_SD,
        h_f0_z = (h_f0 - spkr_f0_mean) / spkr_f0_SD,
        s_f0_z = (s_f0 - spkr_f0_mean) / spkr_f0_SD,
        e_f0_z = (e_f0 - spkr_f0_mean) / spkr_f0_SD,
        utt_mean_f0_z = (utt_mean_f0 - spkr_f0_mean) / spkr_f0_SD,
        lh_mean_f0_z = ( lh_mean_f0 - spkr_f0_mean) / spkr_f0_SD,
        # redo excursion based on z-scores
        f0_exc_z = h_f0_z - l_f0_z,
        e_f0_exc_z = e_f0_z - h_f0_z,
        # Make L and H times relative to vowel onset (TBU).
        s_t = s_t - v_onset_t,
        l_t = l_t - v_onset_t,
        h_t = h_t - v_onset_t,
        e_t = e_t - v_onset_t,
        s_grand_mean_t = s_grand_mean_t - v_grand_mean_t,
        l_grand_mean_t = l_grand_mean_t - v_grand_mean_t,
        h_grand_mean_t = h_grand_mean_t - v_grand_mean_t,
        e_grand_mean_t = e_grand_mean_t - v_grand_mean_t,

        # Ensure factor variables are interpreted as factors
        foot_syls = factor(foot_syls, levels = unique(foot_syls)),
        gender = factor(gender, levels = unique(gender)),

        # create mode and prompt columns
        mode = factor(str_sub(stim, 1, 3), levels = c("MDC", "MWH", "MYN", "MDQ")),
        prompt = factor(str_sub(stim, 4, 4), levels = c(1, 2, 3)),
        # Ignore downstep.
        #acc_phon = str_replace(acc_phon, "!", ""),

        # Arrange speaker factors in more intuitive order.
        speaker = factor(
          speaker,
          levels = c(
            "F5",
            "F6",
            "F12",
            "F15",
            "F16",
            "F17",
            "M4",
            "M5",
            "M8",
            "M9",
            "M10"
          )
        )
      ) %>%
      # Remove columns which have outlived their use!
      select(
        -c(
          v_onset_t,
          foot_end_t,
          foot_start_t,
          tot_syls,
          phr_end_t,
          spkr_f0_mean,
          spkr_f0_SD
        )
      ) %>%
      filter(tot_feet == cur_foot) %>%
      unite(
        nuc_contour,
        acc_phon,
        fin_phon,
        sep = " ",
        remove = FALSE
      ) %>%
      mutate(nuc_contour = if_else(
        unlist(gregexpr('[[]', nuc_contour)) == -1
        & unlist(gregexpr('[]]', nuc_contour)) > -1,
        paste("^[", nuc_contour, sep = ""),
        nuc_contour)
        )
  )

}


###  Get Formula as String from LME/GLM model ##################################
getModelFormula <-function(my_model) {
  require("stringr")
  my_formula <- str_c(formula(my_model))
  my_formula <- paste(my_formula[2], my_formula[1], my_formula[3])
  return(my_formula)
}

###  Get Data as String from LME/GLM model ##################################
getModelDataName <-function(my_model) {
  return(my_model@call[["data"]])
}

###  Summarise LME  ############################################################
summariseLME <-
  function(my_model,
           run_step = FALSE,
           my_tolerance = 1e-05,
           write=NULL,
           extra_text="")
    # short function to remove need for repetition of optimized used throughout.
  {
    require("lme4")
    require("lmerTest")
    require("optimx")
    require("performance")
    require("stringr")

    my_formula <- getModelFormula(my_model)
    my_data_name <- getModelDataName(my_model)

    # output results
    drawResiduals(my_model)
    print(summary(my_model))

    anova <- anova(my_model) %>%
      tidy() %>%
      mutate(across(`sumsq`:`statistic`, ~ round(., 3))) %>%
      formattable(
        caption=paste(
          "Anova of model", my_formula, sep=": ")
        ) %>%
      sigCodesTidy(p.value, FALSE) %>%
      rename(`F value` = statistic)
    if(!is_null(write)){
      anova %>%
      write_csv(write)
      formula_save <- str_replace(write, "_anova.csv", "_formula.txt") %>%
        str_replace(".csv", ".txt")
      write(paste(my_formula, extra_text, sep=""), formula_save)
    }

    cat("\nCheck_singularity(my_model, tolerance =",
        my_tolerance,
        "-->",
        check_singularity(my_model, tolerance=my_tolerance),
        "\n"
    )

    if (run_step)
    {
      cat("\n")
      cat("\nResults of step().\n")
      step(my_model) %>% print()
    }

  return(anova)
  }


###  Print Tidy Model  #########################################################
printTidyModel <-
  function(my_model,
           write_r2 = NULL,
           is_GLM = FALSE)
  {
    require("formattable")
    require("tidyverse")
    require("mefa4")
    require("performance")
    require("kableExtra")

  my_stat <- ifelse(is_GLM, "z.value", "z.value")

  my_headers <- c(
    "term",
    "estimate",
    "std.error",
    "2.5% CI",
    "97.5% CI",
    eval(my_stat),
    "df",
    "p.value"
  )

  my_stat = enquo(my_stat)

  if (is_GLM){my_headers <- my_headers[my_headers != "df"]}

  my_headers = enquos(my_headers)
  my_formula <- getModelFormula(my_model)

  tidy_model <- tidy(my_model) %>%
    filter(effect %notin% "ran_pars") %>%
    select(-c(group, effect)) %>%
    mutate(
      estimate = round(estimate, 3),
      std.error = round(std.error, 3),
      statistic = round(statistic, 3)
    ) %>%
    rename(!!my_stat := statistic)

  if (!is_GLM){
    tidy_model <- tidy_model %>%
    mutate(df = round(df, 2))
    }

  # Get CIs of model using Wald method (fast, fixed effects only)
  ci_Wald <- confint(my_model, method = "Wald") %>%
    as_tibble() %>%
    filter(`2.5 %` != "NA") %>%
    mutate(`2.5 %` = round(`2.5 %`, 3),
           `97.5 %` = round(`97.5 %`, 3)) %>%
    rename("2.5% CI" = "2.5 %",
           "97.5% CI" = "97.5 %")

  # Bind tidy model to CIs
  tidy_model <-
  cbind(tidy_model, ci_Wald) %>%
    # re-order columns
    select(!!!my_headers) %>%
    formattable(
      caption = paste(my_formula)
    ) %>%
    mutate(term = str_replace_all(term, "([\\*\\[\\^\\>])", "\\\\\\1"))

  r2_nakagawa <-  knitr::kable(
    r2_nakagawa(my_model),
    caption = "Conditional and marginal R^2^ of model",
    digits = 2,
    align = "l"
  )  %>% kable_styling(full_width = FALSE, position="left")

  if (!is.null(write_r2)) {
    do.call(rbind, r2_nakagawa(my_model))[, 1] %>%
      write.csv(paste(write_r2, "_r2.csv", sep = ""))
  }

  my_plot <- print(
    plot_model(
      my_model,
      title = my_formula,
      show.intercept = FALSE,
      show.values = TRUE,
      vline.color = "red",
      colors = "Black"
    )
  )
return(list("r2" = r2_nakagawa, "table" = tidy_model, "plot" = my_plot))
}



###  Get Fixed Effects of LME/GLMM Model #######################################
getModelFixedFX <- function(my_equation,
                       my_data,
                       write="",
                       is_GLM=FALSE,
                       optimizer = "optimx",
                       extra_text="")
{
  require("formattable", "performance", "tidyverse", "mefa4")

  my_stat <- ifelse(is_GLM, "z.value", "z.value")

  my_headers <- c(
    "term",
    "estimate",
    "std.error",
    "2.5% CI",
    "97.5% CI",
    eval(my_stat),
    "df",
    "p.value"
  )

  my_stat = enquo(my_stat)

  if (is_GLM){my_headers <- my_headers[my_headers != "df"]}
  my_headers = enquos(my_headers)

  # run base model.
  if (is_GLM) {
    base_model <- glmer(
      my_equation,
      data = my_data,
      family = binomial(link = "logit"),
      # Change optimizer to avoid convergence errors/
      control = glmerControl(
        optimizer = optimizer,
        calc.derivs = FALSE,
        optCtrl = list(
          method = "nlminb",
          starttests = FALSE,
          kkt = FALSE
        )
      )
    )
  }
  else {
    base_model <- lmer(
      my_equation,
      data = my_data,
      control = lmerControl(
        optimizer = optimizer,
        calc.derivs = FALSE,
        optCtrl = list(
          method = "nlminb",
          starttests = FALSE,
          kkt = FALSE
        )
      )
    )
  }

  # write text file for model info
  my_formula <- getModelFormula(base_model)

  write(paste(my_formula, extra_text, sep=""),
        paste(write, "_formula.txt", sep = ""))

  # create list of fixed factors
  fixed_factors <-
    (str_replace_all(deparse(formula(base_model,fixed.only = TRUE)[3]),
                     "[\\(|\\)|+ ]",
                     " ") %>%
       str_squish() %>%
       str_split(" "))[[1]]

  # Get list of non-interaction factors
  multilevel_factors <- fixed_factors[fixed_factors %in% colnames(my_data)]

  # Get list of 2-level fixed factors.
  two_level_factors <- character()
  two_level_terms <- character()
  for (cur_factor in multilevel_factors){
    if(levels(m_corpus[[cur_factor]]) %>% length() == 2){
      two_level_factors <-c(two_level_factors, cur_factor)
      two_level_terms <- c(two_level_terms,
                           paste(cur_factor,
                                 levels(m_corpus[[cur_factor]])[2],
                                 sep=""))
      }
    }

  # remove 2-level factors from list of multilevel_factors
  multilevel_factors <-
    multilevel_factors[multilevel_factors %notin% two_level_factors]

  # create empty tibble for fixed factor output
  all_models_tidy = tibble()

  # loop through each multilevel fixed factor of interest (multilevel_factors)
  for (cur_factor in multilevel_factors)
    {
    # Get levels for current factor
    cur_levels = levels(my_data[[cur_factor]])
    num_levels = length(cur_levels)
    # Make list of terms to keep
    keep_terms = character()
    for (level_name in cur_levels) {
      keep_terms = c(keep_terms, paste(cur_factor, level_name, sep = ""))
    }
    # loop through dataframe, reordering levels of current factor each time.
    for (cur_level in 1:(num_levels))
    {
      # Run current model.
      if (is_GLM) {
        cur_model <- glmer(
          my_equation,
          data = my_data,
          family = binomial(link = "logit"),
          # Change optimizer to avoid convergence errors/
          control = glmerControl(
            optimizer = optimizer,
            calc.derivs = FALSE,
            optCtrl = list(
              method = "nlminb",
              starttests = FALSE,
              kkt = FALSE
            )
          )
        )
      }
      else {
        cur_model <- lmer(
          my_equation,
          data = my_data,
          control = lmerControl(
            optimizer = optimizer,
            calc.derivs = FALSE,
            optCtrl = list(
              method = "nlminb",
              starttests = FALSE,
              kkt = FALSE
            )
          )
        )
      }

      #check_singularity(cur_model) %>% print()

      # Tidy the model.
      cur_model_tidy <- tidy(cur_model) %>%
        # Retain fixed factors only
        filter(effect == "fixed") %>%
        # remove unnecessary columns
        select(-c(effect, group)) %>%
        # Tidy up numbers.
        mutate(
          estimate = round(estimate, 3),
          std.error = round(std.error, 3),
          statistic = round(statistic, 3)
        ) %>%
        rename(!!my_stat := statistic)

      if (!is_GLM){
        cur_model_tidy <- cur_model_tidy %>%
          mutate(df = round(df, 2))
      }

      # Get CIs of model using Wald method (fast, fixed effects only)
      ci_Wald <- confint(cur_model, method = "Wald") %>%
        as_tibble() %>%
        filter(`2.5 %` != "NA") %>%
        mutate(`2.5 %` = round(`2.5 %`, 3),
               `97.5 %` = round(`97.5 %`, 3)) %>%
        rename("2.5% CI" = "2.5 %",
               "97.5% CI" = "97.5 %")

      # Bind tidy model to CI intervals
      cur_model_tidy <- cbind(cur_model_tidy, ci_Wald) %>%
        # re-order columns
        select(!!!my_headers)

      cur_model_tidy <- cur_model_tidy %>%
        filter((term %in% c(keep_terms, "(Intercept)"))) %>%
        # Prepare current model for pasting to all models output.
        # Make 'pairwise' column = intercept.
        mutate(
          pairwise =
            if_else(
              term == "(Intercept)",
              "intercept",
              if_else(term %notin% keep_terms,
                      "N/A",
                      keep_terms[cur_level])
            ),
          # change 'term' so "intercept" states the target condition name.
          term =
            if_else(term == "(Intercept)",
                    keep_terms[cur_level],
                    term)

        )

      keep_comparisons = NULL
      # make list of pairwise comparisons to keeps
      for (j in cur_level:(num_levels))
      {
        keep_comparisons <- c(keep_comparisons, keep_terms[j])
      }

      # remove pairwise comparisons which have already been done
      cur_model_tidy <-
        filter(cur_model_tidy, term %in% keep_comparisons)

      # add remaining pairwise comparisons to main tibble.
      all_models_tidy <- bind_rows(all_models_tidy, cur_model_tidy)

      # restructure the order of levels for next LME cur_model.
      cur_levels <- c(cur_levels[2:num_levels], cur_levels[1])

      factor_var <- sym(cur_factor)
      factor_var_name <- quo_name(cur_factor)

      my_data <- my_data %>%
        mutate(!!factor_var := factor(!!factor_var,
                                      levels = cur_levels))
          }
  }

  # Get intercepts and pairwise comparisons tables
  all_models_tidy <- all_models_tidy %>%
    relocate(pairwise)

  my_intercepts <- tidyIntercepts(all_models_tidy)
  my_pairwise <- tidyPairwise(all_models_tidy, is_GLM=is_GLM)

  # Get tibble of 2-level factor slopes from base model
  ci_Wald <- confint(base_model, method = "Wald") %>%
    as_tibble() %>%
    filter(`2.5 %` != "NA") %>%
    mutate(`2.5 %` = round(`2.5 %`, 3),
           `97.5 %` = round(`97.5 %`, 3)) %>%
    rename("2.5% CI" = "2.5 %",
           "97.5% CI" = "97.5 %")

  two_level_factor_slopes <- tidy(base_model) %>%
    filter(effect %notin% "ran_pars") %>%
    mutate(
      estimate = round(estimate, 3),
      std.error = round(std.error, 3),
      statistic = round(statistic, 3)
    )

  if (!is_GLM){
    two_level_factor_slopes <- two_level_factor_slopes %>%
      mutate(df = round(df, 2))
    }


    two_level_factor_slopes <- two_level_factor_slopes %>%
      cbind(ci_Wald) %>%
      filter(term %in% two_level_terms) %>%
      select(-c(group, effect)) %>%
    mutate(estimate = round(estimate, 3),
           std.error = round(std.error, 3),
           statistic = round(statistic, 3),
    ) %>%
    mutate(intercept = "intercept", .before=term
    )  %>%
    relocate(`97.5% CI`, .after="std.error") %>%
    relocate(`2.5% CI`, .after="std.error") %>%
    rename(!!my_stat := statistic,
           slope=term)
  # bind two-level factor stats to my_pairwise
  my_pairwise <- rbind(my_pairwise, two_level_factor_slopes)

  # Write tables to file
  if (write != "")
  {
    write_csv(my_intercepts,
              paste(write, "_b0.csv", sep = ""))
    write_csv(my_pairwise,
              paste(write, "_b1.csv", sep = ""))
  }

  # Output formatted tables
  my_intercepts <- my_intercepts %>%
    mutate(
      intercept = str_replace_all(intercept, "([\\*\\[\\^\\>])", "\\\\\\1")
      ) %>%
    formattable(caption = paste("b0 for", my_formula, sep = " "))

  my_pairwise <-  my_pairwise %>%
    mutate(
      intercept = str_replace_all(intercept, "([\\*\\[\\^\\>])", "\\\\\\1"),
      slope = str_replace_all(slope, "([\\*\\[\\^\\>])", "\\\\\\1")
    ) %>%
    formattable(caption = paste("b1 for", my_formula, sep = " "))

    return(list("intercepts" = my_intercepts, "pairwise" = my_pairwise,
              "model" = base_model))
}


###  Tidy Intercepts of multiple analyses  #####################################
tidyIntercepts <- function(all_models_tidy)
  {
  return(
      filter(all_models_tidy, pairwise == "intercept") %>%
      select(-pairwise) %>%
      rename(intercept = term)
      )
}


###  Tidy Pairwise Tables of multiple analyses  ################################
tidyPairwise <- function(all_models_tidy, is_GLM = FALSE)
{

  my_stat <- ifelse(is_GLM, "z.value", "z.value")
  my_headers <- c(
    "pairwise",
    "term",
    "estimate",
    "std.error",
    "2.5% CI",
    "97.5% CI",
    eval(my_stat),
    "df",
    "p.value"
  )

  if (is_GLM){my_headers <- my_headers [my_headers != "df"]}
  my_headers = enquos(my_headers)
    return(
    filter(all_models_tidy, pairwise %notin% c("intercept", "N/A")) %>%
      select(!!!my_headers) %>%
      rename(intercept = pairwise, slope = term)
    )
}


###  Kable Chi Squared  ########################################################
kable_chi_sq <- function(chi_sq_test)
  {
    # returns a kable() object of the chi_sq_test input.
    require("knitr", "janitor")

    x2d <- round(chi_sq_test$statistic[1],10)
    names(x2d) <- NULL

    df <- chi_sq_test$parameter[1]
    names(df) <- NULL

    p <-chi_sq_test$p.value[1]

    df <- data.frame(term=c("Chi-squared", "df", "p.value"),
                    value=c(x2d, df, p))

    names(df) <- NULL
    return(kable(df, caption="Pearson's Chi-squared test"))
}


###  Bulk Adjust p Value   ####################################################
adjustP_posthoc <-
  function(my_folder,            # source folder with .csv files to be updated.
           p_column,             # name of p.column
           method = "BH",        # p. adjustment method
           marginal = TRUE,      # include marginal significance flag.
           write = TRUE,         # write results to file flag.
           report = FALSE,       # flag to report total number of tests and
                                 # p.values < 0.05 before and after adjustment.
           print = FALSE,        # Print output or not
           suffix_id="b0b1"      # suffix ID for files for analysis
                                 # (b0b1 = all files ending in "_b0" and "_b1")
  )
  {
    # Load required packages
    require("dplyr")
    require("formattable")
    require("readr")
    require("mefa4")
    require("knitr")
    require("kableExtra")

    # Abbreviate method where necessary.
    if(method %in% c("hochberg", "hommel", "bonferroni")) {
      my_meth <- switch(method,
                        "hochberg" = "hoch",
                        "hommel" = "homm",
                        "bonferroni" = "bonf")
      }
    else{my_meth <- method}

    # Enquote variables which whose values will be evaluated as variables.
    p_column = enquo(p_column)
    new_adj_col = paste("p.adj (", method, ")", sep="")
    new_adj_col = enquo(new_adj_col)

    if(suffix_id=="b0b1"){
    # Get tibble of all b0 and b1 files to be adjusted.
    file_tibble <-
      list.files(my_folder, "*_b0.csv", full.names = TRUE) %>%
      read_csv(id = "file_name",
               col_names = TRUE,
               show_col_types = FALSE) %>%
      # Create dummy slope column for b0 files.
      mutate(slope = NA, .before = intercept) %>%
      rbind(
        list.files(my_folder, "*_b1.csv", full.names = TRUE) %>%
          read_csv(id = "file_name", col_names = TRUE, show_col_types = FALSE)
      ) %>%
      # avoid reduplication of current method column
      select(-any_of(!!new_adj_col)) %>%
      # Add p.adjusted column using method.
      mutate(p.adj = p.adjust(!!p_column,
                              method = method),
             .after = !!p_column) %>%
      relocate(intercept)}
    else {
      # Get tibble of files to be adjusted
      file_tibble <-
        list.files(my_folder,
                   paste("*", suffix_id, ".csv", sep=""),
                   full.names = TRUE) %>%
        read_csv(id = "file_name",
                 col_names = TRUE,
                 show_col_types = FALSE) %>%

        # avoid reduplication of current method column
        select(-any_of(c(!!new_adj_col, "signif."))) %>%
        # Add p.adjusted column using method.
        mutate(p.adj = p.adjust(!!p_column,
                                method = method),
               .after = !!p_column)

    }

    # Get summary info about p values.
    p_values <- file_tibble %>% nrow()
    sig_p_values <- file_tibble %>% filter(!!p_column < 0.05) %>% nrow()
    sig_p_values_adj  <- file_tibble %>% filter(p.adj < 0.05) %>% nrow()
    p_counts <- tibble(p_values, sig_p_values, sig_p_values_adj)

    file_tibble <- file_tibble %>%
      mutate(
        # Add significance column.
        signif. = if_else(
          p.adj < 0.0001, "p<0.0001", if_else(
            p.adj < 0.001, "p<0.001", if_else(
              p.adj < 0.01, "p<0.01", if_else(
                p.adj < 0.05, "p<0.05", if_else(
                  p.adj < 0.1 & marginal, "(p<0.1)",""))))),
        # Change p.adj and p_column to more readable format.
        p.adj = if_else(p.adj < 0.001,
          as.character(formatC(p.adj, format="e", digits = 2)),
          as.character(round(p.adj, 4), digits = 2)),
        !!p_column := if_else(
          !!p_column < 0.001,
          as.character(formatC(!!p_column, format="e", digits = 2)),
          as.character(round(!!p_column, 4), digits = 2))
        ) %>%
      # Change name of p.adj to indicate adjustment method.
      rename(!!new_adj_col := p.adj)


      for (cur_file in unique(file_tibble$file_name))
      {
        cur_set <- file_tibble %>%
          filter(file_name == cur_file) %>%
          select(-file_name)
        # Remove dummy slope column from b0 files.
        if (suffix_id == "b0b1") {
          if (is.na(cur_set$slope[1])) {
            cur_set <- cur_set %>% select(-slope)
          }
        }
        # Re-save updated tables as original file name.
        if (write) {
          write_csv(cur_set, cur_file)
        }


        # print table.
        if (print) {
          formula_file <- cur_file %>%
            str_replace("_b0.csv|_b1.csv|_anova.csv", "_formula.txt")
          if (file.exists(formula_file)) {
            my_caption <- read_lines(formula_file)
          }
          else{
            my_caption = str_replace(cur_file, ".csv", "")
          }

          cur_set %>%
            mutate(across(
              everything(),
              ~ str_replace_all(., "([\\*\\[\\^\\>])", "\\\\\\1")
              )) %>%
            knitr::kable(caption = my_caption, align = "l") %>%
            kable_styling(full_width = FALSE, position = "left") %>%
            print()
        }
      }


    if (report) {
      return(p_counts)
    }
  }

