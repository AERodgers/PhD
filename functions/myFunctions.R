# author: "Antoin Eoin Rodgers"
# date: '2022-05-06'
# Bank of personal functions

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

################################################################################
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

################################################################################
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

################################################################################
bonferroniAdjust <- function(myTibble,
                             bonferroniMultiplier=0,
                             exclude_terms = "",
                             p.adj="p.adj")
  {
  if (bonferroniMultiplier)
    {
    p.adj = enquo(p.adj)

    myTibble <- mutate(myTibble,
                       !!p.adj := if_else(
                         term %in% exclude_terms,
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

################################################################################
sigCodesTidy <-
  function(my_tibble,
           p.adjusted = "p adj",
           incl_marginal_sig = FALSE
  )
  # Create significance column in tibble using Bonferroni adjusted p.
{
  p.adjusted <- enquo(p.adjusted)
  my_tibble <- mutate(my_tibble,
                      `signif. ` =
                        if_else(
                          !!p.adjusted < 0.001,
                          'p<0.001',
                          if_else(
                            !!p.adjusted < 0.01,
                            'p<0.01',
                            if_else(
                              !!p.adjusted < 0.05,
                              'p<0.05',
                              if_else(!!p.adjusted < 0.1 &
                                        incl_marginal_sig,
                                      '(p<0.1)',
                                      '')
                            )
                          )
                        )
                      ) %>%
    mutate(`signif. ` =
             if_else(
               !is.na(!!p.adjusted),
               `signif. `,
               if_else(p.value < 0.001,
                       "p<0.001",
                       if_else(p.value < 0.01,
                               "p<0.01",
                               if_else(p.value < 0.05,
                                        "p<0.05",
                                       if_else(p.value < 0.1 &
                                                 incl_marginal_sig,
                                               "(p<0.1)",
                                               ""
                                               )
                                       )
                               )
                       )
               )
           )



  return(my_tibble)
}

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

get_m_corpus <- function(file_address) {
  # Include package for %in% / %notin% syntactic notation
  installMissingPackages(c("mefa4"))
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
        spkr_f0_SD,

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
        nuc_contour
      ))
  )

}


summarise_lme <- function(my_model, run_step=FALSE, my_tolerance=1e-05)
  # short function to remove need for repetition of optimized used throughout.
{
  require(lme4, lmerTest, optimx, performance)

  # output results
    drawResiduals(my_model)
    print(summary(my_model))
    cat("\nAnova of model\n")
    anova(my_model) %>% print()
    cat("\nCheck_singularity(my_model, tolerance =", my_tolerance, "-->",
        check_singularity(my_model, tolerance = my_tolerance, "\n"))
    cat("\n")
  if(run_step)
    {
    cat("\nResults of step().\n")
    print(step(my_model))
    }

}

###############################################################################
printTidyModel <-
  function(my_model, bf_adj = 1, exclude_terms = "", write_r2 = "")
{
  require(formattable, tidyverse, Mefa4)

  p_adj_name =paste("p.adj. (bf=", bf_adj, ")", sep="")
  p_adj_name = enquo(p_adj_name)
  my_formula <- str_c(formula(my_model))
  my_formula <- paste(my_formula[2], my_formula[1], my_formula[3])

  tidy_model <- tidy(my_model) %>%
    filter(effect %notin% "ran_pars") %>%
    select(-c(group, effect)) %>%
    bonferroniAdjust(7, exclude_terms) %>%
    mutate(
      estimate = round(estimate, 3),
      std.error = round(std.error, 3),
      statistic = round(statistic, 3),
      df = round(df, 2)
    ) %>%
    rename(t.value = statistic)

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
    select(
      "term",
      "estimate",
      "std.error",
      "2.5% CI",
      "97.5% CI",
      "t.value",
      "df",
      "p.value",
      "p.adj"
    ) %>%
    formattable(
      caption = paste("Intercept and slopes of fixed effects:", my_formula)
    ) %>%
    sigCodesTidy(p.adj) %>%
    mutate(
      p.value = if_else(
        p.value < 0.001,
        as.character(scientific(p.value), digits = 2),
        as.character(round(p.value, 4), digits = 2)
      ),
      p.adj = if_else(
        p.adj < 0.001,
        as.character(scientific(p.adj, digits = 2)),
        as.character(round(p.adj, 4))
      )
    ) %>%
    rename(!!p_adj_name := p.adj) %>%
    mutate(term = str_replace_all(term, "([\\*\\[\\^\\>])", "\\\\\\1"))

  r2_nakagawa <- kable(
    r2_nakagawa(my_model),
    caption = "Conditional and marginal R^2^ of model",
    digits = 2,
    align = "l"
  )

  if (write_r2 != "") {
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

###############################################################################
getLMEFixedFX <- function(my_equation,
                       my_data,
                       exclude_terms = "",
                       bf_adj = 0,
                       write="")
{
  require(formattable, performance, tidyverse, Mefa4)

  # run base model.
  base_model <- lmer(
    my_equation,
    data = my_data,
    control = lmerControl(
      optimizer = "optimx",
      calc.derivs = FALSE,
      optCtrl = list(
        method = "nlminb",
        starttests = FALSE,
        kkt = FALSE
      )
    )
  )

  my_formula <- str_c(formula(base_model))
  my_formula <- paste(my_formula[2], my_formula[1], my_formula[3])

  # create list of fixed factors
  fixed_factors <-
    (str_replace_all(deparse(formula(base_model,fixed.only = TRUE)[3]),
                     "[\\(|\\)|+ ]",
                     " ") %>%
       str_squish() %>%
       str_split(" "))[[1]]

  # make list of factors for intercepts and pairwise comparison tables.
  keep_factors <- fixed_factors[fixed_factors %notin% exclude_terms]
  # Ensure only factors representing my_data columns are included.
  keep_factors <- keep_factors[keep_factors %in% colnames(my_data)]

  # create empty tibble for fixed factor output
  all_models_tidy = tibble()

  # loop through each fixed factor of interest (keep_factors)
  for (cur_factor in keep_factors)
    {
    # Get levels for current factor
    cur_levels =  levels(my_data[[cur_factor]])
    num_levels = length(cur_levels)

    # Make list of terms to keep
    keep_terms = NULL
    for (level_name in cur_levels) {
      keep_terms = c(keep_terms, paste(cur_factor, level_name, sep = ""))
    }
    # loop through dataframe, reordering levels of current factor each time.
    for (cur_level in 1:(num_levels))
    {
      # Run current model.
      cur_model <- lmer(
        formula(my_equation),
        data = my_data,
        control = lmerControl(
          optimizer = "optimx",
          calc.derivs = FALSE,
          optCtrl = list(
            method = "nlminb",
            starttests = FALSE,
            kkt = FALSE
          )
        )
      )


      # Tidy the model.
      cur_model_tidy <- tidy(cur_model) %>%
        # Retain fixed factors only
        filter(effect == "fixed") %>%
        # remove unnecessary columns
        select(-c(effect, group)) %>%
        # add bonferroni adjusted p.values.
        bonferroniAdjust(bf_adj, exclude_terms = c("genderM", "fin_phonL%")) %>%
        ############# I KNOW I NEED TO FIX THIS! ^^^^^^^^^^^^^^^^^^^^^^^^^^
        # Tidy up numbers.
        mutate(
          estimate = round(estimate, 3),
          std.error = round(std.error, 3),
          statistic = round(statistic, 3),
          df = round(df, 2)
        ) %>%
        rename(t.value = statistic)

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
        select(
          "term",
          "estimate",
          "std.error",
          "2.5% CI",
          "97.5% CI",
          "t.value",
          "df",
          "p.value",
          "p.adj"
        )

      cur_model_tidy <-cur_model_tidy %>%
        # Prepare current model for pasting to all models output.
        # Make 'pairwise' column = intercept.
        mutate(
          pairwise =
            if_else(
              str_replace_all(term, "([\\*\\:])", "\\\\\\1") == "(Intercept)",
              "intercept",
              if_else(
                term %notin% keep_terms,
                "N/A",
                keep_terms[cur_level])
            ),
          # change 'term' so "intercept" states the target condition name.
          term =
            if_else(str_replace_all(term, "([\\*\\:])", "\\\\\\1") == "(Intercept)",
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

  # Get ready to chance p.adj column to show bonferroni adjustment.
  p_adj_new = paste("p.adj. (bf=", bf_adj, ")", sep="")
  p_adj_name = enquo(p_adj_new)


  # Get intercepts and pairwise comparisons tables
  all_models_tidy <- all_models_tidy %>%
    relocate(pairwise) %>%
    sigCodesTidy(p.adj)

  my_intercepts <- tidyIntercepts(all_models_tidy) %>%
    rename(!!p_adj_name := p.adj)
  my_pairwise <- tidyPairwise(all_models_tidy)  %>%
    rename(!!p_adj_name := p.adj)

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

  return(list("intercepts" = my_intercepts, "pairwise" = my_pairwise))
}

################################################################################
tidyIntercepts <- function(all_models_tidy)
{
  return(
      filter(all_models_tidy, pairwise == "intercept") %>%
      select(-pairwise) %>%
      rename(intercept = term) %>%

      mutate(
        p.value = if_else(
          p.value < 0.001,
          as.character(scientific(p.value), digits = 2),
          as.character(round(p.value, 4), digits = 2)
        ),
        p.adj = if_else(
          p.adj < 0.001,
          as.character(scientific(p.adj, digits = 2)),
          as.character(round(p.adj, 4))
        )
      )
  )
}

################################################################################
tidyPairwise <- function(all_models_tidy)
{
  return(
    filter(all_models_tidy, pairwise %notin% c("intercept", "N/A")) %>%
      select(
        pairwise,
        term,
        estimate,
        std.error,
        `2.5% CI`,
        `97.5% CI`,
        t.value,
        df,
        p.value,
        p.adj,
        `signif. `
      ) %>%
      rename(intercept = pairwise, slope = term) %>%
      mutate(
        p.value = if_else(
          p.value < 0.001,
          as.character(scientific(p.value), digits = 2),
          as.character(round(p.value, 4), digits = 2)
        ),
        p.adj = if_else(
          p.adj < 0.001,
          as.character(scientific(p.adj, digits = 2)),
          as.character(round(p.adj, 4))
        )
      )
  )
}









