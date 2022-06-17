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
      mutate(acc_phon = factor(!!response_col, levels = c("(*)", "L*", "H*", ">H*", "L*H")))
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

bonferroniAdjust <- function(myTibble,
                             excludeTerms,
                             bonferroniMultiplier)
{
  myTibble <- mutate(myTibble,
                     p.adjusted = if_else(
                       term %in% excludeTerms,
                       p.value,
                       if_else(
                         p.value * bonferroniMultiplier >= 1,
                         0.9999,
                         p.value * bonferroniMultiplier
                       )
                     ))

  return(myTibble)
}


sigCodesTidy <- function(my_tibble, incl_marginal_sig = FALSE)
  # Create significance column in tibble using Bonferroni adjusted p.
{
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

        # treat foot_syls
        foot_syls = factor(foot_syls, levels = unique(foot_syls)),

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

