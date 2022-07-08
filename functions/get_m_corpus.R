# author: "Antoin Eoin Rodgers"
# date: '2022-05-06'
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
        fin_phon = factor(fin_phon, levels = unique(fin_phon)),

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
