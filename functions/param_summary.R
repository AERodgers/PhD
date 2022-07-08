# author: "Antoin Eoin Rodgers"
# date: '2022-05-06'
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

