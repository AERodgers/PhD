# Get A_corpus data
A_corpus <- read_csv("../data/a_corpus.csv") %>%
    filter(code %notin% c("F15_A1422_1", "M10_A2422_1", "F5_H0422_2")) %>% # remove surplus reps for balance
    filter(
        cur_foot == 1,
        substr(stim, 2, 4) == metre_ID
    ) %>%
    select(
        speaker, gender, stim,
        init_phon, ana_syls, foot_syls, tot_syls,
        acc_phon, wrd_end_syl,
        ana_end_t, phr_end_t,
        wrd_fin_syl_start_t, wrd_end_t, ana_end_t,
        foot_start_t, foot_end_t,
        v_onset_t, l_t, h_t,
        v_sylNormT, l_sylNormT, h_sylNormT,
        l_f0, h_f0, mean_st, lh_slope
    ) %>%
    mutate(
        d_f0 = h_f0 - l_f0,
        d_t = h_t - l_t,
        l_t = l_t - v_onset_t,
        h_t = h_t - v_onset_t,
        l_tNorm = l_sylNormT - v_sylNormT,
        h_tNorm = h_sylNormT - v_sylNormT,
        foot_dur_t = foot_end_t - foot_start_t,
        SR = 1000 * tot_syls / phr_end_t,
        isLH = acc_phon %in% "L*H",
        stim = factor(stim,levels = unique(stim)),
        genderedDh = factor(
            ifelse(acc_phon == ">H*" & gender == "F",
                   "H*",
                   ifelse(acc_phon == ">H*" & gender == "M",
                          "L*H",
                          as.character(acc_phon)
                          )
                   ),
            levels = unique(acc_phon)
            )
        ) %>%
    rename(ana_dur = ana_end_t) %>%
    select(-c("phr_end_t", "tot_syls", "v_onset_t")) %>%
    convert(fct(speaker:wrd_end_syl))


# Get FOOT subset
pn_foot <- filter(A_corpus, stim %in% c("A0131", "A0221", "A0321", "H0422")) %>%
    mutate(acc_phon = factor(acc_phon,
                             levels = c("(*)", "L*", "H*", ">H*", "L*H"),
                             ),
           stim = factor(stim, levels = unique(stim)),
           genderedDh = factor(genderedDh, levels = unique(genderedDh))
           )


# Get ANAC subset
pn_anac <- filter(A_corpus, stim %in% c("H0422", "A1422", "A2422", "A3422")) %>%
    mutate(acc_phon = factor(acc_phon,
                             levels = c("(*)", "L*", "H*", ">H*", "L*H"),
                             ),
           stim = factor(stim, levels = unique(stim)),
           genderedDh = factor(genderedDh, levels = unique(genderedDh))
           )


pn_all <- filter(A_corpus, stim %in% c("A0131",
                                      "A0221",
                                      "A0321",
                                      "H0422",
                                      "A1422",
                                      "A2422",
                                      "A3422")) %>%
    mutate(acc_phon = factor(acc_phon,
                             levels = c("(*)", "L*", "H*", ">H*", "L*H"),
    ),
    stim = factor(stim, levels = unique(stim)),
    genderedDh = factor(genderedDh, levels = unique(genderedDh))
    )

# Remove A_corpus
rm(A_corpus)
