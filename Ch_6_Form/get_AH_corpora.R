# LOAD AND PROCESS AH CORPUS
# --------------------------

## Get per-speaker f0 stats.
#gen_f0_stats <- as_tibble(read.csv("../data/GenStats_a_corpus.csv"))

#corpus <- as_tibble(read.csv("CH_6_Form/data/a_corpus_audited.csv")) %>%
corpus <- as_tibble(read.csv("data/a_corpus_audited.csv")) %>%
  # Only keep pertinent columns!
  select(
    speaker,
    gender,
    stim,
    cur_foot,
    init_phon,
    fin_phon,
    wrd_end_syl,
    ana_syls,
    foot_syls,
    tot_syls,
    acc_phon,
    ana_end_t,
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
    lh_slope,
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

    # treat foot_syls and ana_syls as factor
    ana_syls = factor(ana_syls, levels = unique(ana_syls)),
    foot_syls = factor(foot_syls, levels = unique(foot_syls)),
    wrd_end_syl = factor(wrd_end_syl, levels = 1:3),
    # Ignore downstep.
    acc_phon = str_replace(acc_phon, "!", ""),
    # Arrange PA levels according to hypothesized hierarchy.
    acc_phon = factor(acc_phon, levels = c("(*)", "L*", "H*", ">H*", "L*H")),
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
    ),
    gender = factor(gender, level = unique(gender)),
    fin_phon = factor(fin_phon, level = unique(fin_phon)),
    foot_syls = factor(foot_syls, levels=(1:4))
  ) %>%
  #rename lh_slope!
  rename(lh_slope = lh_slope) %>%
  # Remove columns which have outlived their use!
  select(-c(
    v_onset_t,
    foot_end_t,
    foot_start_t,
    tot_syls,
    phr_end_t,
    spkr_f0_mean,
    spkr_f0_SD
  ))


# CREATE PN SUBSETS
# -----------------

# Extract PN data.
pn <- filter(corpus, cur_foot == 1) %>%
  unite(init_contour,
        init_phon,
        acc_phon,
        sep = " ",
        remove = FALSE) %>%
  select(-c(e_f0, e_f0_z, e_f0_exc, e_f0_exc_z, e_t, e_grand_mean_t, he_dur))
# Get subset of all data for PN analysis

# Extract PN anacrusis data.
pn_ana <- pn %>%
  filter(stim %in% c("A0423", "A1422", "A2422", "A3422")) %>%
  # Remove columns which are no longer needed for pn_ana analysis.
  select(-c(cur_foot, wrd_end_syl, foot_syls))

# Make PN foot-size dataset.
pn_foot <- pn %>%
  filter(stim %in% c("A0131", "A0221", "A0321", "A0423")) %>%
  # remove columns no longer needed for pn_foot analysis.
  select(-c(ana_syls,fin_phon, wrd_end_syl,cur_foot)) %>%
  mutate(foot_syls = factor(foot_syls, levels = c(1, 2, 3, 4)))


# Make PN word-boundary dataset.
pn_lex <- pn %>% filter(stim %in% c(
  "A0321", "H0322", "H0433", "A0423",  "H1321", "H1322"
))

# CREATE NUCLEAR PA SUBSETS
# -------------------------

nuc <- filter(corpus, cur_foot == 2) %>%
  # remove columns which are no needed for nuclear PA analysis.
  select(-init_phon,-ana_syls) %>%
  # Create nuclear contour column.
  unite(nuc_contour,
        acc_phon,
        fin_phon,
        sep = " ",
        remove = FALSE) %>%
  # Get number of preceding syllables from stim code
  mutate(pre_syls = as.integer(str_sub(stim, 3, 3)) - 1,
         pre_syls = factor(pre_syls, levels = unique(pre_syls))
  ) %>%
  select(-c(s_f0, s_f0_z, s_t, s_grand_mean_t))

# Make nuclear PA preceding syllable dataset.
nuc_pre <- nuc %>%
  filter(stim %in% c("A1111", "A0221", "A0321", "A0423"))


# Make nuclear PA foot size dataset.
nuc_foot <- nuc %>%
  # Get dataset for syllables preceding nuclear PA.
  filter(stim %in% c("A1211", "A0221", "A1231", "A1241"))
