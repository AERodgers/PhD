# LOAD AND PROCESS AH CORPUS
# --------------------------

## Get per-speaker f0 stats.
stress <- read_csv("../4_data/stressed_syls.csv", show_col_types = F)
#corpus <- as_tibble(read.csv("D:/Users/antoi/GitHub/PhD/Ch_Form/data/a_corpus_audited.csv")) %>%
corpus <- as_tibble(read.csv("../4_data/a_corpus_audited.csv")) %>%
  # Only keep pertinent columns!
  select(
    speaker,
    gender,
    stim,
    sent,
    cur_foot,
    init_phon,
    fin_phon,
    wrd_end_syl,
    ana_syls,
    foot_syls,
    tot_syls,
    ana_text,
    ana_has_word_end,
    nuc_pre_text,
    nuc_is_new_word,
    acc_phon,
    ana_end_t,
    foot_start_t,
    foot_end_t,
    v_onset_t,
    l_t,
    h_t,
    s_t,
    e_t,
    foot_end_t,
    v_grand_mean_t,
    l_grand_mean_t,
    h_grand_mean_t,
    s_grand_mean_t,
    e_grand_mean_t,
    h_syl,
    h_syl_ratio,
    s_f0,
    l_f0,
    h_f0,
    e_f0,
    phr_end_t,
    wrd_end_t,
    lh_slope,
    spkr_f0_mean,
    spkr_f0_SD,
    spkr_f0_min,
    spkr_f0_med
  ) %>%
  rename(pn_new_word = ana_has_word_end) %>%
  rename(nuc_new_word = nuc_is_new_word) %>%
  mutate(
    across(any_of(ends_with("_f0")), ~ .- spkr_f0_med),
    # there must be a more efficient way of doing this but...
    # Create column of stressed syllables in foot 1.
    pn_str_syl = stim,
    pn_str_syl = str_replace(pn_str_syl, stress$stim[1], stress$pn_syl[1]),
    pn_str_syl = str_replace(pn_str_syl, stress$stim[2], stress$pn_syl[2]),
    pn_str_syl = str_replace(pn_str_syl, stress$stim[3], stress$pn_syl[3]),
    pn_str_syl = str_replace(pn_str_syl, stress$stim[4], stress$pn_syl[4]),
    pn_str_syl = str_replace(pn_str_syl, stress$stim[5], stress$pn_syl[5]),
    pn_str_syl = str_replace(pn_str_syl, stress$stim[6], stress$pn_syl[6]),
    pn_str_syl = str_replace(pn_str_syl, stress$stim[7], stress$pn_syl[7]),
    pn_str_syl = str_replace(pn_str_syl, stress$stim[8], stress$pn_syl[8]),
    pn_str_syl = str_replace(pn_str_syl, stress$stim[9], stress$pn_syl[9]),
    pn_str_syl = str_replace(pn_str_syl, stress$stim[10], stress$pn_syl[10]),
    pn_str_syl = str_replace(pn_str_syl, stress$stim[11], stress$pn_syl[11]),
    pn_str_syl = str_replace(pn_str_syl, stress$stim[12], stress$pn_syl[12]),
    pn_str_syl = str_replace(pn_str_syl, stress$stim[13], stress$pn_syl[13]),
    pn_str_syl = str_replace(pn_str_syl, stress$stim[14], stress$pn_syl[14]),
    pn_str_syl = str_replace(pn_str_syl, stress$stim[15], stress$pn_syl[15]),
    pn_str_syl = factor(pn_str_syl, levels = unique(pn_str_syl)),
    # Create column of stressed syllables in foot 2.
    nuc_str_syl = stim,
    nuc_str_syl = str_replace(nuc_str_syl, stress$stim[1], stress$nuc_syl[1]),
    nuc_str_syl = str_replace(nuc_str_syl, stress$stim[2], stress$nuc_syl[2]),
    nuc_str_syl = str_replace(nuc_str_syl, stress$stim[3], stress$nuc_syl[3]),
    nuc_str_syl = str_replace(nuc_str_syl, stress$stim[4], stress$nuc_syl[4]),
    nuc_str_syl = str_replace(nuc_str_syl, stress$stim[5], stress$nuc_syl[5]),
    nuc_str_syl = str_replace(nuc_str_syl, stress$stim[6], stress$nuc_syl[6]),
    nuc_str_syl = str_replace(nuc_str_syl, stress$stim[7], stress$nuc_syl[7]),
    nuc_str_syl = str_replace(nuc_str_syl, stress$stim[8], stress$nuc_syl[8]),
    nuc_str_syl = str_replace(nuc_str_syl, stress$stim[9], stress$nuc_syl[9]),
    nuc_str_syl = str_replace(nuc_str_syl, stress$stim[10], stress$nuc_syl[10]),
    nuc_str_syl = str_replace(nuc_str_syl, stress$stim[11], stress$nuc_syl[11]),
    nuc_str_syl = str_replace(nuc_str_syl, stress$stim[12], stress$nuc_syl[12]),
    nuc_str_syl = str_replace(nuc_str_syl, stress$stim[13], stress$nuc_syl[13]),
    nuc_str_syl = str_replace(nuc_str_syl, stress$stim[14], stress$nuc_syl[14]),
    nuc_str_syl = str_replace(nuc_str_syl, stress$stim[15], stress$nuc_syl[15]),
    nuc_str_syl = factor(nuc_str_syl, levels = unique(nuc_str_syl)),

    # Make interlocutor gender column
    partner_gender = factor(
      if_else(speaker %in%  c("F17", "M04", "M05", "M08"), "M", "F"),
      levels = c("F", "M")
      ),

    # create composite parameters for continuous data.
    foot_dur = foot_end_t - foot_start_t,
    foot_v_dur = foot_end_t - v_onset_t,
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
    # get ratio of h_t to foot_dur
    h_t_foot_ratio = (h_t - foot_start_t) /  (foot_end_t - foot_start_t),
    # Make L and H times relative to vowel onset (TBU).
    s_t = s_t - v_onset_t,
    l_t = l_t - v_onset_t,
    h_t = h_t - v_onset_t,
    e_t = e_t - v_onset_t,
   # s_grand_mean_t = s_grand_mean_t - v_grand_mean_t,
   # l_grand_mean_t = l_grand_mean_t - v_grand_mean_t,
   # h_grand_mean_t = h_grand_mean_t - v_grand_mean_t,
   # e_grand_mean_t = e_grand_mean_t - v_grand_mean_t,

    # treat foot_syls and ana_syls as factor
    ana_syls = factor(ana_syls, levels = unique(ana_syls)),
    foot_syls = factor(foot_syls, levels = unique(foot_syls)),
    wrd_end_syl = factor(wrd_end_syl, levels = 1:3),
    # remove errors in str
    sent = str_replace(sent, "\\.", "") %>% tolower() %>% str_trim(),
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
    foot_syls = factor(foot_syls, levels=(1:4)),
    ana_text = if_else(is.na(ana_text), "0", ana_text),
    ana_text = factor(ana_text, level = unique(ana_text)),
    # Make new new word the intercept.
    pn_new_word = factor(as.logical(pn_new_word), level = c(F, T)),
    nuc_new_word = factor(as.logical(nuc_new_word), level = c(F, T)),
    nuc_pre_text = if_else(is.na(nuc_pre_text), "0", nuc_pre_text),
    nuc_pre_text = factor(nuc_pre_text, level = unique(nuc_pre_text)),
  ) %>%

  # Remove columns which have outlived their use!
  select(-c(
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
        remove = F) %>%
  select(speaker,
         gender,
         partner_gender,
         sent,
         stim,
         ana_syls,
         foot_syls,
         wrd_end_syl,
         ana_text,
         pn_str_syl,
         pn_new_word,
         nuc_pre_text,
         acc_phon,
         speech_rate,
         v_onset_t,
         ana_end_t,
         foot_dur,
         l_t,
         l_f0,
         h_t,
         h_f0,
         h_t_foot_ratio,
         f0_exc,
         lh_dur,
         lh_slope,
         h_grand_mean_t,
         h_syl,
         h_syl_ratio,
         wrd_end_t,
         foot_end_t)

# Extract PN anacrusis data.
pn_ana <- pn %>%
  filter(stim %in% c("A0423", "A1422", "A2422", "A3422"))


# Make PN foot-size dataset.
pn_foot <- pn %>%
  filter(stim %in% c("A0131", "A0221", "A0321", "A0423"))

# Make PN word-boundary dataset.
pn_lex <- pn %>% filter(stim %in% c(
  "A0321", "H0322", "H0433", "A0423",  "H1321", "H1322"))

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
        remove = F) %>%
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

