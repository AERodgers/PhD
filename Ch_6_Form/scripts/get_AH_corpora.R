# LOAD AND PROCESS AH CORPUS
# --------------------------
gen_f0_stats <- as_tibble(read.csv("data/GenStats_a_corpus.csv"))

#
# NEEDS FIXING SO THAT F0 IS CONVERTED TO Z-SCORES. 
# PROBABLY NEEDS TO BE A FUNCTION WITH MULTIPLE OUTPUTS
# AND "F0 as z-score" as an input parameter
# WHY? --> BECAUSE COMPOSITE PARAMETERS LIKE EXCURSION SIZE NEED TO BE
# GENERATE FROM BASE VALUES CONVERTED AFTER THE FACT
# values, NOT CONERTED TO Z-SCORES AFTERWARDS
#
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
    foot_start_t,
    foot_end_t,
    v_onset_t,
    l_t,
    h_t,
    l_f0,
    h_f0,
    phr_end_t,
    slope_st
  ) %>% 
mutate(
  # create composite parameters for continuous data.
  foot_dur = foot_end_t - foot_start_t,
  speech_rate = round(tot_syls / phr_end_t * 1000, 3),
  f0_exc = h_f0 - l_f0,
  lh_dur = h_t - l_t,
  foot_dur = foot_end_t - foot_start_t,
  # Make L and H times relative to vowel onset (TBU).
  l_t = l_t - v_onset_t,
  h_t = h_t - v_onset_t,
  # treat foot_syls and ana_syls as factor
  ana_syls = factor(ana_syls, levels = unique(ana_syls)),
  foot_syls = factor(foot_syls, levels = unique(foot_syls)),
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
  )
) %>%
  #rename slope!
  rename(slope = slope_st) %>% 
  # Remove columns which have outlived their use!
  select(-c(v_onset_t,
            foot_end_t,
            foot_start_t,
            tot_syls,
            phr_end_t))


# CREATE PN SUBSETS
# -----------------

# Extract PN data.
pn <- filter(corpus, cur_foot == 1) %>%
  unite(init_contour,
        init_phon,
        acc_phon,
        sep = " ",
        remove = FALSE)
# Get subset of all data for PN analysis

# Extract PN anacrusis data.
pn_ana <- pn %>%
  filter(stim %in% c("A0423", "A1422", "A2422", "A3422")) %>%
  # Remove columns which are no longer needed for pn_ana analysis.
  select(-(cur_foot:wrd_end_syl),-fin_phon)

# Make PN foot-size dataset.
pn_foot <- pn %>%
  filter(stim %in% c("A0131", "A0221", "A0321", "A0423")) %>%
  # remove columns no longer needed for pn_foot analysis.
  select(-(ana_syls:cur_foot), -wrd_end_syl,-fin_phon) %>% 
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
        remove = FALSE)

# Make nuclear PA preceding syllable dataset.
nuc_pre <- nuc %>%
  filter(stim %in% c("A1111", "A0221", "A0321", "A0423")) %>%
  # Get number of preceding syllables from stim code
  mutate(pre_syls = str_sub(stim, 3, 3))  %>%
  mutate(pre_syls = as.integer(pre_syls))

# Make nuclear PA foot size dataset.
nuc_foot <- nuc %>%
  # Get dataset for syllables preceding nuclear PA.
  filter(stim %in% c("A1211", "A0221", "A1231", "A1241"))

