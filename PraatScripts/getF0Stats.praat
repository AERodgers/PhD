# Get F0 Stats
# ============
# Gets F0 means and standard deviations for target.
# 20/05/2022
#
# Global dictonaries: global variables suffixes with _G

## UI replacement
form AER PhD: Get F0 Means and Standard Deviation for Sub Corpus
    natural corpus 1
    natural analysis_set 1
    comment Save declaratives only?
    boolean dec_only 1
    boolean save_results 1
    comment NB: Output will be saved in the same directory as the script.
endform

# Load global dictionary.
@globalDictionaries
@getF0Stats: corpus, analysis_set, dec_only, save_results

include procs/getF0stats.proc
include procs/globalDictionaries.proc
