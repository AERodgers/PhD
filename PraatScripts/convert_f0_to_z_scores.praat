form Convert corpus data to z_scores
    sentence corpus_db ..\Ch_6_Form\data\a_corpus_audited.csv
    sentence gen_stats GenStats_a_corpus.csv
endform

corpus_db = Read Table from comma-separated file: corpus_db$
gen_stats = Read Table from comma-separated file: gen_stats$
x##$ = Read Table from comma-separated file: corpus_db$