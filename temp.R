make_printable<- function(my_tibble, my_column)
{

    my_column <-(arg=my_column)
    mutate(my_tibble,
           my_column = str_replace(my_column, "\\*", "\\\\*"),
           my_column = str_replace(my_column, "\\[", "\\\\["),
           my_column = str_replace(my_column, "\\^", "\\\\^"),
           my_column = str_replace(my_column, "\\>", "\\\\>")
    )
    return(my_tibble)
}

balancedData(m_corpus_short,
             stim, acc_phon,
             "",
             11,
             5,
             use_pa_hierarchy = FALSE) %>%
    rename(mode = stim) %>%
    mutate(mode = substr(mode, 1, 3)) %>% group_by(mode) %>%
    pivot_longer(2:last_col(), "acc_phon") %>%
    mutate(acc_phon = factor(
        acc_phon,
        levels = c("H*", "L*H", "^[L*]H", ">H*", "L*^[H]", "^[L*H]")
    ),
    mode = factor(mode, levels = c("MDC", "MWH", "MYN", "MDQ"))) %>%
    group_by(acc_phon, mode) %>%
    summarise(acc_count = sum(value)) %>%
    pivot_wider(names_from = mode,
                values_from = acc_count,
                values_fill = 0) %>%
    make_printable(acc_phon) %>%
    formattable(
        caption = "Distibution of nuclear pitch accents by mode in M-corpus",
        list(area(col = 2:5) ~ proportion_bar(color = c(
            rep("#9e7cd9", 6),
            rep("#ff8131", 6),
            rep("#fdb863", 6),
            rep("#d2cbf2", 6)
        )))
    )
