mutate(
    acc_phon = str_replace(acc_phon, "\\*", "\\\\*"),
    acc_phon = str_replace(acc_phon, "\\[", "\\\\["),
    acc_phon = str_replace(acc_phon, "\\^", "\\\\^"),
    acc_phon = str_replace(acc_phon, "\\>", "\\\\>")
)

rename_all(list(~str_replace_all(., "([\\*\\[\\^\\>])", "\\\\\\1")))

mutate(acc_phon = str_replace_all(acc_phon, "([\\*\\[\\^\\>])", "\\\\\\1")) %>%


    mutate(phr_phon = str_replace_all(
        phr_phon, "([\\*\\[\\^\\>])", "\\\\\\1"
        )) %>%

rename(
    `H\\*` = `H*`,
    `L\\*H` = `L*H`,
    `^\\[L\\*]H` = `^[L*]H`,
    `L\\^\\[H\\*]` = `L*^[H]`,
    `\\^\\[L\\*H]` = `^[L*H]`,
    `\\>H*` = `>H*`
) %>%


mutate(tot_PAs = sum(c_across()),
       across(2:7, ~ round(5* . / tot_PAs)),
       mode = factor(mode, levels = c("MDC", "MWH", "MYN", "MDQ"))
)



uncount gender table

%>%
    pivot_longer(3:last_col(), "acc_phon") %>%
    uncount(value) %>%
    mutate(acc_phon = factor(acc_phon, levels = c("H*", "L*H", "^[L*]H", ">H*", "L*^[H]", "^[L*H]")))
