sigCodesTidy <- function(myTibble){
    myTibble <- mutate(
        myTibble,
        "signif. (adj.)" =
            if_else(
                p.adjusted < 0.001,'p<0.001',
                if_else(
                    p.adjusted < 0.01,
                    'p<0.01',
                    if_else(
                        p.adjusted < 0.05,
                        'p<0.05',
                        if_else(
                            p.adjusted < 0.1,
                            '(p<0.1)',
                            ''
                            )
                        )
                    )
                )
        )
    return(myTibble)
}
