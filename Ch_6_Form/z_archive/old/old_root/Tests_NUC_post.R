# SCRIPT RUNS LMER (Linear Mixed Model) ANALYSIS ON NUCLEAR DATA FRAME

# GET DATA FRAME FOR SUBSECTION OF CORPUS WITH PRECEDING COUNT AS MAIN FACTOR

load("~/github/R-Alignment_Analysis_One/data/NUC.RData")
NUC.post <- NUC[(NUC$stim %in% c("A1211", "A0221", "A1231", "A1241")),]
write.table(NUC.post, "data/NUC_post.txt", sep = "\t")

# RUN TESTS

# convergence error: remove random fact of speaker by slope of ft_syls
NUC.post.mdl.L_t = lmer(L_t ~ ft_syls + gender + (1 | speaker),
                   data = NUC.post,
                   control = lmerControl(optimizer = "optimx",
                                         calc.derivs = FALSE,
                                         optCtrl = list(method = "nlminb",
                                                        starttests = FALSE,
                                                        kkt = FALSE)
                                         )
                   )


NUC.post.mdl.H_t = lmer(H_t ~ ft_syls + gender + (1 + ft_syls | speaker),
                   data = NUC.post,
                   control = lmerControl(optimizer = "optimx",
                                         calc.derivs = FALSE,
                                         optCtrl = list(method = "nlminb",
                                                        starttests = FALSE,
                                                        kkt = FALSE)
                                         )
                   )

# convergence error: remove random fact of speaker by slope of ft_syls
NUC.post.mdl.D_t = lmer(D_t  ~ ft_syls + gender + (1 | speaker),
                        data = NUC.post,
                        control = lmerControl(optimizer = "optimx",
                                              calc.derivs = FALSE,
                                              optCtrl = list(method = "nlminb",
                                                             starttests = FALSE,
                                                             kkt = FALSE)
                        )
)

NUC.post.mdl.L_fo = lmer(L_fo  ~ ft_syls + gender + (1 + ft_syls | speaker),
                    data = NUC.post,
                    control = lmerControl(optimizer = "optimx",
                                          calc.derivs = FALSE,
                                          optCtrl = list(method = "nlminb",
                                                         starttests = FALSE,
                                                         kkt = FALSE)
                                          )
                    )

NUC.post.mdl.H_fo  = lmer(H_fo ~ ft_syls + gender + (1 + ft_syls | speaker),
                     data = NUC.post,
                     control = lmerControl(optimizer = "optimx",
                                           calc.derivs = FALSE,
                                           optCtrl = list(method = "nlminb",
                                                          starttests = FALSE,
                                                          kkt = FALSE)
                                           )
                      )

NUC.post.mdl.R_fo  = lmer(R_fo ~ ft_syls + gender + (1 + ft_syls | speaker),
                     data = NUC.post,
                     control = lmerControl(optimizer = "optimx",
                                           calc.derivs = FALSE,
                                           optCtrl = list(method = "nlminb",
                                                          starttests = FALSE,
                                                          kkt = FALSE)
                                           )
                      )

NUC.post.mdl.slope = lmer(slope  ~ ft_syls + gender + (1 + ft_syls | speaker),
                     data = NUC.post,
                     control = lmerControl(optimizer = "optimx",
                                           calc.derivs = FALSE,
                                           optCtrl = list(method = "nlminb",
                                                          starttests = FALSE,
                                                          kkt = FALSE)
                                           )
                      )

NUC.post.mdl.med = lmer(med  ~ ft_syls + gender + (1 + ft_syls | speaker),
                   data = NUC.post,
                   control = lmerControl(optimizer = "optimx",
                                         calc.derivs = FALSE,
                                         optCtrl = list(method = "nlminb",
                                                        starttests = FALSE,
                                                        kkt = FALSE)
                                         )
                   )

NUC.post.mdl.mean = lmer(mean  ~ ft_syls + gender + (1 + ft_syls | speaker),
                    data = NUC.post,
                    control = lmerControl(optimizer = "optimx",
                                          calc.derivs = FALSE,
                                          optCtrl = list(method = "nlminb",
                                                         starttests = FALSE,
                                                         kkt = FALSE)
                                          )
                    )


