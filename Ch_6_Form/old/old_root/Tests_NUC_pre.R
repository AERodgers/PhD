# SCRIPT RUNS LMER (Linear Mixed Model) ANALYSIS ON NUCLEAR DATA FRAME

# GET DATA FRAME FOR SUBSECTION OF CORPUS WITH PRECEDING COUNT AS MAIN FACTOR

load("~/github/R-Alignment_Analysis_One/data/NUC.RData")
#NUC.pre <- NUC[(NUC$stim %in% c("A1111", "A0221", "A0321", "A0423")),]
NUC.pre <- NUC[(NUC$stim %in% c("A0221", "A0321", "A0423")),]

write.table(NUC.pre, "data/NUC_pre.txt", sep = "\t")
# RUN TESTS

NUC.pre.mdl.L_t = lmer(L_t ~ PrN_unStr + gender + (1 + PrN_unStr | speaker),
                   data = NUC.pre,
                   control = lmerControl(optimizer = "optimx",
                                         calc.derivs = FALSE,
                                         optCtrl = list(method = "nlminb",
                                                        starttests = FALSE,
                                                        kkt = FALSE)
                                         )
                   )


NUC.pre.mdl.H_t = lmer(H_t ~ PrN_unStr + gender + (1 + PrN_unStr | speaker),
                   data = NUC.pre,
                   control = lmerControl(optimizer = "optimx",
                                         calc.derivs = FALSE,
                                         optCtrl = list(method = "nlminb",
                                                        starttests = FALSE,
                                                        kkt = FALSE)
                                         )
                   )

NUC.pre.mdl.DH_t = lmer(D_t ~ PrN_unStr + gender + (1 + PrN_unStr | speaker),
                       data = NUC.pre,
                       control = lmerControl(optimizer = "optimx",
                                             calc.derivs = FALSE,
                                             optCtrl = list(method = "nlminb",
                                                            starttests = FALSE,
                                                            kkt = FALSE)
                                             )
                       )

NUC.pre.mdl.L_fo = lmer(L_fo  ~ PrN_unStr + gender + (1 + PrN_unStr | speaker),
                    data = NUC.pre,
                    control = lmerControl(optimizer = "optimx",
                                          calc.derivs = FALSE,
                                          optCtrl = list(method = "nlminb",
                                                         starttests = FALSE,
                                                         kkt = FALSE)
                                          )
                    )

NUC.pre.mdl.H_fo  = lmer(H_fo ~ PrN_unStr + gender + (1 + PrN_unStr | speaker),
                     data = NUC.pre,
                     control = lmerControl(optimizer = "optimx",
                                           calc.derivs = FALSE,
                                           optCtrl = list(method = "nlminb",
                                                          starttests = FALSE,
                                                          kkt = FALSE)
                                           )
                      )

NUC.pre.mdl.R_fo  = lmer(R_fo ~ PrN_unStr + gender + (1 + PrN_unStr | speaker),
                     data = NUC.pre,
                     control = lmerControl(optimizer = "optimx",
                                           calc.derivs = FALSE,
                                           optCtrl = list(method = "nlminb",
                                                          starttests = FALSE,
                                                          kkt = FALSE)
                                           )
                      )

NUC.pre.mdl.slope = lmer(slope  ~ PrN_unStr + gender + (1 + PrN_unStr | speaker),
                     data = NUC.pre,
                     control = lmerControl(optimizer = "optimx",
                                           calc.derivs = FALSE,
                                           optCtrl = list(method = "nlminb",
                                                          starttests = FALSE,
                                                          kkt = FALSE)
                                           )
                      )

NUC.pre.mdl.med = lmer(med  ~ PrN_unStr + gender + (1 + PrN_unStr | speaker),
                   data = NUC.pre,
                   control = lmerControl(optimizer = "optimx",
                                         calc.derivs = FALSE,
                                         optCtrl = list(method = "nlminb",
                                                        starttests = FALSE,
                                                        kkt = FALSE)
                                         )
                   )

NUC.pre.mdl.mean = lmer(mean  ~ PrN_unStr + gender + (1 + PrN_unStr | speaker),
                    data = NUC.pre,
                    control = lmerControl(optimizer = "optimx",
                                          calc.derivs = FALSE,
                                          optCtrl = list(method = "nlminb",
                                                         starttests = FALSE,
                                                         kkt = FALSE)
                                          )
                    )
