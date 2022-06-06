library(lmerTest)
library(optimx)

# models after step() using: PARAM ~ mode + (1 + mode + gender | speaker) + (1 | gender) + (1 | frame)
nuc.model.L   = lmer(L ~ mode + (1 + mode + gender | speaker) + (1 | gender), data = nucs,
                     control = lmerControl(optimizer = "optimx", calc.derivs = FALSE,
                     optCtrl = list(method = "nlminb", starttests = FALSE, kkt = FALSE)))
nuc.model.L_t = lmer(L_t ~ mode + (1 + mode | speaker) + (1 | frame), data = nucs,
                     control = lmerControl(optimizer = "optimx", calc.derivs = FALSE,
                     optCtrl = list(method = "nlminb", starttests = FALSE, kkt = FALSE)))
nuc.model.X   = lmer(X ~ mode + (mode | speaker), data = nucs,
                     control = lmerControl(optimizer = "optimx", calc.derivs = FALSE,
                     optCtrl = list(method = "nlminb", starttests = FALSE, kkt = FALSE)))
nuc.model.H   = lmer(H ~ mode + (mode | speaker) + (1 | gender), data = nucs,
                     control = lmerControl(optimizer = "optimx", calc.derivs = FALSE,
                     optCtrl = list(method = "nlminb", starttests = FALSE, kkt = FALSE)))
nuc.model.H_t = lmer(H_t ~ mode + (mode | speaker) + (1 | frame), data = nucs,
                     control = lmerControl(optimizer = "optimx", calc.derivs = FALSE,
                     optCtrl = list(method = "nlminb", starttests = FALSE, kkt = FALSE)))
nuc.model.S   = lmer(S ~ mode + (mode | speaker) + (1 | frame), data = nucs,
                     control = lmerControl(optimizer = "optimx", calc.derivs = FALSE,
                     optCtrl = list(method = "nlminb", starttests = FALSE, kkt = FALSE)))
nuc.model.D   = lmer(D ~ mode + (mode | speaker) + (1 | frame), data = nucs,
                     control = lmerControl(optimizer = "optimx", calc.derivs = FALSE,
                     optCtrl = list(method = "nlminb", starttests = FALSE, kkt = FALSE)))
nuc.model.K   = lmer(K ~ mode + (1 + mode + gender | speaker), data = nucs,
                     control = lmerControl(optimizer = "optimx", calc.derivs = FALSE,
                     optCtrl = list(method = "nlminb", starttests = FALSE, kkt = FALSE)))


# trim

nuc.model.D   = lmer(D ~ mode + (mode | speaker) + (1 | frame), data = nucs,
                          subset =  abs(scale(resid(nuc.model.D))) < 2,                        
                          control = lmerControl(optimizer = "optimx", calc.derivs = FALSE,
                                                optCtrl = list(method = "nlminb", starttests = FALSE, kkt = FALSE)))
