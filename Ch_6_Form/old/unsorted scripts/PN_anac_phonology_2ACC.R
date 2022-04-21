library("RColorBrewer")
source("PN_anac_data_retrieval.R")
## create frequency tables for PN_anac acc/ana_syls

# LIMIT GENDER
#PN_anac = PN_anac[PN_anac$gender == "M",]

# CONFLATE FT_SYL CONDITIONS
#PN_anac = PN_anac[PN_anac$ana_syls != "4",]


#DELETION
#########

#DELETE >H*
#PN_anac = PN_anac[PN_anac$acc != ">H*",]

#DELETE L*
#PN_anac = PN_anac[PN_anac$acc != "L*",]

#DELETE (*)
#PN_anac = PN_anac[PN_anac$acc != "(*)",]


# ACCENT CONFLATION
###################

#CONFLATE L* and (*)
PN_anac$acc[PN_anac$acc == "L*"] <- "(*)"

#CONFLATE >H* and H*
PN_anac$acc[PN_anac$acc == ">H*"] <- "H*"

#CONFLATE non L*H
PN_anac$acc[PN_anac$acc == "H*"] <- "(*)"

#CONFLATE >H* and L*H
#PN_anac$acc[PN_anac$acc == ">H*"] <- "L*H"

#CONFLATE >H* BASED ON GENDER
#PN_anac$acc[PN_anac$acc == ">H*" & PN_anac$gender == "F"] <- "H*"
#PN_anac$acc[PN_anac$acc == ">H*" & PN_anac$gender == "M"] <- "L*H"

#remove any empty factorial levels
PN_anac$acc <- factor(PN_anac$acc, levels = unique(PN_anac$acc))
PN_anac$ana_syls <- factor(PN_anac$ana_syls, levels = unique(PN_anac$ana_syls))

PN_anac$acc <- factor(PN_anac$acc, levels = c("H*", "L*H", "(*)", "other"))
#CREATE OTHER
PN_anac$acc[PN_anac$acc == "(*)"] <- "other"
#remove any empty factorial levels
PN_anac$acc <- factor(PN_anac$acc, levels = unique(PN_anac$acc))
PN_anac$ana_syls <- factor(PN_anac$ana_syls, levels = unique(PN_anac$ana_syls))


# create contingency tables
PN_anac.acc.freq = table(PN_anac$ana_syls, PN_anac$acc, PN_anac$gender)
PN_anac.acc.prop = round(prop.table(PN_anac.acc.freq,1)*100)
print(t(PN_anac.acc.prop))
summary(PN_anac.acc.prop)

colourMe = brewer.pal(5, "Spectral")
myTitle = "PN dist. (M) re foot size (L*H vs other)"

mosaicplot(PN_anac.acc.prop, main = myTitle, col = colourMe)
legend("bottomright", legend = c("L*H", "other"), col =  colourMe, pch = 15)

### GLMER
#PN_anac$ana_syls <- factor(PN_anac$ana_syls, levels = c("3", "0", "1", "2"))

test <- glmer(acc ~ ana_syls + gender + (1|speaker),
           data = PN_anac,
           family = binomial(link = "logit"),
           control = glmerControl(optimizer = "optimx", calc.derivs = FALSE,
                                  optCtrl = list(method = "nlminb",
                                                 starttests = FALSE, kkt = FALSE)),
           nAGQ = 25)
print(summary(test))
