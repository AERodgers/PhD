library("RColorBrewer")
source("PN_foot_data_retrieval.R")
## create frequency tables for PN_foot acc/ft_syls

# LIMIT GENDER
#PN_foot = PN_foot[PN_foot$gender == "M",]

# CONFLATE FT_SYL CONDITIONS
#PN_foot = PN_foot[PN_foot$ft_syls != "4",]
PN_foot$ft_syls <- factor(PN_foot$ft_syls, levels = c("1", "2", "3", "4"))


#DELETION
#########

#DELETE >H*
#PN_foot = PN_foot[PN_foot$acc != ">H*",]

#DELETE L*
#PN_foot = PN_foot[PN_foot$acc != "L*",]

#DELETE (*)
#PN_foot = PN_foot[PN_foot$acc != "(*)",]


# ACCENT CONFLATION
###################

#CONFLATE L* and (*)
PN_foot$acc[PN_foot$acc == "L*"] <- "(*)"

#CONFLATE >H* and H*
PN_foot$acc[PN_foot$acc == ">H*"] <- "H*"

#CONFLATE non L*H
PN_foot$acc[PN_foot$acc == "H*"] <- "(*)"

#CONFLATE >H* and L*H
#PN_foot$acc[PN_foot$acc == ">H*"] <- "L*H"

#CONFLATE >H* BASED ON GENDER
#PN_foot$acc[PN_foot$acc == ">H*" & PN_foot$gender == "F"] <- "H*"
#PN_foot$acc[PN_foot$acc == ">H*" & PN_foot$gender == "M"] <- "L*H"

#remove any empty factorial levels
PN_foot$acc <- factor(PN_foot$acc, levels = unique(PN_foot$acc))
PN_foot$ft_syls <- factor(PN_foot$ft_syls, levels = unique(PN_foot$ft_syls))

PN_foot$acc <- factor(PN_foot$acc, levels = c("H*", "L*H", "(*)", "other"))
#CREATE OTHER
PN_foot$acc[PN_foot$acc == "(*)"] <- "other"
#remove any empty factorial levels
PN_foot$acc <- factor(PN_foot$acc, levels = unique(PN_foot$acc))
PN_foot$ft_syls <- factor(PN_foot$ft_syls, levels = unique(PN_foot$ft_syls))


# create contingency tables
PN_foot.acc.freq = table(PN_foot$ft_syls, PN_foot$acc)
PN_foot.acc.prop = round(prop.table(PN_foot.acc.freq,1)*100)
print(t(PN_foot.acc.prop))
summary(PN_foot.acc.prop)





colourMe = brewer.pal(5, "Spectral")
myTitle = "PN dist. (M) re foot size (L*H vs other)"

mosaicplot(PN_foot.acc.prop, main = myTitle, col = colourMe)
legend("bottomright", legend = c("L*H", "other"), col =  colourMe, pch = 15)





### GLMER
test <- glmer(acc ~ ft_syls + gender + (1|speaker),
           data = PN_foot,
           family = binomial(link = "logit"),
           control = glmerControl(optimizer = "optimx", calc.derivs = FALSE,
                                  optCtrl = list(method = "nlminb",
                                                 starttests = FALSE, kkt = FALSE)),
           nAGQ = 25)
print(summary(test))
