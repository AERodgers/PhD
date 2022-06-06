library(lmerTest)
source('getNucTable.R')

#nucs <- nucs[nucs$mode != "DEC",]
x = "H_t ~ mode + (1 + mode*frame| speaker)"

xP <- as.array(c("L", "H", "X", "L_t", "H_t", "S"))
order <- as.array(c("DEC", "WHQ", "YNQ", "DCQ", "WHQ", "YNQ", "DCQ", "DEC", "YNQ", "DCQ", "DEC", "WHQ", "DCQ", "DEC", "WHQ",  "YNQ"))
dim(order) <- c(4, 4)

for(i in 1:4) {nucs$mode <- factor(nucs$mode, levels = order[,i])
curIcpt = order[1,i]
curSlps = order[2:4,i]
cat(curIcpt, "\n")
source('runNucModelTests.R')
source('printModelTestSummaries.R')
#source('temp.R')
}

### graphing area
nuc.model = nuc.model.H.trimmed
p<-step(nuc.model)
plot(p, effs = c("frame", "speaker"))
p

par(mfrow=c(2,1))
plot(fitted(nuc.model), residuals(nuc.model))
qq = qqnorm(residuals(nuc.model))
ab = lm(qq$y ~ qq$x)
abline(ab[1], ab[2], lwd=2, col = "blue")
remove(qq, ab)
