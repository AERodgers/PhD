library(lmerTest)
source('getNucTable.R')

order <- as.array(c("DEC", "WHQ", "YNQ", "DCQ", "WHQ", "YNQ", "DCQ", "DEC", "YNQ", "DCQ", "DEC", "WHQ", "DCQ", "DEC", "WHQ",  "YNQ"))
dim(order) <- c(4, 4)

for(i in 1:4) {nucs$mode <- factor(nucs$mode, levels = order[,i])
curIcpt = order[1,i]
cat(curIcpt, "\n")
source('runNucModelTests.R')
source('printModelTestSummaries.R')
source('temp.R')
}
