Model2 <- function(df, X) {
  library(lmerTest)
  colNum = grep(X, colnames(df))
  names(df)[colNum] <- "xParam"
  model = lmer(xParam ~ mode + (1 | speaker), data = df,
               control=lmerControl(optimizer = "bobyqa")
               )
  print(paste("Xparam =", xParam))
  return(model)
}

Model3 <- function(df, X) {
  library(lmerTest)
  colNum = grep(X, colnames(df))
  names(df)[colNum] <- "xParam"
  model = lmer(xParam ~ mode + (1 + mode | speaker), data = df,
               control=lmerControl(optimizer = "bobyqa")
               )
  print(paste("Xparam =", xParam))
  return(model)
}



Model5 <- function(df, X) {
  library(lmerTest)
  colNum = grep(X, colnames(df))
  names(df)[colNum] <- "xParam"
  model = lmer(xParam ~ mode + (1 | speaker) + (1 | frame), data = df,
               control=lmerControl(optimizer = "bobyqa")
               )
  print(paste("Xparam =", xParam))
  return(model)
}

Model6 <- function(df, X) {
  library(lmerTest)
  colNum = grep(X, colnames(df))
  names(df)[colNum] <- "xParam"
  model = lmer(xParam ~ mode + (1 | speaker) + (0 + mode | speaker) + (1|frame), data = df,
               control=lmerControl(optimizer = "bobyqa")
               )
  print(paste("Xparam =", xParam))
  return(model)
}

Model7 <- function(df, X) {
  library(lmerTest)
  colNum = grep(X, colnames(df))
  names(df)[colNum] <- "xParam"
  model = lmer(H ~ mode + (0 + mode | speaker) + (1 | frame), data = df,
               control=lmerControl(optimizer = "bobyqa")
               )
  print(paste("Xparam =", xParam))
  return(model)
}





Model4 <- function() {
  library(lmerTest)
  model = lmer(H ~ mode + (1 + mode | speaker) + (1 | frame), data = nucs,
               control=lmerControl(optimizer = "bobyqa"))
  return(model)
}

xP <- as.array(c("L", "H", "X", "L_t", "H_t", "S"))
order <- as.array(c("DEC", "WHQ", "YNQ", "DCQ", "WHQ", "YNQ", "DCQ", "DEC", "YNQ", "DCQ", "DEC", "WHQ"))
dim(order) <- c(4, 3)

for(i in 1:3) {nucs$mode <- factor(nucs$mode, levels = order[,i])
  model_H = lmer(L ~ mode + (1 + mode | speaker) + (1 | frame), data = nucs,
             control=lmerControl(optimizer = "bobyqa"))
  print(summary(x))
  print(anova(x))
  #p<-step(model_H)
  #print(p)
  #plot(p, effs = c("frame", "speaker"))
}

