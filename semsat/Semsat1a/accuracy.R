for(i in 1:length(semsat1a$ANSWER)) {
  if(semsat1a[i, 3] == "related") {
    if(semsat1a[i, 11] == "M") {
      semsat1a[i, 27] = 1
    } else {
      semsat1a[i, 27] = 0
    }
  } else {
    if(semsat1a[i, 11] == "C") {
      semsat1a[i, 27] = 1
    } else {
      semsat1a[i, 27] = 0
    }
  }
}

semsat <- subset(semsat1a, select = c(PID, BIAS, REPS, RELATEDNESS,
                                      ANSWER, MS, MS.3.STDEV, V27))
colnames(semsat) <- c("PID", "BIAS", "REPS", "RELATEDNESS",
                      "ANSWER", "MS", "MS.STDEV", "CORRECT")
semsat <- within(semsat, {
  BIAS <- factor(BIAS)
  RELATEDNESS <- factor(RELATEDNESS)
  REPS <- factor(REPS)
  PID <- factor(PID)
})

View(semsat)

accuracy <- data.frame(with(
  semsat, aggregate(
    CORRECT, by = list(
      PID, BIAS, REPS, RELATEDNESS),
    FUN = "sum")))

semsat$COUNT <- c(rep(1, length(semsat[,1])))

accuracy$COUNT <- with(
  semsat, aggregate(
    COUNT, by = list(
      PID, BIAS, REPS, RELATEDNESS),
    FUN = "sum")[,5])

accuracy$PROPORTION <- accuracy[,5] / accuracy[,6]

colnames(accuracy) <- c("PID", "BIAS", "REPS", "RELATEDNESS",
                      "CORRECT", "TOTAL", "PROPORTION")

View(accuracy)

fit <- lmer(PROPORTION ~ BIAS * RELATEDNESS * REPS +
              (1 | PID),
            data = accuracy)
summary(fit)
anova(fit)

fit2 <- lme(PROPORTION ~ BIAS * RELATEDNESS * REPS,
             random = (~1 | PID/BIAS/RELATEDNESS/REPS),
             data = accuracy)
summary(fit2)
anova(fit2)

fit3 <- aov(PROPORTION ~ BIAS * RELATEDNESS * REPS +
              Error(PID), data = accuracy)
summary(fit3)
