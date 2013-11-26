<<<<<<< HEAD
raw <- subset(semsat3_data_compiled, ((BIAS=="dominant" | BIAS=="subordinate")
              & ANSWER=="M") | (BIAS=="unrelated" & ANSWER=="C"),select=c(
                PID,BIAS,REPS,Elapsed,MS,STDEV,IQR))
raw <- within(raw, {
  BIAS <- factor(BIAS)
  REPS <- factor(REPS)
  PID <- factor(PID)
})

install.packages("car")
library(car)

raw$Elapsed <- with(raw,recode(Elapsed, "NA=0"))
View(raw)

fit <- with(raw, lm(MS~BIAS*REPS*Elapsed))
summary(fit)

fit.aov <- with(raw, aov(MS~BIAS*REPS*Elapsed+Error(PID)))
=======
raw <- subset(semsat3_data_compiled, ((BIAS=="dominant" | BIAS=="subordinate")
              & ANSWER=="M") | (BIAS=="unrelated" & ANSWER=="C"),select=c(
                PID,BIAS,REPS,Elapsed,MS,STDEV,IQR))
raw <- within(raw, {
  BIAS <- factor(BIAS)
  REPS <- factor(REPS)
  PID <- factor(PID)
})

install.packages("car")
library(car)

raw$Elapsed <- with(raw,recode(Elapsed, "NA=0"))
View(raw)

fit <- with(raw, lm(MS~BIAS*REPS*Elapsed))
summary(fit)

fit.aov <- with(raw, aov(MS~BIAS*REPS*Elapsed+Error(PID)))
>>>>>>> 0b11987ac2d7bc3424907f4d5fc90a3ae48e33b5
summary(fit.aov)