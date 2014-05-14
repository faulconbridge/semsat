raw <- subset(Semsat1b_compiled, ((BIAS=="dominant" | BIAS=="subordinate")
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

raw.stdev <- subset(raw, select=c(BIAS,REPS,PID,Elapsed,STDEV))
raw.stdev <- na.omit(raw.stdev)

raw.iqr <- subset(raw, select=c(BIAS,REPS,PID,Elapsed,IQR))
raw.iqr <- na.omit(raw.stdev)

raw.aov <- with(raw, aov(MS~BIAS*REPS*Elapsed+Error(PID/(BIAS*REPS))+Elapsed))
summary(raw.aov)

stdev.aov <- with(raw.stdev, aov(STDEV~BIAS*REPS*Elapsed+Error(PID/(BIAS*REPS))+Elapsed))
summary(stdev.aov)

iqr.aov <- with(raw.iqr, aov(IQR~BIAS*REPS*Elapsed+Error(PID/(BIAS*REPS))+Elapsed))
summary(iqr.aov)

dominant <- subset(raw.stdev, BIAS=="dominant", select=c(STDEV))
subordinate <- subset(raw.stdev, BIAS=="subordinate", select=c(STDEV))
unrelated <- subset(raw.stdev, BIAS=="unrelated", select=c(STDEV))

t.test(dominant,subordinate)
t.test(dominant,unrelated)
t.test(subordinate,unrelated)