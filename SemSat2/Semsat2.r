raw <- subset(Semsat2_compiled, ((BIAS=="dominant" | BIAS=="subordinate")
              & ANSWER=="M") | (BIAS=="unrelated" & ANSWER=="C"),select=c(
                PID,BIAS,REPS,Elapsed,MS,STDEV,IQR))

raw <- within(raw, {
  BIAS <- factor(BIAS)
  REPS <- factor(REPS)
  PID <- factor(PID)
  #MS <- numeric(MS)
  #STDEV <- numeric(STDEV)
  #IQR <- numeric(IQR)
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

with(raw.stdev, plot(Elapsed,STDEV))
fit <- with(raw.stdev, lm(STDEV~BIAS*REPS*Elapsed))
summary(fit)

layout(matrix(c(1,2),2,1))

domlong <- subset(raw.stdev, BIAS=="dominant" & REPS=="30", select=c(Elapsed,STDEV,REPS))
with(domlong, interaction.plot(Elapsed,REPS,STDEV))

sublong <- subset(raw.stdev, BIAS=="subordinate" & REPS=="30", select=c(Elapsed,STDEV,REPS))
with(sublong, interaction.plot(Elapsed,REPS,STDEV))

domshort <- subset(raw.stdev, BIAS=="dominant" & REPS=="3", select=c(Elapsed,STDEV,REPS))
with(domshort, interaction.plot(Elapsed,REPS,STDEV))

subshort <- subset(raw.stdev, BIAS=="subordinate" & REPS=="3", select=c(Elapsed,STDEV,REPS))
with(subshort, interaction.plot(Elapsed,REPS,STDEV))

unlong <- subset(raw.stdev, BIAS=="unrelated" & REPS=="30", select=c(Elapsed,STDEV,REPS))
with(unlong, interaction.plot(Elapsed,REPS,STDEV))

unshort <- subset(raw.stdev, BIAS=="unrelated" & REPS=="3", select=c(Elapsed,STDEV,REPS))
with(unshort, interaction.plot(Elapsed,REPS,STDEV))