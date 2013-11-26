raw <- semsat2_compiled_csv
raw <- within(raw, {
  BIAS <- factor(BIAS)
  HomType <- factor(HomType)
  REPS <- factor(REPS)
  PID <- factor(PID)
  STDEV <- MS.3.STDEV
  IQR <- MS.IQR
})

raw <- subset(raw, (BIAS=="dominant" & ANSWER=="M") |
                (BIAS=="subordinate" & ANSWER=="M") |
                (BIAS=="unrelated" & (HomType=="NV" | HomType=="NN")
                 & ANSWER=="C"),
              select=c(PID,BIAS,HomType,REPS,
                       ANSWER,MS,STDEV,IQR))

raw <- subset(raw, PID!=57 | PID!=42 | PID!=14,
              select=c(PID,BIAS,HomType,REPS,
                       ANSWER,MS,STDEV,IQR))
View(raw)

raw.stdev <- subset(raw,select=c(PID,BIAS,HomType,
                                 REPS,ANSWER,STDEV))
raw.stdev <- na.omit(raw.stdev)

raw.IQR <- subset(raw,select=c(PID,BIAS,HomType,
                               REPS,ANSWER,IQR))
raw.IQR <- na.omit(raw.IQR)

raw.aov <- aov(MS ~ BIAS*HomType*REPS+Error(PID/(BIAS*HomType*REPS)),data=raw)
summary(raw.aov)

raw.stdev.aov <- aov(STDEV ~ BIAS*HomType*REPS+Error(PID/BIAS*HomType*REPS),
                     data=raw.stdev)
summary(raw.stdev.aov)

raw.IQR.aov <- aov(IQR ~ BIAS*HomType*REPS+Error(PID),
                     data=raw.IQR)
summary(raw.IQR.aov)

with(raw,interaction.plot(REPS,HomType,MS))
with(raw.stdev,interaction.plot(REPS,BIAS,STDEV))