raw <- semsat2_compiled_csv
raw <- within(raw, {
  BIAS <- factor(BIAS)
  HomType <- factor(HomType)
  REPS <- factor(REPS)
  PID <- factor(PID)
  STDEV <- MS.3.STDEV
  IQR <- MS.IQR
})

unambig <- subset(raw, 
                  (BIAS=="related" & ANSWER=="M") | 
                    (BIAS=="unrelated" & HomType=="filler" & ANSWER=="C"), 
                  select=c(PID,BIAS,HomType,REPS,ANSWER,MS,STDEV,IQR))

View(unambig)

unambig <- within(unambig, {
  BIAS <- factor(BIAS) 
  REPS <- factor(REPS) 
  PID <- factor(PID)
})

raw <- subset(raw, (BIAS=="dominant" & ANSWER=="M") |
                (BIAS=="subordinate" & ANSWER=="M") |
                (BIAS=="unrelated" & (HomType=="NV" | HomType=="NN")
                 & ANSWER=="C"),
              select=c(PID,BIAS,HomType,REPS,
                       ANSWER,MS,STDEV,IQR))
View(raw)

raw.stdev <- subset(raw,select=c(PID,BIAS,HomType,
                                 REPS,ANSWER,STDEV))
raw.stdev <- na.omit(raw.stdev)

unambig.stdev <- subset(unambig,select=c(PID,BIAS,HomType,
                                 REPS,ANSWER,STDEV))

unambig.stdev <- na.omit(unambig.stdev)

raw.IQR <- subset(raw,select=c(PID,BIAS,HomType,
                               REPS,ANSWER,IQR))
raw.IQR <- na.omit(raw.IQR)

raw.aov <- aov(MS ~ BIAS*HomType*REPS+Error(PID),data=raw)
summary(raw.aov)

raw.stdev.aov <- aov(STDEV ~ BIAS*HomType*REPS+Error(PID),
                     data=raw.stdev)
summary(raw.stdev.aov)

raw.IQR.aov <- aov(IQR ~ BIAS*HomType*REPS+Error(PID),
                     data=raw.IQR)
summary(raw.IQR.aov)

unambig.aov <- aov(MS ~ BIAS*REPS+Error(PID),
                         data=unambig)
summary(unambig.aov)

unambig.stdev.aov <- aov(STDEV ~ BIAS*REPS+Error(PID),
                     data=unambig.stdev)
summary(unambig.stdev.aov)

with(unambig,interaction.plot(REPS,BIAS,MS))
with(unambig.stdev,interaction.plot(REPS,BIAS,STDEV))