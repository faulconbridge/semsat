# Makes a copy of our data
raw <- SemSat1_compiled

# treats our within subjects variables as factors
# renames a couple of ugly variables
raw <- within(raw, {
  BIAS <- factor(BIAS)
  RELATEDNESS <- factor(RELATEDNESS)
  REPS <- factor(REPS)
  PID <- factor(PID)
  STDEV <- MS.3.STDEV
})

# strips out all filler items and incorrect responses
raw <- subset(raw, (BIAS=="dominant" | BIAS=="subordinate") & 
                ((RELATEDNESS=="related" & ANSWER=="M") |
                (RELATEDNESS=="unrelated" & ANSWER=="C")),
              select=c(PID,BIAS,RELATEDNESS,REPS,
                       ANSWER,MS,STDEV,IQR))
View(raw)

# subsets the data to remove any null values
# in the RTs trimmed to 3 standard deviations
raw.stdev <- subset(raw,select=c(PID,BIAS,RELATEDNESS,
                                 REPS,ANSWER,STDEV))
raw.stdev <- na.omit(raw.stdev)

# same thing trimming at +- 1.5 times the IQR
raw.iqr <- subset(raw,select=c(PID,BIAS,RELATEDNESS,
                                 REPS,ANSWER,IQR))
raw.iqr <- na.omit(raw.iqr)

############################################################

raw.aov <- with(raw, aov(MS~BIAS*RELATEDNESS*REPS+
                             Error(PID/(BIAS*RELATEDNESS*REPS)
                                   )))
summary(raw.aov)

stdev.aov <- with(raw.stdev, aov(STDEV~BIAS*RELATEDNESS*REPS+
                                   Error(PID/(BIAS*RELATEDNESS*REPS)
                                         )))
summary(stdev.aov)

iqr.aov <- with(raw.iqr, aov(IQR~BIAS*RELATEDNESS*REPS+
                               Error(PID/(BIAS*RELATEDNESS*REPS)
                                     )))
summary(iqr.aov)

with(raw.stdev, interaction.plot(REPS,RELATEDNESS,STDEV,
                                 trace.label=deparse(substitute(trace.factor))))
with(raw.stdev, interaction.plot(REPS,BIAS,STDEV,
                                 trace.label=deparse(substitute(trace.factor))))
with(raw.stdev, interaction.plot(RELATEDNESS,BIAS,STDEV,
                                 trace.label=deparse(substitute(trace.factor))))