# Makes a copy of our data
raw <- Semsat1_compiled

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
                             Error(PID/(BIAS*RELATEDNESS*REPS))))
summary(raw.aov)

stdev.aov <- with(raw.stdev, aov(STDEV~BIAS*RELATEDNESS*REPS+
                                   Error(PID/(BIAS*RELATEDNESS*REPS))))
summary(stdev.aov)

iqr.aov <- with(raw.iqr, aov(IQR~BIAS*RELATEDNESS*REPS+
                               Error(PID/(BIAS*RELATEDNESS*REPS))))
summary(iqr.aov)

############################################################

with(raw.stdev, interaction.plot(REPS,RELATEDNESS,STDEV,
                                 trace.label=deparse(substitute(trace.factor))))
with(raw.stdev, interaction.plot(REPS,BIAS,STDEV,
                                 trace.label=deparse(substitute(trace.factor))))
with(raw.stdev, interaction.plot(RELATEDNESS,BIAS,STDEV,
                                 trace.label=deparse(substitute(trace.factor))))

############################################################

domrel <- subset(raw.stdev, RELATEDNESS=="related" & BIAS=="dominant",
                          select=c(STDEV))
domunrel <- subset(raw.stdev, RELATEDNESS=="unrelated" & BIAS=="dominant",
                 select=c(STDEV))
subrel <- subset(raw.stdev, RELATEDNESS=="related" & BIAS=="subordinate",
                 select=c(STDEV))
subunrel <- subset(raw.stdev, RELATEDNESS=="unrelated" & BIAS=="subordinate",
                   select=c(STDEV))

t.test(domrel,domunrel)
t.test(domrel,subrel)
t.test(subrel,subunrel)
t.test(domunrel,subunrel)

############################################################

relshort <- subset(raw.stdev, RELATEDNESS=="related" & REPS=="short",
                   select=c(STDEV))
rellong <- subset(raw.stdev, RELATEDNESS=="related" & REPS=="long",
                   select=c(STDEV))
unrelshort <- subset(raw.stdev, RELATEDNESS=="unrelated" & REPS=="short",
                   select=c(STDEV))
unrellong <- subset(raw.stdev, RELATEDNESS=="unrelated" & REPS=="long",
                  select=c(STDEV))

t.test(relshort,rellong)
t.test(relshort,unrelshort)
t.test(rellong,unrellong)
t.test(unrelshort,unrellong)