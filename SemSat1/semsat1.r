<<<<<<< HEAD
# Makes a copy of our data
raw <- SemSat1_compiled_csv

# treats our within subjects variables as factors
# renames a couple of ugly variables
raw <- within(raw, {
  BIAS <- factor(BIAS)
  RELATEDNESS <- factor(RELATEDNESS)
  REPS <- factor(REPS)
  PID <- factor(PID)
  STDEV <- MS.3.STDEV
  IQR <- X1.5.IQR
})

# sets up a data frame for our unambiguous items
unambig <- subset(raw, BIAS=="filler" &
                  ((RELATEDNESS=="related" & ANSWER=="M") | 
                    (RELATEDNESS=="unrelated" & ANSWER=="C")), 
                  select=c(PID,BIAS,RELATEDNESS,REPS,ANSWER,MS,STDEV,IQR))

View(unambig)

# makes sure that everything's read as a factor
unambig <- within(unambig, {
  BIAS <- factor(BIAS) 
  REPS <- factor(REPS)
  RELATEDNESS <- factor(RELATEDNESS)
  PID <- factor(PID)
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

# same thing for our filler items
unambig.stdev <- subset(unambig,select=c(PID,BIAS,RELATEDNESS,
                                 REPS,ANSWER,STDEV))
unambig.stdev <- na.omit(unambig.stdev)

############################################################

ambig.aov <- with(raw.stdev, aov(STDEV~BIAS*RELATEDNESS*REPS+
                             Error(PID/(BIAS*RELATEDNESS*REPS)
                                   )))
summary(ambig.aov)

filler.aov <- with(unambig, aov(STDEV~RELATEDNESS*REPS+
                                  Error(PID/(RELATEDNESS*REPS)
                                        )))
summary(filler.aov)

############################################################



# and now for the interquartile range trimming
raw.IQR <- subset(raw,select=c(PID,BIAS,RELATEDNESS,
                               REPS,ANSWER,IQR))
raw.IQR <- na.omit(raw.IQR)

# layout(matrix(c(1,2,3,4),2,2))
# 
# with(raw, interaction.plot(REPS,BIAS,MS,
#    ylab="Mean RT",xlab="REPS",trace.label="BIAS"))
# 
# with(raw, interaction.plot(REPS,RELATEDNESS,MS,
#    ylab="Mean RT",xlab="REPS",trace.label="RELATEDNESS"))
# 
# with(raw.stdev, interaction.plot(REPS,BIAS,STDEV,
#    ylab="Mean RT, STDEV",xlab="REPS",trace.label="BIAS"))
# 
# with(raw.stdev, interaction.plot(REPS,RELATEDNESS,STDEV,
#    ylab="Mean RT, STDEV",xlab="REPS",trace.label="RELATEDNESS"))

# the analyses of variance...
raw.aov <- aov(MS ~ BIAS*RELATEDNESS*REPS+Error(PID),
               data=raw)
summary(raw.aov)

raw.stdev.aov <- aov(STDEV ~ BIAS*RELATEDNESS*REPS+Error(PID),
                     data=raw.stdev)
summary(raw.stdev.aov)

raw.IQR.aov <- aov(IQR ~ BIAS*RELATEDNESS*REPS+Error(PID),
                     data=raw.IQR)
summary(raw.IQR.aov)

unambig.aov <- aov(MS ~ RELATEDNESS*REPS+Error(PID),
                   data=unambig)
summary(unambig.aov)

unambig.stdev.aov <- aov(STDEV ~ RELATEDNESS*REPS+Error(PID),
                   data=unambig.stdev)
summary(unambig.stdev.aov)

with(unambig,interaction.plot(REPS,RELATEDNESS,MS))
with(unambig.stdev,interaction.plot(REPS,RELATEDNESS,STDEV))
=======
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
  IQR <- X1.5.IQR
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

# subsets the data to select the values trimmed
# along 1.5 times the IQR

raw.iqr <- subset(raw, select=c(PID,BIAS,RELATEDNESS,
                                REPS,ANSWER,IQR))
raw.iqr <- na.omit(raw.stdev)

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
>>>>>>> 0b11987ac2d7bc3424907f4d5fc90a3ae48e33b5
