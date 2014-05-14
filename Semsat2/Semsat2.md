Semantic Satiation among Lexically Ambiguous Words: Experiment 2
================================================================

Acquiring the Data
------------------

We will first download the raw CSV from github and do a bit of preprossing on it:


```r
download.file("https://github.com/faulconbridge/semsat/raw/master/Semsat2/Semsat2_compiled.csv", 
    "semsat2.csv", "wget", extra = "--no-check-certificate")
semsat2 <- read.csv("semsat2.csv", header = TRUE, sep = ",")

raw <- semsat2

# Specify which variables should be treated as factors
raw <- within(raw, {
    BIAS <- factor(BIAS)
    HomType <- factor(HomType)
    REPS <- factor(REPS)
    PID <- factor(PID)
    STDEV <- MS.3.STDEV
    IQR <- MS.IQR
})

# Strip out all filler and incorrectly-answered items
raw <- subset(raw, (BIAS == "dominant" & ANSWER == "M") | (BIAS == "subordinate" & 
    ANSWER == "M") | (BIAS == "unrelated" & (HomType == "NV" | HomType == "NN") & 
    ANSWER == "C"), select = c(PID, BIAS, HomType, REPS, ANSWER, MS, STDEV, 
    IQR))

# Strip out participants who encountered bugs during the experiment
raw <- subset(raw, PID != 57 | PID != 42 | PID != 14, select = c(PID, BIAS, 
    HomType, REPS, ANSWER, MS, STDEV, IQR))

# Subset data to include untrimmed MS response times
raw.MS <- subset(raw, select = c(PID:MS))
raw.MS <- na.omit(raw.MS)

# Take the participant average RTs for each condition
raw.mean.MS <- aggregate(raw.MS$MS, by = list(raw.MS$PID, raw.MS$BIAS, raw.MS$HomType, 
    raw.MS$REPS), FUN = "mean")
colnames(raw.mean.MS) <- c("PID", "BIAS", "HomType", "REPS", "MS")

# Subset the data to remove outliers beyond +-3 sd
raw.stdev <- subset(raw, select = c(PID, BIAS, HomType, REPS, STDEV))
raw.stdev <- na.omit(raw.stdev)

# Take the participant average RTs for each condition

raw.mean.stdev <- aggregate(raw.stdev$STDEV, by = list(raw.stdev$PID, raw.stdev$BIAS, 
    raw.stdev$HomType, raw.stdev$REPS), FUN = "mean")
colnames(raw.mean.stdev) <- c("PID", "BIAS", "HomType", "REPS", "STDEV")

# Subset the data to remove outliers 1.5x IQR beyond Q1 or Q3
raw.IQR <- subset(raw, select = c(PID, BIAS, HomType, REPS, IQR))
raw.IQR <- na.omit(raw.IQR)

# Take the participant average RTs for each condition
raw.mean.IQR <- aggregate(raw.IQR$IQR, by = list(raw.IQR$PID, raw.IQR$BIAS, 
    raw.IQR$HomType, raw.IQR$REPS), FUN = "mean")
colnames(raw.mean.IQR) <- c("PID", "BIAS", "HomType", "REPS", "IQR")
```


Performing the Analyses
-----------------------
First we can look at the untrimmed data:

```r
raw.aov <- aov(MS ~ BIAS * HomType * REPS + Error(PID), data = raw.mean.MS)
```

<!-- html table generated in R 3.1.0 by xtable 1.7-3 package -->
<!-- Wed May 14 11:10:38 2014 -->
<TABLE border=1>
<TR> <TH>  </TH> <TH> Df </TH> <TH> Sum Sq </TH> <TH> Mean Sq </TH> <TH> F value </TH> <TH> Pr(&gt;F) </TH>  </TR>
  <TR> <TD> BIAS      </TD> <TD align="right"> 1 </TD> <TD align="right"> 4853496.45 </TD> <TD align="right"> 4853496.45 </TD> <TD align="right"> 0.19 </TD> <TD align="right"> 0.6614 </TD> </TR>
  <TR> <TD> HomType   </TD> <TD align="right"> 1 </TD> <TD align="right"> 12823397.06 </TD> <TD align="right"> 12823397.06 </TD> <TD align="right"> 0.51 </TD> <TD align="right"> 0.4769 </TD> </TR>
  <TR> <TD> REPS      </TD> <TD align="right"> 1 </TD> <TD align="right"> 90025073.96 </TD> <TD align="right"> 90025073.96 </TD> <TD align="right"> 3.59 </TD> <TD align="right"> 0.0619 </TD> </TR>
  <TR> <TD> Residuals </TD> <TD align="right"> 80 </TD> <TD align="right"> 2008699426.87 </TD> <TD align="right"> 25108742.84 </TD> <TD align="right">  </TD> <TD align="right">  </TD> </TR>
  <TR> <TD> BIAS              </TD> <TD align="right"> 2 </TD> <TD align="right"> 38663066.69 </TD> <TD align="right"> 19331533.34 </TD> <TD align="right"> 18.26 </TD> <TD align="right"> 0.0000 </TD> </TR>
  <TR> <TD> HomType           </TD> <TD align="right"> 1 </TD> <TD align="right"> 5594529.45 </TD> <TD align="right"> 5594529.45 </TD> <TD align="right"> 5.28 </TD> <TD align="right"> 0.0218 </TD> </TR>
  <TR> <TD> REPS              </TD> <TD align="right"> 1 </TD> <TD align="right"> 482447.51 </TD> <TD align="right"> 482447.51 </TD> <TD align="right"> 0.46 </TD> <TD align="right"> 0.4998 </TD> </TR>
  <TR> <TD> BIAS:HomType      </TD> <TD align="right"> 2 </TD> <TD align="right"> 7350351.61 </TD> <TD align="right"> 3675175.81 </TD> <TD align="right"> 3.47 </TD> <TD align="right"> 0.0315 </TD> </TR>
  <TR> <TD> BIAS:REPS         </TD> <TD align="right"> 2 </TD> <TD align="right"> 674669.51 </TD> <TD align="right"> 337334.75 </TD> <TD align="right"> 0.32 </TD> <TD align="right"> 0.7272 </TD> </TR>
  <TR> <TD> HomType:REPS      </TD> <TD align="right"> 1 </TD> <TD align="right"> 518789.63 </TD> <TD align="right"> 518789.63 </TD> <TD align="right"> 0.49 </TD> <TD align="right"> 0.4841 </TD> </TR>
  <TR> <TD> BIAS:HomType:REPS </TD> <TD align="right"> 2 </TD> <TD align="right"> 5567072.26 </TD> <TD align="right"> 2783536.13 </TD> <TD align="right"> 2.63 </TD> <TD align="right"> 0.0727 </TD> </TR>
  <TR> <TD> Residuals         </TD> <TD align="right"> 908 </TD> <TD align="right"> 961391831.98 </TD> <TD align="right"> 1058801.58 </TD> <TD align="right">  </TD> <TD align="right">  </TD> </TR>
   </TABLE>


Then trimming based on standard deviations:

```r
stdev.aov <- aov(STDEV ~ BIAS * HomType * REPS + Error(PID), data = raw.mean.stdev)
```

<!-- html table generated in R 3.1.0 by xtable 1.7-3 package -->
<!-- Wed May 14 11:10:38 2014 -->
<TABLE border=1>
<TR> <TH>  </TH> <TH> Df </TH> <TH> Sum Sq </TH> <TH> Mean Sq </TH> <TH> F value </TH> <TH> Pr(&gt;F) </TH>  </TR>
  <TR> <TD> BIAS      </TD> <TD align="right"> 1 </TD> <TD align="right"> 7873612.05 </TD> <TD align="right"> 7873612.05 </TD> <TD align="right"> 0.38 </TD> <TD align="right"> 0.5374 </TD> </TR>
  <TR> <TD> HomType   </TD> <TD align="right"> 1 </TD> <TD align="right"> 11262294.84 </TD> <TD align="right"> 11262294.84 </TD> <TD align="right"> 0.55 </TD> <TD align="right"> 0.4610 </TD> </TR>
  <TR> <TD> REPS      </TD> <TD align="right"> 1 </TD> <TD align="right"> 96854068.86 </TD> <TD align="right"> 96854068.86 </TD> <TD align="right"> 4.72 </TD> <TD align="right"> 0.0328 </TD> </TR>
  <TR> <TD> Residuals </TD> <TD align="right"> 80 </TD> <TD align="right"> 1641606933.76 </TD> <TD align="right"> 20520086.67 </TD> <TD align="right">  </TD> <TD align="right">  </TD> </TR>
  <TR> <TD> BIAS              </TD> <TD align="right"> 2 </TD> <TD align="right"> 25243306.61 </TD> <TD align="right"> 12621653.30 </TD> <TD align="right"> 22.25 </TD> <TD align="right"> 0.0000 </TD> </TR>
  <TR> <TD> HomType           </TD> <TD align="right"> 1 </TD> <TD align="right"> 1094335.32 </TD> <TD align="right"> 1094335.32 </TD> <TD align="right"> 1.93 </TD> <TD align="right"> 0.1652 </TD> </TR>
  <TR> <TD> REPS              </TD> <TD align="right"> 1 </TD> <TD align="right"> 2040907.80 </TD> <TD align="right"> 2040907.80 </TD> <TD align="right"> 3.60 </TD> <TD align="right"> 0.0582 </TD> </TR>
  <TR> <TD> BIAS:HomType      </TD> <TD align="right"> 2 </TD> <TD align="right"> 418252.47 </TD> <TD align="right"> 209126.24 </TD> <TD align="right"> 0.37 </TD> <TD align="right"> 0.6918 </TD> </TR>
  <TR> <TD> BIAS:REPS         </TD> <TD align="right"> 2 </TD> <TD align="right"> 767006.70 </TD> <TD align="right"> 383503.35 </TD> <TD align="right"> 0.68 </TD> <TD align="right"> 0.5089 </TD> </TR>
  <TR> <TD> HomType:REPS      </TD> <TD align="right"> 1 </TD> <TD align="right"> 821331.90 </TD> <TD align="right"> 821331.90 </TD> <TD align="right"> 1.45 </TD> <TD align="right"> 0.2292 </TD> </TR>
  <TR> <TD> BIAS:HomType:REPS </TD> <TD align="right"> 2 </TD> <TD align="right"> 3666420.60 </TD> <TD align="right"> 1833210.30 </TD> <TD align="right"> 3.23 </TD> <TD align="right"> 0.0400 </TD> </TR>
  <TR> <TD> Residuals         </TD> <TD align="right"> 908 </TD> <TD align="right"> 515093667.40 </TD> <TD align="right"> 567283.77 </TD> <TD align="right">  </TD> <TD align="right">  </TD> </TR>
   </TABLE>


And finally based on interquatrile range:

```r
iqr.aov <- aov(IQR ~ BIAS * HomType * REPS + Error(PID), data = raw.mean.IQR)
```

<!-- html table generated in R 3.1.0 by xtable 1.7-3 package -->
<!-- Wed May 14 11:10:38 2014 -->
<TABLE border=1>
<TR> <TH>  </TH> <TH> Df </TH> <TH> Sum Sq </TH> <TH> Mean Sq </TH> <TH> F value </TH> <TH> Pr(&gt;F) </TH>  </TR>
  <TR> <TD> BIAS      </TD> <TD align="right"> 2 </TD> <TD align="right"> 45028697.46 </TD> <TD align="right"> 22514348.73 </TD> <TD align="right"> 2.12 </TD> <TD align="right"> 0.1265 </TD> </TR>
  <TR> <TD> HomType   </TD> <TD align="right"> 1 </TD> <TD align="right"> 14608488.81 </TD> <TD align="right"> 14608488.81 </TD> <TD align="right"> 1.38 </TD> <TD align="right"> 0.2441 </TD> </TR>
  <TR> <TD> Residuals </TD> <TD align="right"> 80 </TD> <TD align="right"> 848850768.72 </TD> <TD align="right"> 10610634.61 </TD> <TD align="right">  </TD> <TD align="right">  </TD> </TR>
  <TR> <TD> BIAS              </TD> <TD align="right"> 2 </TD> <TD align="right"> 17773069.82 </TD> <TD align="right"> 8886534.91 </TD> <TD align="right"> 34.93 </TD> <TD align="right"> 0.0000 </TD> </TR>
  <TR> <TD> HomType           </TD> <TD align="right"> 1 </TD> <TD align="right"> 1034003.46 </TD> <TD align="right"> 1034003.46 </TD> <TD align="right"> 4.06 </TD> <TD align="right"> 0.0441 </TD> </TR>
  <TR> <TD> REPS              </TD> <TD align="right"> 1 </TD> <TD align="right"> 3136651.91 </TD> <TD align="right"> 3136651.91 </TD> <TD align="right"> 12.33 </TD> <TD align="right"> 0.0005 </TD> </TR>
  <TR> <TD> BIAS:HomType      </TD> <TD align="right"> 2 </TD> <TD align="right"> 935814.03 </TD> <TD align="right"> 467907.02 </TD> <TD align="right"> 1.84 </TD> <TD align="right"> 0.1595 </TD> </TR>
  <TR> <TD> BIAS:REPS         </TD> <TD align="right"> 2 </TD> <TD align="right"> 342143.48 </TD> <TD align="right"> 171071.74 </TD> <TD align="right"> 0.67 </TD> <TD align="right"> 0.5107 </TD> </TR>
  <TR> <TD> HomType:REPS      </TD> <TD align="right"> 1 </TD> <TD align="right"> 199345.74 </TD> <TD align="right"> 199345.74 </TD> <TD align="right"> 0.78 </TD> <TD align="right"> 0.3763 </TD> </TR>
  <TR> <TD> BIAS:HomType:REPS </TD> <TD align="right"> 2 </TD> <TD align="right"> 9811.62 </TD> <TD align="right"> 4905.81 </TD> <TD align="right"> 0.02 </TD> <TD align="right"> 0.9809 </TD> </TR>
  <TR> <TD> Residuals         </TD> <TD align="right"> 906 </TD> <TD align="right"> 230500108.53 </TD> <TD align="right"> 254415.13 </TD> <TD align="right">  </TD> <TD align="right">  </TD> </TR>
   </TABLE>

