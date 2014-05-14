Semantic Satiation among Lexically Ambiguous Words: Experiment 1
================================================================

Acquiring the Data
------------------

We will first (1) download the raw CSV from github; (2) convert the relevant variables---meaning bias, repetitions, relatedness, and participant ID---to factors; and (3) create three subsets of the data: one using the full dataset, one using a dataset with outliers $\pm$ 3 standard deviations removed, and a third with outliers more than 1.5 times the IQR above Q3 or below Q1 removed. We will then drop any null values from the dependent variable (MS response time) column.


```r
download.file("https://raw.githubusercontent.com/faulconbridge/semsat/master/SemSat1/SemSat1a_compiled.csv", 
    "semsat1a.csv", "wget", extra = "--no-check-certificate")
```

```
## Warning: download had nonzero exit status
```

```r
semsat1 <- read.csv("semsat1.csv", header = TRUE, sep = ",")
```

```
## Warning: cannot open file 'semsat1.csv': No such file or directory
```

```
## Error: cannot open the connection
```

```r

# Makes a copy of our data
raw <- semsat1
```

```
## Error: object 'semsat1' not found
```

```r

# treats our within subjects variables as factors renames a couple of ugly
# variables
raw <- within(raw, {
    BIAS <- factor(BIAS)
    RELATEDNESS <- factor(RELATEDNESS)
    REPS <- factor(REPS)
    PID <- factor(PID)
    STDEV <- MS.3.STDEV
})
```

```
## Error: no applicable method for 'within' applied to an object of class
## "function"
```

```r

# strips out all filler items and incorrect responses
raw <- subset(raw, (BIAS == "dominant" | BIAS == "subordinate") & ((RELATEDNESS == 
    "related" & ANSWER == "M") | (RELATEDNESS == "unrelated" & ANSWER == "C")), 
    select = c(PID, BIAS, RELATEDNESS, REPS, ANSWER, MS, STDEV, IQR))
```

```
## Error: object 'BIAS' not found
```

```r

raw.mean <- aggregate(raw$MS, by = list(raw$PID, raw$BIAS, raw$RELATEDNESS, 
    raw$REPS), FUN = "mean")
```

```
## Error: object of type 'closure' is not subsettable
```

```r
colnames(raw.mean) <- c("PID", "BIAS", "RELATEDNESS", "REPS", "MS")
```

```
## Error: object 'raw.mean' not found
```

```r

# subsets the data to remove any null values in the RTs trimmed to 3
# standard deviations
raw.stdev <- subset(raw, select = c(PID, BIAS, RELATEDNESS, REPS, ANSWER, STDEV))
```

```
## Error: argument "subset" is missing, with no default
```

```r
raw.stdev <- na.omit(raw.stdev)
```

```
## Error: object 'raw.stdev' not found
```

```r
raw.mean.stdev <- aggregate(raw.stdev$STDEV, by = list(raw.stdev$PID, raw.stdev$BIAS, 
    raw.stdev$RELATEDNESS, raw.stdev$REPS), FUN = "mean")
```

```
## Error: object 'raw.stdev' not found
```

```r
colnames(raw.mean.stdev) <- c("PID", "BIAS", "RELATEDNESS", "REPS", "STDEV")
```

```
## Error: object 'raw.mean.stdev' not found
```

```r

# same thing trimming at +- 1.5 times the IQR
raw.iqr <- subset(raw, select = c(PID, BIAS, RELATEDNESS, REPS, ANSWER, IQR))
```

```
## Error: argument "subset" is missing, with no default
```

```r
raw.iqr <- na.omit(raw.iqr)
```

```
## Error: object 'raw.iqr' not found
```

```r
raw.mean.iqr <- aggregate(raw.iqr$IQR, by = list(raw.iqr$PID, raw.iqr$BIAS, 
    raw.iqr$RELATEDNESS, raw.iqr$REPS), FUN = "mean")
```

```
## Error: object 'raw.iqr' not found
```

```r
colnames(raw.mean.iqr) <- c("PID", "BIAS", "RELATEDNESS", "REPS", "IQR")
```

```
## Error: object 'raw.mean.iqr' not found
```


Performing the Analyses
-----------------------


```r
raw.aov <- with(raw.mean, aov(MS ~ BIAS * RELATEDNESS * REPS + Error(PID)))
```

```
## Error: object 'raw.mean' not found
```

```r

stdev.aov <- with(raw.mean.stdev, aov(STDEV ~ BIAS * RELATEDNESS * REPS + Error(PID)))
```

```
## Error: object 'raw.mean.stdev' not found
```

```r

iqr.aov <- with(raw.mean.iqr, aov(IQR ~ BIAS * RELATEDNESS * REPS + Error(PID)))
```

```
## Error: object 'raw.mean.iqr' not found
```

```
## Error: object 'raw.aov' not found
```

```
## Error: object 'stdev.aov' not found
```

```
## Error: object 'iqr.aov' not found
```


### ANOVA of the untrimmed data ###

```
## Error: object 'xt' not found
```


### ANOVA of the stdev-trimmed data ###

```
## Error: object 'xt2' not found
```


### ANOVA of the IQR-trimmed data ###

```
## Error: object 'xt3' not found
```


We can visualize these differences as:

```
## Error: object 'raw.stdev' not found
```

```
## Error: object 'raw.stdev' not found
```

```
## Error: object 'raw.stdev' not found
```


And we can more formally conduct follow-up tests where appropriate. First, we will look at the bias by relatedness interaction:

```
## Error: object 'raw.mean.stdev' not found
```

```
## Error: object 'raw.mean.stdev' not found
```

```
## Error: object 'raw.mean.stdev' not found
```

```
## Error: object 'raw.mean.stdev' not found
```

```
## Error: object 'domrel' not found
```

```
## Error: object 'domrel' not found
```

```
## Error: object 'subrel' not found
```

```
## Error: object 'domunrel' not found
```


Next, the relatedness by repetition interaction:

```
## Error: object 'raw.mean.stdev' not found
```

```
## Error: object 'raw.mean.stdev' not found
```

```
## Error: object 'raw.mean.stdev' not found
```

```
## Error: object 'raw.mean.stdev' not found
```

```
## Error: object 'relshort' not found
```

```
## Error: object 'relshort' not found
```

```
## Error: object 'rellong' not found
```

```
## Error: object 'unrelshort' not found
```

