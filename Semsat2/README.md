SemSat2
=======

###Raw Data
RAW contains the raw individual participant data files, entirely unedited. Given that they're a little bit messy (columns don't always line up; participant ID isn't repeated on every row), I've included a utility to compile the data into a single csv file. You may find the source code for this in the Compile Data folder.

###Compiled Data

What I give is messy and an overall mediocre solution; however, if you throw this up onto a PHP server, it'll take and parse a CSV file containing the raw data from Experiment 2. I'd recommend first navigating to the directory where all of the raw data files are stored and concatenating them into one csv file via:
    
    WINDOWS:
    	> type *.txt > data.csv
      
    MAC/LINUX:
    	> cat *.txt > data.csv
    
You can then upload the data via this page and have returned to you a second CSV that cleans up all of the columns. (The raw files aren't particularly pretty: you could do it manually in Excel; however, I won't recommend it.

The only issue is that this is a memory hog and there's a ton of data to sift through. I cobbled this script together quickly, not really caring about memory constraints, so my bad there. You'll probably want to take your full CSV file and split it up about ever 35 participants. That'll just make life a bit easier passing things through the PHP script.

###Analyses
semsat2.R contains a simple script to perform a repeated measures ANOVA on the data contained in SemSat2\_compiled.csv. SemSat2\_compiled\_data.xlsx is much the same data, including participant condition means if you prefer to analyze the data in SPSS.

This Excel file contains 4 tabs: the raw data; accuracy information for each participant per condition; the participant condition means with no values dropped on the basis of overall accuracy; and participant condition means with participant data dropped if they had lower than 33% accuracy in 4 of the 8 conditions.

In both of the final tabs, participant mean data are reported with any values more than 3 standard deviations from each participant's mean response time dropped from the calculations. Any response time below 200ms was automatically dropped (a glitch in the program used allowed response times of 0ms, leading us to this lower bound).

In the raw output tab, participants responded to the relatedness prompts pressing either the C or M key. M indicated that the cue was related to the target; C indicated that they were unrelated.

Included also is an R markdown file that allows full reproducability of the analyses along with context where appropriate.