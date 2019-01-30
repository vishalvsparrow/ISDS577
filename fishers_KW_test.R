#Example

Job <- matrix(c(1,2,1,0, 3,3,6,1, 10,10,14,9, 6,7,12,11), 4, 4,
              dimnames = list(income = c("< 15k", "15-25k", "25-40k", "> 40k"),
                              satisfaction = c("VeryD", "LittleD", "ModerateS", "VeryS")))
Job
fisher.test(Job)

# Start here
rm(list=ls(all.names = TRUE))
setwd('C:\\Users\\vishal\\Google Drive\\CSUF\\Spring_2018\\data_analysis')

dat = read.table('fishers_race_online_and_second.csv', header = TRUE, sep = "\t", row.names = 1, as.is = TRUE)

#dat1 = dat[1,]
#dat1 = dat1[,-which(colnames(dat1) == "Native.Other")]
#new_df <- rbind(dat,dat1)

#  Fisher's test
fisher.test(dat)

# remove a column for chi-squared
#dat = dat[, -c(3,6)]

# Chi-squared test
chisq.test(dat)

# Tests for ordinal data
# Read the data
dat.gpa = read.csv('kw_gpa_online_and_second.csv', header= TRUE, sep = '\t', na.strings = "#N/A", stringsAsFactors = TRUE)
dat.gpa.whitney = read.csv('whitney_gpa.csv', header= TRUE, sep = '\t', na.strings = "#N/A", stringsAsFactors = TRUE)
#levels(dat.gpa$ISDS.577)

colnames(dat.gpa.whitney)
colnames(dat.gpa)
# change column names (temp)
colnames(dat.gpa) <- c("survey", "value")
colnames(dat.gpa)

# combine the two (temp)
dat.gpa.combined <- rbind(dat.gpa, dat.gpa.whitney)

#library(Hmisc)
#rcorr(dat.gpa$value, dat.gpa.combined$value)

#dat.gpa1 = dat.gpa$ISDS.577
#dat.gpa2 = dat.gpa$ISDS.577.Survey

#dat.gpa1 = dat.gpa1[-which(dat.gpa1 == "")]
#dat.gpa1 = factor(dat.gpa1)
#dat.gpa1

nrow(dat.gpa.combined)
nrow(na.omit(dat.gpa.combined))

dat.gpa.combined <- na.omit(dat.gpa.combined)

table(dat.gpa)

#table(dat.gpa2)

#tbl.gpa = table(dat.gpa$ISDS.577, dat.gpa$ISDS.577.Survey)
#tbl.gpa

# Whitney-U test --> fails for multiple levels
wilcox.test(survey ~ value, data=dat.gpa) 
library('FSA')
Summarize(value ~ survey, data= dat.gpa)

# Kruskal-Wallis's test
kruskal.test(value ~ survey, data= dat.gpa.combined)


## 4/18/18

dat.campus.gpa <- read.csv('kw_gpa_campus.csv', header= TRUE, sep = ',', na.strings = "", stringsAsFactors = TRUE)
#dat.campus.gpa
kruskal.test(Value ~ Survey, data= dat.campus.gpa)
################################## KW TEST ##################################
## ISDS 577 reddit, online_friends
dat.reddit_online_friends.gpa <- read.csv('kw_gpa_reddit_online_friends.csv', header= TRUE, sep = ',', na.strings = "", stringsAsFactors = TRUE)
#dat.campus.gpa
dat.reddit_online_friends.gpa <- na.omit(dat.reddit_online_friends.gpa)
kruskal.test(Value ~ Survey, data= dat.reddit_online_friends.gpa)

################################## FISHER'S TEST ##################################
dat.reddit_online_friends.race <- read.table('fishers_race_online_and_reddit.csv', header = TRUE, sep = ",", na.strings = "", stringsAsFactors = TRUE, row.names = 1, as.is = TRUE)

fisher.test(dat.reddit_online_friends.race)

### Time listening to music per week

dat.reddit_online.time_per_week = read.csv('kw_time-spent_reddit_online.csv', header= TRUE, sep = ',', na.strings = "", stringsAsFactors = TRUE)
kruskal.test(Value ~ Survey, data = dat.reddit_online.time_per_week)

dat.campus.time_per_week = read.csv('kw_time-spent_campus.csv', header= TRUE, sep = ',', na.strings = "", stringsAsFactors = TRUE)
kruskal.test(Value ~ Survey, dat.campus.time_per_week)

### Age

dat.campus.age = read.csv('kw_age_campus.csv', header= TRUE, sep = ',', na.strings = "", stringsAsFactors = TRUE)
kruskal.test(Value ~ Survey, dat.campus.age)

### College Year
dat.campus.degree = read.csv('kw_degree_campus.csv', header= TRUE, sep = ',', na.strings = "", stringsAsFactors = TRUE)
kruskal.test(Value ~ Survey, dat.campus.degree)

### RACE
dat.campus.race = read.table('fishers_race_campus.csv', header = TRUE, sep = ",", na.strings = "", stringsAsFactors = TRUE, row.names = 1, as.is = TRUE)
fisher.test(dat.campus.race, simulate.p.value=TRUE, B=1e7)
