#install.packages("Momocs")
library(reshape2)
#install.packages("VIM", keep_source=T)
library(Momocs)
library(ggplot2)
library(VIM)
library(Hmisc)
library("nnet")
library('rlist')

rm(list=ls(all.names = TRUE))
setwd(dirname(rstudioapi::getSourceEditorContext()$path))

data = read.csv('complete_data.csv', header = TRUE, stringsAsFactors = TRUE, na.strings = c('', 'N/A', '#N/A','NA'))
clean_data = read.csv('clean_data_lazy.csv', header = TRUE, stringsAsFactors = TRUE, na.strings = c('', 'N/A', '#N/A','NA'))

data = clean_data

######### column names vector
dput(names(data)) 
v <- list()
v$big5_items <- c("Extraverted_Enthusiastic", "Critical_Quarrelsome", 
                  "Dependable_SelfDisciplined", "Anxious_Upset", "Open.to.new.experiences_complex", 
                  "Reserved_Quiet", "Sympathetic_Warm", "Disorganized_Careless", 
                  "Calm_EmotionallyStable", "Conventional_Uncreative")
v$audio_rating <- c("Classical_Audio", "Jazz_Audio", "Hardrock_Audio", "Country_Audio", 
                    "Pop_Audio", "Rap_Audio")
v$genre_rating <- c("CountryMusic", "JazzMusic", "Rap.HipHop", "CalssicalMusic", 
                    "Soft.Rock.Pop", "HardRock.heavyMetal")
v$mood_items <- c("Fedup", "Gloomy", "Jittery", "Nervous", "Sad", "Calm")
##########

#data[,!(grepl("text", names(data), ignore.case = T))]
data <- data[, -which(names(data) %in% c("missing_count_big5", "missing_count_audio", "missing_count_genre"))]
data1 <- data[,-1]
data1 <- data1[, !(grepl("text", names(data1), ignore.case = T))]
data1 <- data1[, -which(names(data1) %in% v$big5_items)]
data1 <- data1[, -which(names(data1) %in% v$mood_items)] 
names(data1)<- abbreviate(names(data1))

####### data 1 is possibly for correlation
View(data1)

summary(data)

### Reorder factors
data$GPA_text <- ordered((data$GPA_text), levels(data$GPA_text)[c(4, 1:3)])

levels(data$EducationLevel_Text)
data$EducationLevel_Text <- ordered(data$EducationLevel_Text, levels(data$EducationLevel_Text)[c(4, 6, 5, 1, 3, 2)])

levels(data$Time_Spent_on_Music_Text)
data$Time_Spent_on_Music_Text <- ordered(data$Time_Spent_on_Music_Text, levels(data$Time_Spent_on_Music_Text)[c(6, 3, 4, 5, 1, 2, 7)])

### 

nrow(data)
nrow(na.omit(data))

data.campus <- data[data$Survey_Source %in% c("577", "577_Survey", "second_playlist"),]
nrow(data.campus)

data.reddit <- data[data$Survey_Source %in% c("Reddit"), ]

data.online <- data[!data$Survey_Source %in% c("577", "577_Survey", "second_playlist"),]
nrow(data.online)

View(data.online)


# describe
summary(data[, v$big5_items])
apply(data.campus[, v$big5_items], 2, mean)
apply(data.online[, v$big5_items], 2, mean)

# any missing values in big5?
data$missing_count_big5 <- apply(data[,v$big5_items], 1, function(X) sum(is.na(X)))
table(data$missing_count_big5)
# any missing values in genre rating?
data$missing_count_genre <- apply(data[,v$genre_rating], 1, function(X) sum(is.na(X)))
table(data$missing_count_genre)

# any missing data in audio  rating?
data$missing_count_audio <- apply(data[,v$audio_rating], 1, function(X) sum(is.na(X)))
table(data$missing_count_audio) #clearly the effect of Attrition

##################### Missing data handling
# complete cases for data
library(mice)
nrow(data1[!complete.cases(data1), ])

# missing.counts <- as.data.frame
(md.pattern(data[,-grep("text", names(data), ignore.case = TRUE)]))
#View(missing.counts)

# take everything after Negative.Relaxed.Mood
split.col.index <- which(names(data2) == "Negative.Relaxed.Mood")
split.col.index <- split.col.index+1 # no missing values past the Negative.Relaxed.Mood column

tmp.dat.split <- data2[,split.col.index:length(data2)]
tmp.dat.split <- tmp.dat.split[,-grep("text", names(tmp.dat.split), ignore.case = T)]
  
summary(aggr(tmp.dat.split, prop = F, numbers = T, axes= T, plot = T))

## Remove specific rows with appropriate missing data combinations
## MAYBE I do not need this
get.missing.comb <- function(col.name, tmp.dat.split) {
  
  t1 <- (is.na(tmp.dat.split[, which(names(tmp.dat.split) == col.name)])) # boolean mask
  #sum(t1)
  
  ## I notice that the GPA column has 35 missing examples. I need to target and remove only 13
  
  t2 <- ((complete.cases(tmp.dat.split[,-which(names(tmp.dat.split) == col.name)])))
  
  missing_gpa_rows <- intersect(rownames(tmp.dat.split[t1,]), rownames(tmp.dat.split[t2,]))
  
  return(missing_gpa_rows)
}

ind.miss.gpa <- get.missing.comb("GPA", tmp.dat.split)
#ind.miss.age <- get.missing.comb("Age", tmp.dat.split)

## impute GPA --> really needed?
data[ind.miss.gpa, which(names(data) == "GPA")] <- with(data[(ind.miss.gpa),], mean(na.omit(data[["GPA"]]))) 

View(data)

summary(aggr(data, prop = F, numbers = T, axes= T, plot = F))

#### Delete all rows with greater than 5 missing columns
del.missing.comb <- function(threshold, tmp.dat.split) {
  
  #wat <- (apply(tmp.dat.split, 1, function(x){ if(sum(is.na(x))>5) {(x)} }))
  
  return(data[-which(rowSums(is.na(tmp.dat.split)) > threshold),])
  
}
####### OUTPUT ###########
data1 <- (del.missing.comb(5, data[,-grep("text", names(data), ignore.case = T)]))

remove.data <- function(threshold = 5, tmp.dat.split) {
  
  tmp <- list()
  for(i in 1: nrow(tmp.dat.split)) {
  #print(rowSums(is.na(tmp.dat.split[i,])))
    if(rowSums(is.na(tmp.dat.split[i,-grep("text", names(tmp.dat.split), ignore.case = TRUE)])) > threshold) {
      tmp$ind.remove[i] <- TRUE
    }
    else {
      tmp$ind.remove[i] <- FALSE
      
        for(j in split.col.index:length(data)) {
          if((grepl("text", names(data[j]), ignore.case = T) == TRUE) | grepl( "audio", names(data[j]), ignore.case = T)
             | names(data[j]) == "PersonalChoice_MusicGenre") {
            #@vishal
            #print(i)
            #print(j)
            next
          }

          else if(is.na(tmp.dat.split[i, j])) {
            #print(tmp.dat.split[i,j])
            #print(colnames(tmp.dat.split[j]))
            tmp.dat.split[i, j] <-  mean(na.omit(tmp.dat.split[,j]))
            #tmp$dummy_value[i] <- mean(na.omit(data[,j]))
          }
        }
      }
  }
  
  tmp.dat.split <- (tmp.dat.split[!tmp$ind.remove, ])
  
  return(tmp.dat.split)
  }


clean_dat <- remove.data(5, data)
View(clean_dat)
nrow(clean_dat)

clean_dat_lazy <- (na.omit(clean_dat[,-grep("text", names(clean_dat), ignore.case = TRUE)]))

summary(aggr(clean_dat, prop = F, numbers = T, axes= T, plot = T))
summary(aggr(clean_dat[,-grep("text", names(clean_dat), ignore.case = TRUE)], prop = F, numbers = T, axes= T, plot = T))
#write.csv(clean_dat, file = "clean_data.csv")
#write.csv(clean_dat_lazy, "clean_data_lazy.csv")



############################# Working with clean_data

## ggplot missing values

ggplot_missing <- function(x){
  
  x %>% 
    is.na %>%
    melt %>%
    ggplot(data = .,
           aes(x = Var2,
               y = Var1)) +
    geom_raster(aes(fill = value)) +
    scale_fill_grey(name = "",
                    labels = c("Present","Missing")) +
    theme_minimal() + 
    theme(axis.text.x  = element_text(angle=45, vjust=0.5)) + 
    labs(x = "Variables in Dataset",
         y = "Rows / observations")
}

tmp.dat <- clean_data

ggplot_missing(tmp.dat)

split.col.index <- which(names(data) == "Negative.Relaxed.Mood")

split.col.index <- split.col.index+1 # no missing values past the Negative.Relaxed.Mood column

ggplot_missing(data[,split.col.index:length(data)]) #INCORRECT~ for clean_dat

#tmp.missing <- list()
for(j in 1:length(clean_dat)) {
  if(length(which(is.na(clean_dat[,j])))>0 & !grepl("text",names(clean_dat[j]), ignore.case = TRUE)) {
    #tmp.missing$clean_dat[j] <- which(is.na(clean_dat[,j]))
    print(names(clean_dat[j]))
    print(which(is.na(clean_dat[,j])))
  }
  
}

# Reddit race mean
reddit.race <- (na.omit(data.reddit$Race))
reddit.race

#table(is.na(data[,which(names(data) == "Negative.Relaxed.Mood")]))


#cor(data[, v$genre_rating])


##################### Correlation matrix -- use rcorr() to get p-values as well
#install.packages("Hmisc", dependencies = TRUE)
#install.packages("corrplot", dependencies = TRUE)
#install.packages("caret", dep="TRUE")

library("Hmisc")
library("corrplot")
#library("digest")

res_genre <- rcorr(as.matrix(clean_data[, v$genre_rating]), type = c("pearson","spearman"))
res_audio <- rcorr(as.matrix(clean_data[, v$audio_rating]), type = c("pearson","spearman"))

corrplot(res_genre$r, type = "upper", order = "hclust", p.mat = res_genre$P , sig.level = 0.01, insig = "blank")
corrplot(res_audio$r, type = "upper", order = "hclust", p.mat = res_genre$P , sig.level = 0.01, insig = "blank")

###################### Multi-collinearity
# @vishal clean the missing values first

#### Corrplot

### @vishal: get the right columns
res_all <- rcorr(as.matrix(data1[, !(grepl("text", names(data1), ignore.case = T))]), type =c("pearson", "spearman"))
corrplot(res_all$r, p.mat = res_all$P, sig.level = 0.01, insig = "blank", order = "hclust", type = "upper")
## column name mapping
abbreviate(names(data))

## Exploratory Factor Analysis

pcobj <- PCA((data[, v$big5_items]))
pcobj_audio <- PCA(na.omit(data[, v$audio_rating]))
pcobj_genre <- PCA(na.omit(data[, v$genre_rating]))
pcobj_mood <- PCA(data[, v$mood_items])

scree(pcobj_mood)
scree_plot(pcobj)
scree_plot(pcobj_audio)
scree_plot(pcobj_genre)

?factanal
# how to choose rotation? achieve goals of simple structure
# For difference between rating scales....
# 1 tailed t-test if one directional hypothesis
# A series of 1-way ANOVA --> by default gives one-way probability. Divide it by 2 for directional hypothesis

fit <- factanal(data[, v$big5_items], 5, rotation = "promax")
fit_audio <- factanal(na.omit(data[, v$audio_rating]), 3, rotation = "promax")
fit_genre <- factanal(na.omit(data[, v$genre_rating]), 3, rotation = "promax")
fit_mood <- factanal(na.omit(data[, v$mood_items]), 3, rotation = "promax")

print(fit_mood, cutoff = 0.3)
print(fit, cutoff=0.3)
print(fit_audio, cutoff = 0.3)
print(fit_genre, cutoff = 0.3)



########################################## START RESEARCH QUESTIONS HERE ################################33
################### Ordinal Logistic regression
#### What is their MOST favorite music choice?
# Personal_Choice_text is the dependent variable

levels(data$Personal_Choice_text)
p.t <- (table(data$Personal_Choice_text))
p.t <- as.data.frame(p.t)
plot.bar <- ggplot(p.t, aes(Var1, Freq))
plot.bar +geom_bar(stat = "identity") + xlab("Most Favorite Genres") + ylab("Frequency")


############################################################## IMPORTANT: SET SOFT ROCK/POP to the reference category
data$Personal_Choice_text <- relevel(data$Personal_Choice_text, ref = "Soft rock/Pop")


### Function to calculate p-value and std. errors

p_std <- function(obj) {
  
  z <- summary(obj)$coefficients/summary(obj)$standard.errors
  p <- (1 - pnorm(abs(z), 0, 1))*2
  
  return(list(p))
  
}
###


############ EXPERIMENT IGNORE because scaling does not make a difference
tmp.dat.exp <- data.frame(data[, -which(names(data) == "Personal_Choice_text")])
tmp.dat.exp <- tmp.dat.exp[, which(names(data) %in% c("Extraversion.Score..out.of.7.", "Openness.to.Experiences.Score..out.of.7.", "Agreeableness.Score..out.of.7.", "Emotional.Stability.Score..out.of.7.", "Conscientiousness.Score..out.of.7."))]
tmp.dat.exp <- data.frame(scale(tmp.dat.exp))
tmp.dat.exp$choice <- NA 
tmp.dat.exp$choice <- data[, which(names(data) == "Personal_Choice_text")]

test_exp <- multinom(choice ~ Extraversion.Score..out.of.7. + Openness.to.Experiences.Score..out.of.7. + Agreeableness.Score..out.of.7. + Emotional.Stability.Score..out.of.7. + Conscientiousness.Score..out.of.7., data = tmp.dat.exp)
summary(test_exp)
z <- summary(test_exp)$coefficients/summary(test_exp)$standard.errors
p <- (1 - pnorm(abs(z), 0, 1))*2
p
############### END



############################### Q: Are Big 5 and most fav. genre related? --> Let Malvika handle it all
test <- multinom(Personal_Choice_text ~ Extraversion.Score..out.of.7. + Openness.to.Experiences.Score..out.of.7. + Agreeableness.Score..out.of.7. + Emotional.Stability.Score..out.of.7. + Conscientiousness.Score..out.of.7., data = data)
summary(test)
z <- summary(test)$coefficients/summary(test)$standard.errors
p <- (1 - pnorm(abs(z), 0, 1))*2
p

################################ Filtering out the non-significant ones
test2 <- multinom(Personal_Choice_text ~ Openness.to.Experiences.Score..out.of.7. + Agreeableness.Score..out.of.7. + Emotional.Stability.Score..out.of.7., data = data)
summary(test2)
p2 <- p_std(test2)
p2

############################### Q: Is there a relationship between mood and most fav. genre?
test_mood <- multinom(Personal_Choice_text ~ Scaled_Negative.Relaxed_Mood_seven_point, data)
summary(test_mood)
z_mood <- summary(test_mood)$coefficients/summary(test_mood)$standard.errors
p_mood <- (1 - pnorm(abs(z_mood), 0, 1))*2
p_mood




################ Q2: Test for interaction effect with Mood, Personality, Personal Music Choice 
#### Relationship between Mood and Personal Choice (REDUNDANT)
test_choice_mood <- multinom((Personal_Choice_text) ~ Scaled_Negative.Relaxed_Mood_seven_point, data = data)
summary(test_choice_mood) 
p_choice_mood <- p_std(test_choice_mood)
p_choice_mood
### 


### Q2: Relationship between Most fav. and Big 5 and Mood
test_mood_personality <- multinom(Personal_Choice_text ~ 
                                    Openness.to.Experiences.Score..out.of.7. + Agreeableness.Score..out.of.7. + 
                                    Emotional.Stability.Score..out.of.7. + 
                                    Scaled_Negative.Relaxed_Mood_seven_point, data = data)
summary(test_mood_personality)
z_mood_personality <- summary(test_mood_personality)$coefficients/summary(test_mood_personality)$standard.errors
p_mood_personality <- (1 - pnorm(abs(z_mood_personality), 0, 1))*2
p_mood_personality

### Q2: Relationship between Mood and Big 5 and Mood

test_med <- lm(Scaled_Negative.Relaxed_Mood_seven_point ~ Openness.to.Experiences.Score..out.of.7. + Agreeableness.Score..out.of.7. + 
                 Emotional.Stability.Score..out.of.7., data = data)

summary(test_med)

test_med <- lm(Scaled_Negative.Relaxed_Mood_seven_point ~  
                 Emotional.Stability.Score..out.of.7., data = data)

summary(test_med)

test_med_pre <- multinom(Personal_Choice_text ~  
                         Emotional.Stability.Score..out.of.7., data=data)
summary(test_med_pre)
p_std(test_med_p)

test_med_f <- multinom(Personal_Choice_text ~  
                 Emotional.Stability.Score..out.of.7. + Scaled_Negative.Relaxed_Mood_seven_point, data = data)

summary(test_med_f)
p_med_f <- p_std(test_med_f)
p_med_f


########## IGNORE FOR NOW
test_countryMusic <- multinom((Personal_Choice_text) ~ Scaled_Negative.Relaxed_Mood_seven_point, data = data)
z_countryMusic <- summary(test_countryMusic)$coefficients/summary(test_countryMusic)$standard.errors
p_countryMusic <- (1 - pnorm(abs(z_countryMusic), 0, 1))*2
summary(test_countryMusic) 
p_countryMusic

###################################### Race and Age as a mediator
data$Race_text <- relevel(data$Race_text, ref = "Asian Indian")
data$Age_text <- relevel(data$Age_text, ref = "22 to 26")

test_race_music <- multinom((Personal_Choice_text) ~ Race_text, data=data)
summary(test_race_music)
p_std(test_race_music)

test_age_music <- multinom((Personal_Choice_text) ~ Age_text, data=data)
summary(test_age_music)
p_std(test_age_music)

test_age_person <- multinom(Race_text ~ Extraversion.Score..out.of.7. + Openness.to.Experiences.Score..out.of.7. + Agreeableness.Score..out.of.7. + Emotional.Stability.Score..out.of.7. + Conscientiousness.Score..out.of.7., data = data)
summary(test_age_person)
p_std(test_age_person)

test_race_person_choice <- multinom(Personal_Choice_text ~  Emotional.Stability.Score..out.of.7., data = data)

summary(test_race_person_choice)
p_std(test_race_person_choice)

####


########### Scale Country Music and Mood
test_countryMusic <- lm((CountryMusic) ~ Scaled_Negative.Relaxed_Mood_seven_point, data = data)
z_countryMusic <- summary(test_countryMusic)$coefficients/summary(test_countryMusic)$standard.errors
p_countryMusic <- (1 - pnorm(abs(z_countryMusic), 0, 1))*2
summary(test_countryMusic) 
p_countryMusic

#scale(data$Country_Audio)
#data$CountryMusic


#################TIME SPENT ON MUSIC WITH PERSONALITY

test_1 <- lm(Time.Spent.on.Music ~ Openness.to.Experiences.Score..out.of.7. + 
                     Agreeableness.Score..out.of.7. + Emotional.Stability.Score..out.of.7. + 
                     Conscientiousness.Score..out.of.7., data=data)
summary(test_1)
p_std(test_1)

#####################MEDIATION WITH COMMUTER

test_2 <- lm(Time.Spent.on.Music ~ factor(Commuter_text), data=data)
summary(test_2)

test_3 <- glm((commuter_binary) ~  Openness.to.Experiences.Score..out.of.7. + 
                     Agreeableness.Score..out.of.7. + Emotional.Stability.Score..out.of.7. + 
                     Conscientiousness.Score..out.of.7., data=data, family = "binomial")
summary(test_3)
p_std(test_3)

###### Country music with personality

test_countryMusic_personality <- lm(CountryMusic ~ Extraversion.Score..out.of.7. + Openness.to.Experiences.Score..out.of.7. + Agreeableness.Score..out.of.7. + Emotional.Stability.Score..out.of.7. + Conscientiousness.Score..out.of.7., data = data)
z_countryMusic_person <- summary(test_countryMusic_personality)$coefficients/summary(test_countryMusic_personality)$standard.errors
p_countryMusic_person <- (1 - pnorm(abs(z_countryMusic_person), 0, 1))*2
summary(test_countryMusic_personality) 
p_countryMusic_person

####### Jazz with personality
test_JazzMusic_personality_linear <- multinom(JazzMusic ~ Extraversion.Score..out.of.7. + Openness.to.Experiences.Score..out.of.7. + Agreeableness.Score..out.of.7. + Emotional.Stability.Score..out.of.7. + Conscientiousness.Score..out.of.7., data = data) 
p_std(test_JazzMusic_personality_linear)


#### question 7

test_q7 <- multinom(Personal_Choice_text ~Extraversion.Score..out.of.7. + Openness.to.Experiences.Score..out.of.7. + 
                      Agreeableness.Score..out.of.7. + Emotional.Stability.Score..out.of.7. + 
                      Conscientiousness.Score..out.of.7. + (Age_text) + (GPA_text),
              data = data) 
summary(test_q7)
p_std(test_q7)

test_q7_1 <- multinom(Personal_Choice_text ~(GPA_text),
                    data = data)

summary(test_q7_1)
p_std(test_q7_1)


test_q7_2 <- lm(Rap_Audio ~ factor(GPA_text) + factor(Age_text, ordered =T),
                      data = data) 

summary(test_q7_2)
p_std(test_q7_2)


###################### t-test for question 9

t.test(data$Country_Audio, data$CountryMusic, paired = TRUE, conf.level = 0.95) #yes
t.test(data$JazzMusic, data$Jazz_Audio, paired = TRUE, conf.level = 0.95) # yes
t.test(data$HardRock.heavyMetal, data$Hardrock_Audio, paired = T, conf.level = 0.95) # no
t.test(data$Classical_Audio, data$CalssicalMusic, paired = T, conf.level = 0.95) #no
t.test(data$Soft.Rock.Pop, data$Pop_Audio, paired = T, conf.level = 0.95) # yes
t.test(data$Rap.HipHop, data$Rap_Audio, paired = T, conf.level = 0.95) # yes

data$diff.country <- abs(data$Country_Audio - data$CountryMusic) #no
data$diff.jazz <- abs(data$JazzMusic - data$Jazz_Audio) #yes
data$diff.pop <- abs(data$Pop_Audio - data$Soft.Rock.Pop)
data$diff.rap <- abs(data$Rap.HipHop - data$Rap_Audio)

data$diff.country


test_5 <- lm(diff.country ~ Scaled_Negative.Relaxed_Mood_seven_point, data=data)
summary(test_5)


############### Question 11

dat.loc <- read.csv("filtered_location.csv", header = T, na.strings = '')
dat.loc <- dat.loc[, 1:3]
dat.loc
class(dat.loc$Location.Longitude)

test_3 <- multinom(choice ~ Location.Longitude + ï..Location.Latitude, data = dat.loc)
summary(test_3)
p_std(test_3)
