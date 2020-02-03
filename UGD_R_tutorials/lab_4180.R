##########################################
# PSYC 4180 - INTRO TO STATISTICS USING R
##########################################

##########################################
# PART I: ONE-WAY REPEATED-MEASURES ANOVA

##########################################
# check out R studio workspace

getwd() #this is where you are in your computer

##########################################
# what are packages? 
# R has a lot of great built-in capabilities
# packages are add-ons with many more functions that might be more specific, tailored

# we will be using the Tidyverse to explore R 
# the Tidyverse is a family of packages, all support each other seamlessly 

library(tidyverse) # see the packages attached

##########################################
# how to assign variables - let's input our own data
# imagine we run an experiment on 5 people where we give them a specific diet and follow how their
# weight changes in the next 3 months

# let's first write a few vectors
subject <- c(1:5, 1:5, 1:5) # here we have 5 subjects
time_mth <- c(1, 1, 1, 1, 1, # each subject gives us data from 3 different time points  
             2, 2, 2, 2, 2,
             3, 3, 3, 3, 3)
weight_kg <- c(98, 99, 101, 97, 97, # that data is their weight in kilograms
               80, 75, 80, 73, 78,
               70, 62, 55, 58, 70)

weight_df <- data.frame(subject, time_mth, weight_kg) # we combine those vectors together to create
# a "dataframe" which is a matrix that has all those datapoints

# you're going to see the %>% (pipe) operator a lot - it just means "do this next"
weight_kg %>% mean() 

##########################################
# describing the data

head(weight_df)
boxplot(weight_kg ~ time_mth , data = weight_df) # left side is dependent variable

library(psych) # this package has a handy function for getting descriptive statistics
describeBy(weight_df, group = time_mth)

##########################################
# plotting data

weight_df <- weight_df %>% 
  group_by(time_mth) %>% # for every time point, calculate the MEAN, STDEV, STANDARD ERROR
  mutate(mean_kg = mean(weight_kg), # this function "mutates" a new column with the new calculations
         sd_kg = sd(weight_kg),
         sem_kg = sd_kg/(sqrt(15)))
            
ggplot(weight_df, aes(x = time_mth, y = weight_kg)) +
  geom_bar(stat = "summary", fun.y = "mean") +
  geom_errorbar(aes(ymin= mean_kg - sem_kg, ymax = mean_kg + sem_kg), width = .2, size = 0.7) +
  theme_bw() +
  theme(axis.title = element_text(face = "bold"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  ylab("Mean weight per month (kg)")

##########################################
# statistical analysis 

# anatomy of an R model
# dependent_measure ~ factor1 + factor2, data = data_frame
# our dependent measure is measured by (~) factor 1 and factor 2
# this equation needs to go into a model function e.g. if we want anova we do aov(), if we want a regression, lm() 

# if we didn't have the SAME participants for every time point, how do we analyze it?
not_mod <- aov(weight_kg ~ time_mth, data = weight_df) # this doesn't output anything, we need summary()
summary(not_mod)

# however, we use the SAME participants across all time points, so we need to account 
# for individual error in a repeated-measures ANOVA
rm_mod1 <- aov(weight_kg ~ time_mth + Error(subject/time_mth),
               data = weight_df)
summary(rm_mod1)

##########################################
# interpreting data

# we say that there is a significant effect of time in the weight loss of these participants
# who received an intervention (F(1,11) = ..., p < 0.05).

# if it's significant, do post-hoc testing 
# month 1 vs. month 2
t.test(weight_df$weight_kg[which(weight_df$time_mth == 1)],
       weight_df$weight_kg[which(weight_df$time_mth == 2)])

# month 1 vs. month 3
t.test(weight_df$weight_kg[which(weight_df$time_mth == 1)],
       weight_df$weight_kg[which(weight_df$time_mth == 3)])

# month 2 vs. month 3
t.test(weight_df$weight_kg[which(weight_df$time_mth == 2)],
       weight_df$weight_kg[which(weight_df$time_mth == 3)])
# account for type 1 error - bonferroni p level control
# 0.05 / 3 = 0.0167 ; all are significant at this level

##########################################
# PART II: MIXED ANOVA

# using the same data set we will now add a new factor
# what if we have another group whose weight we tracked but didn't get a diet (i.e. our controls)

wdf <- read.csv('lab_4180_data1.csv', header = TRUE)

# let's add labels and means that will be used for plots later
wdf <- wdf %>%
  group_by(time_mth, diet) %>%
  mutate(mean_kg = mean(weight_kg),
         sd_kg = sd(weight_kg),
         sem_kg = sd_kg/(sqrt(5)))

# now let's take a look at descriptive statistics again...
head(wdf)
boxplot(weight_kg ~ time_mth + diet, data = wdf)

# let's get the descriptive statistics
describeBy(wdf, group = c('time_mth', 'diet'))

# stats - include now an interaction term factor1*factor2
rm_mod2 <- aov(weight_kg ~ time_mth + diet + time_mth*diet + Error(subject/time_mth),
               data = wdf)
summary(rm_mod2) # there is a significant effect of time on weight (F(1,24) = 49.21, p < .05), 
# and diet on weight (F(1,24) = 30.76, p < .05)

# posthoc - since we saw an effect of interaction
# do a posthoc for each main effect - not necessary for diet since there's only two,
# do it for the other factor

# month 1 vs. month 2 - PAIRWISE T TESTS
t.test(weight_df$weight_kg[which(weight_df$time_mth == 1)],
       weight_df$weight_kg[which(weight_df$time_mth == 2)],
       paired = TRUE)

# month 1 vs. month 3
t.test(weight_df$weight_kg[which(weight_df$time_mth == 1)],
       weight_df$weight_kg[which(weight_df$time_mth == 3)],
       paired = TRUE)

# month 2 vs. month 3
t.test(weight_df$weight_kg[which(weight_df$time_mth == 2)],
       weight_df$weight_kg[which(weight_df$time_mth == 3)],
       paired = TRUE)

# control for type 1 error, bonferroni correction 0.05/3 comparisons, alpha = 0.017

# plot your results
ggplot(wdf, aes(x = time_mth, y = weight_kg, fill = diet)) +
  geom_bar(stat = "summary", fun.y = "mean", position = "dodge") +
  geom_errorbar(aes(ymin = mean_kg - sem_kg,
                    ymax = mean_kg + sem_kg), width = .2, size = 0.7,
                position = position_dodge(1)) +
  theme_bw() +
  theme(axis.title = element_text(face = "bold"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  ylab("Mean weight per month (kg)") +
  xlab("Time passed (months)")

