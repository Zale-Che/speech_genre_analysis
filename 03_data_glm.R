library(hqmisc)
library(tidyverse)
library(tidytext)
library(psych)
library(lme4)

#### data preparation ####
## add new f0 feature(f0_reset)
corpus_all <- read_tsv("data/corpus_final.tsv") %>%
  group_by(topic) %>%
  mutate(f0_reset = if_else(utter_id == 1, 0, abs(f0_start - f0_final %>% lag(n = 1L)))) %>%
  ungroup

## add speaker ID
corpus_all %>%
  mutate(speaker_ID = case_when(topic == "korean" ~ 1,
                                topic == "instant" ~ 2,
                                topic == "shopping" ~ 3,
                                topic == "ugly" ~ 4,
                                topic == "stupid" ~ 4,
                                topic == "love" ~ 5,
                                topic == "package" ~ 6,
                                topic == "spicy" ~ 7,
                                topic == "waa" ~ 8,
                                topic == "diet" ~ 9,
                                topic == "constipation" ~ 9,
                                topic == "animal" ~ 10,
                                topic == "pregnant" ~ 10,
                                topic == "russia" ~ 11,
                                topic == "scope1" ~ 12,
                                topic == "scope2" ~ 12,
                                topic == "sit" ~ 13,
                                topic == "oil" ~ 14, 
                                topic == "soul" ~ 15,
                                topic == "teaching" ~ 16)) %>%
  mutate(speaker_ID = as.factor(speaker_ID)) -> corpus_all

## count  the final numbers of utterances of the corpus
corpus_all %>%
  count(genre)
corpus_all %>%
  count(genre, speaker_gender)

## remove NAs
corpus_all %>%
  na.omit() -> corpus_all_noNA

## count final utterance numbers without NAs
corpus_all_noNA %>%
  count(genre)
corpus_all_noNA %>%
  count(genre, speaker_gender)



#### statistical analysis ####
library(MASS)
library(car)
library(ggfortify)
logi_data <- corpus_all_noNA[,c(1, 3, 6:10, 15:18)] %>%   
  rename(f0_mean = "f0_mean_acoustic",
         f0_range = "f0_range_acoustic") %>%
  rename(utterance_duration = "duration",
         duration_PVI = "dur_PVI",
         speech_rate = "rate",
         pause_duration = "pause_dur") %>%
  mutate(genre = as.factor(genre)) %>%
  mutate(speaker_gender = as.factor(speaker_gender))
  
## duration: log transformation
logi_data %>%
  mutate(utterance_duration = log10(utterance_duration),
         pause_duration = log10(pause_duration)) %>%
  dplyr::select(genre, speaker_ID, speaker_gender, tidyselect::everything()) -> logi_data

## check durational feature normality
hist(logi_data$utterance_duration)
qqnorm(logi_data$utterance_duration)
hist(logi_data$pause_duration)
qqnorm(logi_data$pause_duration)

## descriptive statistics
## mean
logi_data %>%
  dplyr::select(-speaker_gender, -speaker_ID) %>%
  group_by(genre) %>%
  summarise(across(everything(), list(mean))) -> logi_data_mean

## standard error
library(plotrix)
logi_data %>%
  dplyr::select(-speaker_gender, -speaker_ID) %>%
  group_by(genre) %>%
  summarise(across(everything(), list(std.error))) -> logi_data_se


##################################
########### Plotting #############
##################################
library(Rmisc)
## utterance_duration
logi_data %>%
  summarySE(measurevar="utterance_duration", groupvars=c("genre")) %>%
  ggplot(aes(x = genre, y = utterance_duration, color = genre)) + 
  geom_point(size = 4) +
  geom_errorbar(aes(ymin= utterance_duration-ci, ymax=utterance_duration+ci), 
                width = .1) +
  labs(x = "speech genre", y = "log-10(s)") +
  theme(legend.position="none",
        axis.text = element_text(size = 20),
        axis.title = element_text(size = 18))

## pause_duration
logi_data %>%
  summarySE(measurevar="pause_duration", groupvars=c("genre")) %>%
  ggplot(aes(x = genre, y = pause_duration, color = genre)) + 
  geom_point(size = 4) +
  geom_errorbar(aes(ymin= pause_duration-ci, ymax=pause_duration+ci), 
                width = .1) +
  labs(x = "speech genre", y = "log-10(s)") +
  theme(legend.position="none",
        axis.text = element_text(size = 20),
        axis.title = element_text(size = 18))

## duration_PVI
logi_data %>%
  summarySE(measurevar="duration_PVI", groupvars=c("genre")) %>%
  ggplot(aes(x = genre, y = duration_PVI, color = genre)) + 
  geom_point(size = 4) +
  geom_errorbar(aes(ymin= duration_PVI-ci, ymax=duration_PVI+ci), 
                width = .1) +
  labs(x = "speech genre", y = "") +
  theme(legend.position="none",
        axis.text = element_text(size = 20),
        axis.title = element_text(size = 18))

## speech_rate
logi_data %>%
  summarySE(measurevar="speech_rate", groupvars=c("genre")) %>%
  ggplot(aes(x = genre, y = speech_rate, color = genre)) + 
  geom_point(size = 4) +
  geom_errorbar(aes(ymin= speech_rate-ci, ymax=speech_rate+ci), 
                width = .1) +
  labs(x = "speech genre", y = "nsyll/s") +
  theme(legend.position="none",
        axis.text = element_text(size = 20),
        axis.title = element_text(size = 18))

## f0_mean
logi_data %>%
  summarySE(measurevar="f0_mean", groupvars=c("genre")) %>%
  ggplot(aes(x = genre, y = f0_mean, color = genre)) + 
  geom_point(size = 4) +
  geom_errorbar(aes(ymin= f0_mean-ci, ymax=f0_mean+ci), 
                width = .1) +
  labs(x = "speech genre", y = "semitones") +
  theme(legend.position="none",
        axis.text = element_text(size = 20),
        axis.title = element_text(size = 18))

## f0_reset
logi_data %>%
  summarySE(measurevar="f0_reset", groupvars=c("genre")) %>%
  ggplot(aes(x = genre, y = f0_reset, color = genre)) + 
  geom_point(size = 4) +
  geom_errorbar(aes(ymin= f0_reset-ci, ymax=f0_reset+ci), 
                width = .1) +
  labs(x = "speech genre", y = "semitones") +
  theme(legend.position="none",
        axis.text = element_text(size = 20),
        axis.title = element_text(size = 18))

## f0_range
logi_data %>%
  summarySE(measurevar="f0_range", groupvars=c("genre")) %>%
  ggplot(aes(x = genre, y = f0_range, color = genre)) + 
  geom_point(size = 4) +
  geom_errorbar(aes(ymin= f0_range-ci, ymax=f0_range+ci), 
                width = .1) +
  labs(x = "speech genre", y = "semitones") +
  theme(legend.position="none",
        axis.text = element_text(size = 20),
        axis.title = element_text(size = 18))

## f0_PVI
logi_data %>%
  summarySE(measurevar="f0_PVI", groupvars=c("genre")) %>%
  ggplot(aes(x = genre, y = f0_PVI, color = genre)) + 
  geom_point(size = 4) +
  geom_errorbar(aes(ymin= f0_PVI-ci, ymax=f0_PVI+ci), 
                width = .1) +
  labs(x = "speech genre", y = "") +
  theme(legend.position="none",
        axis.text = element_text(size = 20),
        axis.title = element_text(size = 18))

##################################
######## find  correlation #######
##################################
library(ggcorrplot)
# Compute a correlation matrix
logi_data_cor <- logi_data[,4:11]
corr <- cor(logi_data_cor)
#head(corr[, 1:6])

# Compute a matrix of correlation p-values
p.matrix <- cor_pmat(logi_data_cor)
#head(p.matrix[, 1:4])

## Plot correlation
# Types of correlogram layout
# --------------------------------
# Get the lower triangle
ggcorrplot(corr, hc.order = TRUE, type = "lower",
           outline.col = "white",
           lab = TRUE)


##################################
######### GLMM or not ############
##################################

## turn genre and gender into dummy variables
logi_data$genre <- ifelse(logi_data$genre=="informative", 1, 0) ## 1 = informative
logi_data$speaker_gender <- ifelse(logi_data$speaker_gender=="M", 1, 0)## 1 = male

# logistic regression 
glm.base <- glm(genre ~ utterance_duration  + duration_PVI + pause_duration + speech_rate + 
                  f0_mean + f0_range + f0_PVI + f0_reset,
                data = logi_data,
                family = binomial)
summary(glm.base)
vif(glm.base)

# GLMM with gender intercept
m.base <- glmer(genre ~ utterance_duration  + duration_PVI + pause_duration + speech_rate + 
                  f0_mean + f0_range + f0_PVI + f0_reset +  (1 | speaker_gender),
                 data = logi_data,
                 family = binomial)
summary(m.base)
vif(m.base)

## GLMM vs. logistic regression 
anova(m.base, glm.base, test="Chisq") ## 結果為不必考量gender effect


################################################################################################
#### Logistic Regression ####
################################################################################################
glm.base <- glm(genre ~ utterance_duration  + duration_PVI + pause_duration + speech_rate + 
                  f0_mean + f0_range + f0_PVI + f0_reset,
                data = logi_data,
                family = "binomial")
summary(glm.base)
vif(glm.base)

# stepAIC
stepAIC(glm.base, direction ="backward")

# remove f0_reset
glm.best <- glm(genre ~ utterance_duration  + pause_duration + duration_PVI + speech_rate + 
                  f0_mean + f0_range + f0_PVI,
                data = logi_data,
                family = "binomial")
summary(glm.best)
vif(glm.best)


# check model performance
anova(glm.best, glm.base, test="Chisq")

# effect plots
library(effects)
plot(allEffects(glm.base))
plot(allEffects(glm.base), selection=8, main = "Effect plot of f0-reset", xlab="reset", ylab="predicted probability")
plot(allEffects(glm.best))
plot(allEffects(glm.best), selection=1, main = "Effect plot of utterance duration", xlab="utterance duration", ylab="predicted probability")
plot(allEffects(glm.best), selection=2, main = "Effect plot of pause duration", xlab="pause duration", ylab="predicted probability")
plot(allEffects(glm.best), selection=3, main = "Effect plot of dur-PVI", xlab="dur-PVI", ylab="predicted probability")
plot(allEffects(glm.best), selection=4, main = "Effect plot of speech rate", xlab="speech rate", ylab="predicted probability")
plot(allEffects(glm.best), selection=5, main = "Effect plot of f0 mean", xlab="f0 mean", ylab="predicted probability")
plot(allEffects(glm.best), selection=6, main = "Effect plot f0 range", xlab="f0 range", ylab="predicted probability")
plot(allEffects(glm.best), selection=7, main = "Effect plot of f0-PVI", xlab="f0-PVI", ylab="predicted probability")

# install.packages("DescTools") # for C-statistic
library(Hmisc)
library(DescTools)
# 1st way
Cstat(glm.best) # C-statistic
# 2nd way
probs = 1/(1+exp(-fitted(glm.best)))
somers2(probs, as.numeric(logi_data$genre)) # C-statistic

# odds ratio
odds <- data.frame(feature = exp(coef(glm.best)) %>% names,
                   odds = exp(coef(glm.best)) %>% unname() %>% round(2),   
                   exp(confint.default(glm.best)) %>% unname()) %>% # Confidence Interval
  dplyr::rename(low_CI = "X1",
         high_CI = "X2")
odds %>%
  filter(feature != '(Intercept)') -> odds

# Plotting odds ratio
ggplot(odds, aes(x = odds, y = feature)) + 
  geom_vline(aes(xintercept = 1), size = .25, linetype = "dashed", color = "red") + 
  geom_errorbarh(aes(xmax = high_CI, xmin = low_CI), size = .5, 
                 height = 0, color = "gray50") +
  geom_point(size = 2, color = "orange") +
  #theme_bw() +
  ylab("") +
  xlab("Odds ratios") +
  geom_text(aes(label = odds), nudge_y = 0.2, colour = "gray25", size = 3)



