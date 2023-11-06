###############################################################
###################### data preparation #######################
###############################################################
library(tidyverse)
library(tidytext)
library(quanteda)
library(stringr)
library(readtext)

setwd("/Users/chesinjhang/Desktop/speech_genre_analysis")

## read text-time data  ####
entertain_data <- read_tsv("data/01_text_time_data/entertain_data.tsv") %>%
  mutate_if(is.double, round, 4)
informative_data <- read_tsv("data/01_text_time_data/informative_data.tsv") %>%
  mutate_if(is.double, round, 4)
entertain_data2 <- read_tsv("data/01_text_time_data/entertain_data2.tsv") %>%
  mutate_if(is.double, round, 4)
informative_data2 <- read_tsv("data/01_text_time_data/informative_data2.tsv") %>%
  mutate_if(is.double, round, 4)

## combine text-time data of the same genre
entertain_data <- bind_rows(entertain_data, entertain_data2)
informative_data <- bind_rows(informative_data, informative_data2)

#### f0 data  ####
entertain_f0_data <- read_tsv("data/02_utt_f0_data/entertain_f0_data.tsv") %>%
  mutate_if(is.double, round, 4)
informative_f0_data <- read_tsv("data/02_utt_f0_data/informative_f0_data.tsv") %>%
  mutate_if(is.double, round, 4)
entertain_f0_data2 <- read_tsv("data/02_utt_f0_data/entertain_f0_data2.tsv") %>%
  mutate_if(is.double, round, 4)
informative_f0_data2 <- read_tsv("data/02_utt_f0_data/informative_f0_data2.tsv") %>%
  mutate_if(is.double, round, 4)

## combine f0 data of the same genre
entertain_f0_data <- bind_rows(entertain_f0_data, entertain_f0_data2)
informative_f0_data <- bind_rows(informative_f0_data, informative_f0_data2)

##join text-time and f0 data
left_join(entertain_data, entertain_f0_data, by = c("topic", "text" )) %>%
  select(topic, speaker_gender, text, syllable, duration, everything()) -> entertain_data_complete

left_join(informative_data, informative_f0_data, by = c("topic", "text" ))%>%
  select(topic, speaker_gender, text, syllable, duration, everything()) -> informative_data_complete


## compute pause duration
informative_data_complete %>%
  group_by(topic) %>%
  mutate(pause_dur = lead(start) - end) %>%
  ungroup -> informative_data_complete
entertain_data_complete %>%
  group_by(topic) %>%
  mutate(pause_dur = lead(start) - end) %>%
  ungroup -> entertain_data_complete


## check unwanted utterance (denoted by x in txet)
informative_data_complete %>%
  group_by(topic) %>%
  filter(str_detect(text, "[A-Za-z]+") == T) %>%
  ungroup
entertain_data_complete %>%
  group_by(topic) %>%
  filter(str_detect(text, "[A-Za-z]+") == T) %>%
  ungroup

## remove unwanted utterance (denoted by x in txet)
informative_data_complete %>%
  group_by(topic) %>%
  filter(str_detect(text, "[A-Za-z]+") == F) %>%
  ungroup -> informative_data_complete
entertain_data_complete %>%
  group_by(topic) %>%
  filter(str_detect(text, "[A-Za-z]+") == F) %>%
  ungroup -> entertain_data_complete


## 檢查重複的utterance (text中重複者有數字)
informative_data_complete %>%
  filter(str_detect(text, "[0-9]+") == T) -> check_inform

entertain_data_complete %>%
  filter(str_detect(text, "[0-9]+") == T)  -> check_enter

## 復原重複的utterance (text中重複者有數字)
informative_data_complete %>%
  mutate(text = str_replace_all(text, "[0-9]+", "")) -> informative_data_complete

entertain_data_complete %>%
  mutate(text = str_replace_all(text, "[0-9]+", "")) -> entertain_data_complete




###############################################################
######################## create corpus ########################
###############################################################
library(hqmisc)
library(tidyverse)
library(tidytext)
library(stringr)


#### import data  ####
entertaining <- entertain_data_complete %>%
  select(-f0_sd_acoustic) %>%
  mutate(genre = "entertaining")
informative <- informative_data_complete %>%
  select(-f0_sd_acoustic) %>%
  mutate(genre = "informative")

# combine two genres of data into a big dataframe
all_data <- rbind(entertaining, informative) %>% 
  mutate(genre = as.factor(genre)) %>%
  mutate(syllable = ifelse(is.na(text)==FALSE, nchar(text), NA)) %>% ##重新算原本錯誤的音節數目
  mutate(rate = syllable/duration) %>%
  select(-syllable) %>% 
  select(genre, topic, speaker_gender, text, start, end, duration, pause_dur, rate, everything())
  
# semitone transformation
all_data %>% 
  group_by(topic) %>%
  summarize(video_f0_mean = mean(f0_mean_acoustic, na.rm = TRUE)) %>% # compute video f0 mean
  ungroup %>%
  right_join(all_data, by = "topic") %>%
  #group_by(topic) %>%
  mutate_at(c("f0_mean_acoustic",
              "f0_max_acoustic",
              "f0_min_acoustic",
              "f0_start",
              "f0_final" ), ~(f2st(., base = video_f0_mean) %>% as.numeric)) %>% ## semitone transformation: video_f0_mean based
  #ungroup %>%
  #select(-video_f0_mean) %>%
  mutate(f0_range_acoustic = f0_max_acoustic - f0_min_acoustic) %>%  # add f0 range
  group_by(topic) %>%
  mutate(utter_id = row_number()) %>%
  ungroup -> scaled_all_data 


############ add PVI information ############
## text to character-based
scaled_all_data %>% 
  mutate(text = strsplit(as.character(text), "")) %>%
  unnest(text) %>%
  group_by(topic) %>%
  mutate(syllable_id = row_number()) %>%
  ungroup %>%
  select(-start, -end) -> scaled_all_char


## import syllable f0 information
all_syll <- read_tsv("data/04_corpus/corpus_syll_f0dB.tsv") %>% 
  mutate(f0 = f0 %>% as.numeric,
         syllable_dur = syllable_dur %>% as.numeric)


## join syllable information and prepare statistics for PVI
# duration-PVI preparation
scaled_all_char %>%
  left_join(all_syll, by = c("topic", "syllable_id")) %>%
  #mutate(syl_f0_mean = syl_f0_mean %>% as.numeric) %>%
  #mutate_at("syl_f0_mean", ~(f2st(., base = video_f0_mean) %>% as.numeric)) %>%   ## speaker based
  #mutate_at("syl_f0_mean", ~(f2st(., base = 50) %>% as.numeric)) %>%  ## speaker base 50 Hz based
  select(-video_f0_mean) %>%
  group_by(topic, utter_id) %>%
  mutate(dur_diff = abs(syllable_dur - lag(syllable_dur)),    ## duration PVI numerator
         dur_down = abs((syllable_dur + lag(syllable_dur))/2), ## duration PVI denominator
         dur_fra = dur_diff/dur_down) %>%    ## duration PVI fraction
  ungroup -> scaled_all_char_dur

# f0-PVI preparation
scaled_all_char %>%
  left_join(all_syll, by = c("topic", "syllable_id")) %>%
  #mutate(syl_f0_mean = syl_f0_mean %>% as.numeric) %>%
  #mutate_at("syl_f0_mean", ~(f2st(., base = video_f0_mean) %>% as.numeric)) %>%   ## speaker based
  #mutate_at("syl_f0_mean", ~(f2st(., base = 50) %>% as.numeric)) %>%  ## speaker base 50 Hz based
  select(-video_f0_mean) %>%
  group_by(topic, utter_id) %>%
  mutate(no_NA = sum(is.na(f0))) %>% #計算每個句子音節缺少數量
  ungroup %>%
  filter(no_NA <= 2) %>%  #如果句子中少於(包含)兩個音節以內沒有值仍可計算pvi
  filter(is.na(f0) == F) %>%
  group_by(topic, utter_id) %>%
  mutate(f0_diff = abs(f0 - lag(f0)),     ## f0 PVI numerator
         f0_down = abs((f0 + lag(f0))/2),  ## f0 PVI denominator
         f0_fra = f0_diff/f0_down) %>%  ## f0 PVI fraction
  ungroup -> scaled_all_char_f0


############  PVI computation ############
## computing duration PVI
scaled_all_char_dur %>%
  group_by(topic, utter_id) %>%
  summarize(dur_PVI = 100*(sum(dur_fra, na.rm = TRUE)/(n()-1))) %>% ## final duration PVI computation
  ungroup -> dur_PVI

## computing f0 PVI
scaled_all_char_f0 %>%
  group_by(topic, utter_id) %>%
  mutate(n_1 = sum(!is.na(f0_diff))) %>% # n-1分母部份
  summarize(f0_PVI= 100*(sum(f0_fra, na.rm = TRUE)/n_1)) %>%  ## final f0 PVI computation
  ungroup %>%
  distinct_all()-> f0_PVI

## join two PVIs back to utterance data
scaled_all_data %>%
  left_join(dur_PVI, by = c("topic", "utter_id")) %>%
  left_join(f0_PVI, by = c("topic", "utter_id")) %>%
  select(-start, -end, -video_f0_mean) %>%
  select(genre, topic, speaker_gender, utter_id, text, duration, pause_dur, dur_PVI, rate, everything()) -> corpus_final

## check single word utterance 
corpus_final %>%
  filter(nchar(text) == 1) # 50 in total

## remove single word utterance
corpus_final %>%
  filter(nchar(text) != 1) %>%
  group_by(topic) %>%
  mutate(utter_id = row_number()) %>%
  ungroup-> corpus_final

#corpus_final %>%
#  write_tsv("data/corpus_final.tsv")



