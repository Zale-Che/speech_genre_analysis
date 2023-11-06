library(hqmisc)
library(tidyverse)
library(tidytext)
library(psych)
library(lme4)
library(dplyr) 


## read file
syll <- read_tsv("06_corpus/corpus_syll_f0dB_v2.tsv")
word <- read_tsv("06_corpus/corpus_word_tokens_v2.tsv") %>%
  group_by(topic) %>%
  mutate(word_id = row_number()) %>%
  ungroup

## restructure word
word %>%
  mutate(nch=nchar(text)) %>%
  slice(rep(1:n(), nch)) %>%
  select(-nch) %>%
  group_by(topic) %>%
  mutate(syllable_id = row_number()) %>%
  ungroup %>%
  mutate(speaker_id = case_when(topic == "korean" ~ 1,
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
  mutate(speaker_id = as.factor(speaker_id)) -> word

## combine word and syll dataframes
word_lst <- left_join(word, syll, by = c("topic", "syllable_id")) %>%
  mutate(topic = case_when(topic == "korean" ~ "korean_food",
                           topic == "instant" ~ "instant_noodles",
                           topic == "shopping" ~ "shopping_haul",
                           topic == "ugly" ~ "ugly",
                           topic == "stupid" ~ "stupid",
                           topic == "love" ~ "true_love",
                           topic == "package" ~ "packaging",
                           topic == "spicy" ~ "spicy_noodles",
                           topic == "waa" ~ "convenience_store",
                           topic == "diet" ~ "diet",
                           topic == "constipation" ~ "constipation",
                           topic == "animal" ~ "animal",
                           topic == "pregnant" ~ "pregnancy",
                           topic == "russia" ~ "russia",
                           topic == "scope1" ~ "scope1",
                           topic == "scope2" ~ "scope2",
                           topic == "sit" ~ "sit",
                           topic == "oil" ~ "oil", 
                           topic == "soul" ~ "soul",
                           topic == "teaching" ~ "online_teaching")) %>%
  select(genre, topic, text, word_id, utter_id, speaker_id, speaker_gender, everything())

# prepare duration PVI
word_lst %>%
  group_by(topic, word_id) %>%
  mutate(dur_diff = abs(syllable_dur - lag(syllable_dur)),    ## duration PVI numerator
         dur_down = abs((syllable_dur + lag(syllable_dur))/2), ## duration PVI denominator
         dur_fra = dur_diff/dur_down) %>%    ## duration PVI fraction
  ungroup -> word_lst_dur


# prepare f0 PVI
word_lst %>%
  group_by(topic, word_id) %>%
  mutate(f0_diff = abs(f0 - lag(f0)),     ## f0 PVI numerator
         f0_down = abs((f0 + lag(f0))/2),  ## f0 PVI denominator
         f0_fra = f0_diff/f0_down) %>%  ## f0 PVI fraction
  ungroup -> word_lst_f0


## computing duration PVI
word_lst_dur %>%
  group_by(topic, word_id) %>%
  summarize(dur_PVI = 100*(sum(dur_fra, na.rm = TRUE)/(n()-1))) %>% ## final duration PVI computation
  ungroup -> dur_PVI


## computing f0 PVI
word_lst_f0 %>%
  group_by(topic, word_id) %>%
  mutate(n_1 = sum(!is.na(f0_diff))) %>% # n-1分母部份
  summarize(f0_PVI= 100*(sum(f0_fra, na.rm = TRUE)/n_1)) %>%  ## final f0 PVI computation
  ungroup %>%
  distinct_all() -> f0_PVI

## join PVI information
word_lst %>%
  select(genre, topic, text, word_id, utter_id, start_time, end_time, speaker_id, speaker_gender) %>%
  distinct_at(c("word_id","topic"), .keep_all = T) %>% ## keep the first row to get start_time
  left_join(dur_PVI, by = c("topic", "word_id")) %>%
  left_join(f0_PVI, by = c("topic", "word_id")) %>%
  mutate_at(c("dur_PVI", "f0_PVI"), ~ifelse(is.na(.), NA, .)) -> word_lst_start_time_correct

## join PVI information
word_lst %>%
  select(genre, topic, text, word_id, utter_id, start_time, end_time, speaker_id, speaker_gender) %>%
  group_by(word_id,topic) %>%
  filter(row_number()==n()) %>% ## ## keep the last row to get end_time
  ungroup %>%
  left_join(dur_PVI, by = c("topic", "word_id")) %>%
  left_join(f0_PVI, by = c("topic", "word_id")) %>%
  mutate_at(c("dur_PVI", "f0_PVI"), ~ifelse(is.na(.), NA, .)) -> word_lst_end_time_correct

## correct the wrong end_time in word_lst_start_time_correct
word_lst_start_time_correct %>%
  mutate(end_time = word_lst_end_time_correct['end_time'] %>% unlist()) %>%
  select(genre, topic, text, utter_id, word_id, start_time, end_time, dur_PVI, f0_PVI, speaker_id, speaker_gender) -> word_lst_final

#word_lst_final %>%
#  write_tsv("data/corpus_word_list.tsv")

