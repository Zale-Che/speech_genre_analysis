library(tidyverse)
library(tidytext)
library(quanteda)
library(stringr)
library(readtext)

setwd("/Users/chesinjhang/Desktop/speech_genre_analysis")
#### informative data ####
## Get the filenames from the directory
inform_wd <- dir("data/03_extract_f0dB_data/informative/word", 
                 full.names = TRUE)
inform_dB <- dir("data/03_extract_f0dB_data/informative/dB", 
                 full.names = TRUE)
inform_f0 <- dir("data/03_extract_f0dB_data/informative/f0", 
                 full.names = TRUE)

## Holder
inform_wd_lst <- list()
inform_dB_lst <- list()
inform_f0_lst <- list()

## Traverse each file
for (i in 1:length(inform_wd)) {
  inform_wd_lst[[i]] <- read_tsv(inform_wd[i]) %>%
    rename(start = "tmin",
           end = "tmax") %>%
    mutate(syllable_dur = end-start) %>%
    mutate(word_No = row_number()) %>%
    select(word_No, text, start, end, syllable_dur) %>%
    mutate(topic = inform_wd[i]) %>%
    mutate(topic = str_replace_all(topic, "_wd.tsv", "")) %>%
    mutate(topic = str_replace_all(topic, "data.+/", ""))
}
for (i in 1:length(inform_dB)) {
  inform_dB_lst[[i]] <- read_tsv(inform_dB[i]) %>%
    select(-rowLabel) %>%
    rename(id = "row",
           time = `Time (s)`,
           intensity = `Intensity (dB)`) %>%
    mutate(topic = inform_dB[i]) %>%
    mutate(topic = str_replace_all(topic, "_dB.tsv", "")) %>%
    mutate(topic = str_replace_all(topic, "data.+/", ""))
}
for (i in 1:length(inform_f0)) {
  inform_f0_lst[[i]] <- read_tsv(inform_f0[i]) %>%
    select(-rowLabel) %>%
    rename(id = "row",
           time2 = "Time",
           f0 = "F0") %>%
    mutate(topic = inform_f0[i]) %>%
    mutate(topic = str_replace_all(topic, "_f0.tsv", "")) %>%
    mutate(topic = str_replace_all(topic, "data.+/", ""))
}


## Holder
inform_max_dB <- list()
inform_max_dB_f0 <- list()
inform_max_dB_f02 <- list()
inform_syl_f0 <- list()
for (i in 1:length(inform_wd_lst)) {
# 以時間組合文字及intensity table
left_join(inform_dB_lst[[i]], inform_wd_lst[[i]], join_by(topic==topic, between(time, start, end))) %>%
  select(-id) %>%
  na.omit() %>%
  select(start, end, time, intensity, text, word_No, syllable_dur, topic) %>%
  group_by(word_No) %>%
  filter(intensity == max(intensity)) %>%
  rename(maxdB_time = time) %>%
  ungroup -> inform_max_dB[[i]]

# 找出最大值intensity的左邊界最接近f0
left_join(inform_max_dB[[i]], inform_f0_lst[[i]], join_by(topic==topic, closest(maxdB_time >= time2))) %>%
  select(-id) %>%
  na.omit() %>%
  mutate(t.dist = maxdB_time-time2) -> inform_max_dB_f0[[i]]

# 找出最大值intensity的右邊界最接近f0
left_join(inform_max_dB[[i]], inform_f0_lst[[i]], join_by(topic==topic, closest(maxdB_time <= time2))) %>%
  select(-id) %>%
  na.omit() %>%
  mutate(t.dist = time2-maxdB_time) -> inform_max_dB_f02[[i]]

# 決定最後最接近的f0
inform_syl_f0[[i]] <- full_join(inform_max_dB_f02[[i]], inform_max_dB_f0[[i]], by = c("maxdB_time", "intensity", "text", "word_No", "topic", "syllable_dur")) %>%
                    select(-time2.x, -time2.y) %>%
                    mutate(f0 = case_when(t.dist.x < t.dist.y ~ f0.x,
                                          t.dist.x > t.dist.y ~ f0.y,
                                          t.dist.x == t.dist.y ~ (f0.x+f0.y)/2,
                                          is.na(t.dist.y) == T ~ f0.x,
                                          is.na(t.dist.x) == T ~ f0.y)) %>%
                    mutate(start_time= case_when(is.na(start.y) == T ~ start.x,
                                            is.na(start.x) == T ~ start.y,
                                            is.na(start.x) == F ~ start.x,
                                            is.na(start.y) == F ~ start.y),
                           end_time = case_when(is.na(end.y) == T ~ end.x,
                                           is.na(end.x) == T ~ end.y,
                                           is.na(end.x) == F ~ end.x,
                                           is.na(end.y) == F ~ end.y)) %>%
                    select(word_No, text, start_time, end_time, syllable_dur, f0, topic)
}

inform_syl_f0_df <- bind_rows(inform_syl_f0)

##################################################################################################################################
#### entertaining data ####
## Get the filenames from the directory
enter_wd <- dir("data/03_extract_f0dB_data/entertaining/word", 
                full.names = TRUE)
enter_dB <- dir("data/03_extract_f0dB_data/entertaining/dB", 
                full.names = TRUE)
enter_f0 <- dir("data/03_extract_f0dB_data/entertaining/f0", 
                full.names = TRUE)

## Holder
enter_wd_lst <- list()
enter_dB_lst <- list()
enter_f0_lst <- list()

## Traverse each file
for (i in 1:length(enter_wd)) {
  enter_wd_lst[[i]] <- read_tsv(enter_wd[i]) %>%
    rename(start = "tmin",
           end = "tmax") %>%
    mutate(syllable_dur = end-start) %>%
    mutate(word_No = row_number()) %>%
    select(word_No, text, start, end, syllable_dur) %>%
    mutate(topic = enter_wd[i]) %>%
    mutate(topic = str_replace_all(topic, "_wd.tsv", "")) %>%
    mutate(topic = str_replace_all(topic, "data.+/", ""))
}
for (i in 1:length(enter_dB)) {
  enter_dB_lst[[i]] <- read_tsv(enter_dB[i]) %>%
    select(-rowLabel) %>%
    rename(id = "row",
           time = `Time (s)`,
           intensity = `Intensity (dB)`) %>%
    mutate(topic = enter_dB[i]) %>%
    mutate(topic = str_replace_all(topic, "_dB.tsv", "")) %>%
    mutate(topic = str_replace_all(topic, "data.+/", ""))
}
for (i in 1:length(enter_f0)) {
  enter_f0_lst[[i]] <- read_tsv(enter_f0[i]) %>%
    select(-rowLabel) %>%
    rename(id = "row",
           time2 = "Time",
           f0 = "F0") %>%
    mutate(topic = enter_f0[i]) %>%
    mutate(topic = str_replace_all(topic, "_f0.tsv", "")) %>%
    mutate(topic = str_replace_all(topic, "data.+/", ""))
}


## Holder
enter_max_dB <- list()
enter_max_dB_f0 <- list()
enter_max_dB_f02 <- list()
enter_syl_f0 <- list()
for (i in 1:length(enter_wd_lst)) {
  # 以時間組合文字及intensity table
  left_join(enter_dB_lst[[i]], enter_wd_lst[[i]], join_by(topic==topic, between(time, start, end))) %>%
    select(-id) %>%
    na.omit() %>%
    select(start, end, time, intensity, text, word_No, syllable_dur, topic) %>%
    group_by(word_No) %>%
    filter(intensity == max(intensity)) %>%
    rename(maxdB_time = time) %>%
    ungroup -> enter_max_dB[[i]]
  
  # 找出最大值intensity的左邊界最接近f0
  left_join(enter_max_dB[[i]], enter_f0_lst[[i]], join_by(topic==topic, closest(maxdB_time >= time2))) %>%
    select(-id) %>%
    na.omit() %>%
    mutate(t.dist = maxdB_time-time2) -> enter_max_dB_f0[[i]]
  
  # 找出最大值intensity的右邊界最接近f0
  left_join(enter_max_dB[[i]], enter_f0_lst[[i]], join_by(topic==topic, closest(maxdB_time <= time2))) %>%
    select(-id) %>%
    na.omit() %>%
    mutate(t.dist = time2-maxdB_time) -> enter_max_dB_f02[[i]]
  
  # 決定最後最接近的f0
  enter_syl_f0[[i]] <- full_join(enter_max_dB_f02[[i]], enter_max_dB_f0[[i]], by = c("maxdB_time", "intensity", "text", "word_No", "topic", "syllable_dur")) %>%
                      select(-time2.x, -time2.y) %>%
                      mutate(f0 = case_when(t.dist.x < t.dist.y ~ f0.x,
                                            t.dist.x > t.dist.y ~ f0.y,
                                            t.dist.x == t.dist.y ~ (f0.x+f0.y)/2,
                                            is.na(t.dist.y) == T ~ f0.x,
                                            is.na(t.dist.x) == T ~ f0.y)) %>%
                      mutate(start_time= case_when(is.na(start.y) == T ~ start.x,
                                                   is.na(start.x) == T ~ start.y,
                                                   is.na(start.x) == F ~ start.x,
                                                   is.na(start.y) == F ~ start.y),
                             end_time = case_when(is.na(end.y) == T ~ end.x,
                                                  is.na(end.x) == T ~ end.y,
                                                  is.na(end.x) == F ~ end.x,
                                                  is.na(end.y) == F ~ end.y)) %>%
                      select(word_No, text, start_time, end_time, syllable_dur, f0, topic)
}

enter_syl_f0_df <- bind_rows(enter_syl_f0)


##################################################################################################################################
all_syll <- bind_rows(inform_syl_f0_df, enter_syl_f0_df) %>%
  rename(syllable = "text",
         syllable_id = "word_No")
#write_tsv(all_syll, "data/corpus_syll_f0dB_v2.tsv")

