# speech_genre_analysis
說明：

1. textgrid中標註為ｘ之區段為欲捨棄之utterance，原因主要包含：內容與主題無關、唱歌、語句非普通話、語句背景聲過大無法有效去除。

2. scope1 textgrid檔案有一大段標註為x，是因為當初音檔剪輯有誤，造成該區段有缺失語句，因此該區段跳過。該區段實際正確音檔及textgrid於scope2音檔及scope2 textgrid補上。

3. 以02_extract_utt_prosody.praat檔案抽取f0時，執行前需先調整到該影片之pitch q1/q3值，每個影片q1/q3值於f0_q1q3.xlsx檔案，為手動點擊praat取得。

4. 抽取後的utterance-based f0檔案，重複的utterance會有編號，而text_time_data中utterances的編號與之相應，為事先由手動標記。

5. 03_extract_f0dB_data資料夾中為以各影片為單位，以praat手動輸出每個時間點之dB/f0/word。

6. corpus_final.tsv為仍然允許 NA 於utterance中之版本。

7. 06_word_list.R產出檔無pos tags，若需pos tags需繼續執行word_pos.ipynb以CKIP tagger完成。

8. 每個音檔的原始影片資訊記錄於 video_infor.tsv

###############

產出詞彙PVI(corpus_word_list.tsv)步驟：

1.準備data：corpus_syll_f0dB.tsv 以及 corpus_word_tokens.tsv。

2.執行04_word_list.R產出word_lst_final輸出。

3.如需pos tag，將產出檔以word_pos.ipynb繼續執行後另存。


* 如需以semintone計算PVI：

4. 讀入corpus_syll_f0dB.tsv檔時新增 %>% mutate_at("f0", ~(f2st(., base = 自行修改 ) %>% as.numeric))

5. 正常執行04_word_list.R。


* 如需修正f0擷取設定：

6. 原corpus_syll_f0dB.tsv，無法使用。

7. 03_extract_f0dB_data資料夾中，記錄各影片時間單位之f0檔案需全部更新。

8. 更新各影片之f0檔案後，執行01_get_syll_f0.R 產出更新版corpus_syll_f0dB.tsv。

9. 回到步驟1產出corpus_word_list.tsv。
