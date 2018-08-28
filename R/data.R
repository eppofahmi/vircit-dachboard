library(tidyverse)
library(textclean)
library(ggplot2)

# data ----
tabelOutput <- read.csv("https://raw.githubusercontent.com/eppofahmi/vircit-dachboard/master/data/tabelOutput.csv", stringsAsFactors = FALSE, header = TRUE, sep = ",")

tabelOutput$parameter <- gsub("\\bJangan kembali renggut kebebasan masyarakat jawa barat untuk memilih transportasi!\\b", "memilih transportasi",
                              tabelOutput$parameter)
tabelOutput$parameter <- gsub("\\bDukung keberadaan gojek di Purwokerto\\b", "Gojek di Purwokerto",
                              tabelOutput$parameter)
tabelOutput$parameter <- gsub("\\blayanan transportasi berbasis online\\b", "",
                              tabelOutput$parameter)

# cleaning ----
clean_text <- tweet_cleaner(tabelOutput$teks)
tabelOutput$teks_clean <- clean_text$clean_text

# removing balnk rows
tabelOutput <- tabelOutput[!(is.na(tabelOutput$teks_clean) | tabelOutput$teks_clean==""), ]
  
# write csv ----
write_csv(tabelOutput, path = "//Volumes/mydata/rSkilss/vircit-dachboard/data/tabelOutput.csv")

# data analysis ----
tm_petisi_change <- read.csv(paste0(getwd(), "/data/tm_petisi_change.csv"), 
                             stringsAsFactors = FALSE)
tm_petisi_change <- tm_petisi_change %>%
  mutate(sumber_data = "petisi", 
         kasus = "change")

tm_petisi_gojek <- read.csv(paste0(getwd(), "/data/tm_petisi_gojek.csv"), 
                             stringsAsFactors = FALSE)
tm_petisi_gojek <- tm_petisi_gojek %>%
  mutate(sumber_data = "petisi_gojek", 
         kasus = "gojek")

tm_playstore_customer <- read.csv(paste0(getwd(), "/data/tm_playstore_customer.csv"), 
                            stringsAsFactors = FALSE)
tm_playstore_customer <- tm_playstore_customer %>%
  mutate(sumber_data = "playsotore_customer", 
         kasus = "gojek")

tm_playstore_driver <- read.csv(paste0(getwd(), "/data/tm_playstore_driver.csv"), 
                            stringsAsFactors = FALSE)
tm_playstore_driver <- tm_playstore_driver %>%
  mutate(sumber_data = "playsotore_driver", 
         kasus = "gojek")

tm_twit_change_btr <- read.csv(paste0(getwd(), "/data/tm_twit_change_btr.csv"), 
                            stringsAsFactors = FALSE)
tm_twit_change_btr <- tm_twit_change_btr %>%
  mutate(sumber_data = "twitter", 
         kasus = "change")

tm_twit_gojek <- read.csv(paste0(getwd(), "/data/tm_twit_gojek.csv"), 
                            stringsAsFactors = FALSE)
tm_twit_gojek <- tm_twit_gojek %>%
  mutate(sumber_data = "twitter", 
         kasus = "gojek")

analisis_data <- bind_rows(tm_petisi_change, tm_petisi_gojek, tm_playstore_customer, 
                           tm_playstore_driver, tm_twit_change_btr, tm_twit_gojek)

class(analisis_data)

write_csv(analisis_data, path = "//Volumes/mydata/rSkilss/vircit-dachboard/data/analisis_data.csv")

# tes visualisasi ----
analisis_data %>%
  filter(kasus == "change") %>%                # input 1 - kasus
  filter(sumber_data == "petisi") %>%          # input 2 - sumber data
  select(topic, term, beta) %>%
  filter(topic == 5) %>%                       # input 3 - nomor topik
  group_by(topic) %>%
  top_n(5) %>%                                 # input 4 - jumlah term/bigram
  ggplot(aes(reorder(term, beta), beta, fill = as.factor(topic))) + 
  geom_col(show.legend = FALSE) + 
  coord_flip() + facet_wrap(~topic, scales = "free") + 
  labs(x = NULL)


daftarTable <- tabelOutput %>%
  select(sumber_data, kasus) %>%
  group_by(sumber_data, kasus) %>%
  count(sumber_data) %>%
  select(sumber_data, kasus)


sumberData <- tabelOutput %>%
  select(sumber_data, kasus) %>%
  group_by(kasus) %>%
  count(sumber_data)
