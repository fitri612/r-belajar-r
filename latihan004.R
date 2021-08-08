#1. data mentah
#2. Pre-processing
    #1. memilih kolom yang akan dianalisis
    #2. melakukan manipulasi data
    #3. melakukan cleaning data
#3. EDA - Ekplorasi dengan tujuan
    #1. mengetahui tipe-tipe variable
    #2. mengetahui karakteristik spesifik dari data
    #3. variabel yang akan dianalisa
    #4. pertanyaan yang mungkin bisa dijawab oleh data yang kita miliki
#4. Analisis (SNA)
    #1. bisa menentukan nodes (actor/point/organisasi/etc.) -> siapa?
    #2. bisa menentukan edges (hubungan "abstrak" antara nodes) -> apa?

# kasus-1
# kolom 1 - pengirim: @jokowi
# kolom 2 - postingan: saya bersama pak @prabowo melakukan tinjauan pembangunan waduk di NTB

# nodes: username(teks yang didepannya diawali dengan tanda @)
# nodes yang dalam data: @jokowi dan @prabowo
# edges: pengirim dan yang dimention (mention)

# @jokowi -> @prabowo

library(tidyverse)

# data mentah
df_latihan1 = read_csv("mentah/data_latihan.csv")
glimpse(df_latihan1)

# pilih variabel yang cocok untuk dianalisis menggunakan SNA dari data_latihan.csv
# nodes = @username
# edges = mention
df_sna = df_latihan1 %>%
  select(user_screen_name, full_text)

# Konsep network
# nodes dan edges secara operasional kita harus tahu secara konseptual

# SNA = SOCIAL NETWORK ANALYSIS = ANALISIS HUBUNGAN SOSIAL
# HUBUNGAN apa?
# SIAPA/APA yang berhubungan?

token_hashtag = df_latihan1 %>%
  select(entities_hashtags)%>%
  tidytext::unnest_tokens(token_tag, entities_hashtags, token = "words", to_lower = TRUE, drop = FALSE)%>%
  count(token_tag, sort = TRUE)

# Centrality
## Degree Centrality
## Betweenness Centrality
## Closeness Centrality
## Eligenvector Centrality
## Page Rank Centrality

# Modularity