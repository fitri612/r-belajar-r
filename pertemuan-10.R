library(tidyverse)

library(tidytext)

#df1 = read_csv("https://raw.githubusercontent.com/eppofahmi/endgraf-academic-ubj/master/data/raw.csv

     #token=AHIOS7ITMVABHTH3PWVXL7TBEXIFU")
df1 = read_csv("C:/Users/ASUS/Downloads/kmmi-r/data/mentah/data_latihan.csv")
glimpse(df1)









#1
df1 = df1 %>%
  mutate(id_baris = row_number(1:4315))
glimpse(df1)

df2a <- df1 %>%
  select(id_baris, created_at, full_text, user_screen_name, favorite_count, reply_count, retweet_count)

df2b = df2a %>%
  mutate(engagement = favorite_count + reply_count + retweet_count)
glimpse(df2b)


df3 = df1 %>%
  select(created_at, favorite_count, reply_count, retweet_count) %>%
  separate(col = created_at, into = c("tanggal", "jam"), sep = " ") %>%
  group_by(tanggal) %>%
  summarise(total_fav = sum(favorite_count),
            total_rep = sum(reply_count),
            total_ret = sum(retweet_count))
df3$tanggal = as.Date(df3$tanggal)
glimpse(df3)


# mengubah data wide ke long
df3 = df3 %>%
  pivot_longer(cols = starts_with("total"))

df3 %>%
  ggplot(aes(x = tanggal, y = value, group = name, colour = name))+
  geom_line() +
  scale_color_manual(values = c("red", "green", "blue"))

#memperjelas visualisasi data yang dihasilkan
df3 %>%
  ggplot(aes(x = tanggal, y = value, group = name)) +
  geom_line() +
  facet_wrap(vars(name), scales = "free_y", nrow = 2, strip.position = "top")

##plot perbandingan
ret = sum(df2b$retweet_count)
fav = sum(df2b$favorite_count)
rep = sum(df2b$reply_count)

df4 = tibble(var = c("ret", "fav", "rep"), value = c(ret, fav, rep))
df4
df4 %>%
  ggplot(aes(x = var, value)) + 
  geom_col()

#plot hubungan
df5 = economics
#hubungan biasanya di representasikan menggunakan scatter plot
# teori umum : menganggur tidak bisa menabung
df5 %>%
  ggplot(aes(x = unemploy, y = psavert)) +
  geom_point() +
  geom_smooth()

