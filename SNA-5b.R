kasus = df2 %>%
  select(user_name, mention) %>%
  unnest_tokens(mention, mention, "words", to_lower = FALSE)

kasus$user_name = paste0("@", kasus$user_name)
kasus$mention = paste0("@", kasus$mention)

write_csv(kasus, "~/Documents/WORK/SADASA/Data Science/SADASA'21/UDINUS/SNA Wth R/SNA/kasus.csv")
glimpse(kasus)

# analisis konten
node_OG <- read_csv("~/Documents/WORK/SADASA/Data Science/SADASA'21/UDINUS/SNA Wth R/SNA/node_OG.csv")

node_OG$modularity_class = paste0("Kel. ", node_OG$modularity_class)

node_OG %>%
  count(modularity_class, sort = TRUE) %>%
  head(10) %>%
  ggplot(aes(x = modularity_class, y = n)) +
  geom_col() +
  coord_flip()

kelompok8 = node_OG %>%
  filter(modularity_class == "Kel. 8")

df1$user_name = paste0("@", df1$user_name)

konten_kel8 = df1 %>%
  filter(user_name %in% kelompok8$Label)
