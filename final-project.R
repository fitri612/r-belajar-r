library(tidyverse)
library(tidytext)
library(igraph)
library(ggraph)
library(rgexf)

#import data(read_)
data_covid <- read_csv(file = "C:/Users/ASUS/Downloads/kmmi-r/data/mentah/covid19_tweet.csv")
View(data_covid)

#mendefinisikan nodes dan edges
#nodes adalah username hashtag
#edges adalah mention

#pilih data or kolom(select)
df_covid_net = data_covid %>%
  select(screen_name, text)

#extract
regex <- "(^|[^#\\w])#(\\w{1,15})\\b"

hashtag1 <- str_extract_all(string = df_covid_net$text, 
                            pattern = "(#[[:alnum:]_]*)", 
                            simplify = TRUE)

hashtag2 <- str_extract_all(string = df_covid_net$text, 
                            pattern = regex, 
                            simplify = TRUE)

hashtag3 <- data.frame(hashtag1)

# menggabungkan kolom
hashtag4 <- hashtag3 %>%
  unite("hashtags", sep = " ") 
df_covid_net2 = bind_cols(df_covid_net, hashtag4)

#mentokenisassi data target pre processing
df_covid_net3 = df_covid_net2 %>%
  select(screen_name, hashtags)%>%
  unnest_tokens(tagar, hashtags, token = "words", to_lower = TRUE)

colnames(df_covid_net3)= c("sumber", "target")
glimpse(df_covid_net3)


#poste0()
df_covid_net3$sumber = paste0("@", df_covid_net3$sumber)
df_covid_net3$target = paste0("#", df_covid_net3$target)
glimpse(df_covid_net3)

df_covid_net3 = df_covid_net3 %>%
  count(sumber,target, sort = TRUE)
glimpse(df_covid_net3)
class(df_covid_net3)


#object graph/igraph
df_covid_net4 = df_covid_net3 %>%
  sample_n(100)

net1 = graph_from_data_frame(d = df_covid_net4, directed = FALSE)
plot(net1)
net2 = graph_from_data_frame(d = df_covid_net3, directed = FALSE)


write_graph(graph = net2, file = "C:/Users/ASUS/Downloads/kmmi-r/data/bersih/dataTest_covid_net.graphml", format = "graphml")

#mengekstrak fitur
?V()
nodes = data_frame(nodes = V(graph = net1)$name)

#menghitung centrality
##degree centrality

dc = centr_degree(graph = net1, mode = "all")
dc = data_frame(degree_centrality = dc$res)

all_net = bind_cols(nodes, dc)

?closeness()
?betweenness()
?eigen_centrality()

all_net = bind_cols(nodes, dc)

kelompok = cluster_walktrap(graph = net1)

?modularity
modularity(x = kelompok)
membership(communities = kelompok)

kelompok_anggt = membership(communities = kelompok)
kelompok_anggt = data_frame(kelompok_anggt = kelompok_anggt)
all_net = bind_cols(all_net, kelompok_anggt)


#re analisis
nodes_covid <- read_csv(file = "C:/Users/ASUS/Downloads/kmmi-r/data/bersih/nodes-covid.csv", trim_ws = FALSE)


#anggota kelompok
nodes_covid %>%
  count(modularity_class)%>%
  ggplot(aes(x = modularity_class, y = n))+
  geom_col()


#kelompok 0 ini apa atau siapa?
glimpse(nodes_covid)

kelompok_covid_79 <-  nodes_covid %>%
  filter(modularity_class == 79)

#siapa akun utama dari kelompok 0
user_kelompok_covid_79 = kelompok_covid_79 %>%
  filter(str_detect(Label, "^#"))

user_kelompok_covid_79 %>%
  arrange(desc(betweenesscentrality))%>%
  head(n = 10)%>%
  select(Label, betweenesscentrality)

user_kelompok_covid_79 %>%
  arrange(desc(betweenesscentrality))%>%
  head(n = 10)%>%
  select(Label, betweenesscentrality) %>%
  ggplot(aes(x = Label, y = betweenesscentrality))+
  geom_col()

user_kelompok_covid_79 %>%
  arrange(desc(betweenesscentrality))%>%
  head(n = 10)%>%
  select(Label, betweenesscentrality) %>%
  ggplot(aes(x = reorder(Label, betweenesscentrality), y = betweenesscentrality))+
  geom_col()+
  coord_flip()+
  labs(x = "USERNAME", y = "betweenesscentrality")


#pemakai hashtag utama pada kelompok 79




#siapakah @GICSManagement
wb = user_kelompok_covid_79 %>%
  filter(str_detect(Label, "covid19"))



