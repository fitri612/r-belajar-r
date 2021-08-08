library(tidyverse)
library(tidytext)
library(igraph)

#baca data / import data
data_covid <- read.csv(file = "C:/Users/ASUS/Downloads/kmmi-r/data/mentah/covid19_tweet.csv",
                       nrows=10000)
View(data_covid)

#pilih data or kolom(select)
dataF_net = data_covid %>%
  select(screen_name, text)

#tokenisasi
dataF_net %>%
  head(10) %>%
  unnest_tokens(output = "mention", input = text, token = "words", to_lower =  FALSE, drop = FALSE)


#extract usernamne sing regex
regex <- "(^|[^@\\w])@(\\w{1,15})\\b"

?str_extract_all
mention1 <- str_extract_all(string = dataF_net$text, 
                            pattern = "(@[[:alnum:]_]*)", 
                            simplify = TRUE)

mention2 <- str_extract_all(string = dataF_net$text, 
                            pattern = regex, 
                            simplify = TRUE)

mention3 <- data.frame(mention1)

# menggabungkan kolom
mention4 <- mention3 %>%
  unite("mention", sep = " ") 

dataF_net2 = bind_cols(dataF_net, mention4)

#adjacency list
dataF_net3 = dataF_net2 %>%
  select(screen_name, mention)%>%
  unnest_tokens(dimention, mention, token = "ngrams", n = 1, to_lower = FALSE)
colnames(dataF_net3) = c("sumber", "target")

dataF_net4 = dataF_net3 %>%  
  filter(!is.na(target))

write_csv(dataF_net4, "C:/Users/ASUS/Downloads/kmmi-r/data/bersih/data_covid_net.csv")

#membuat data graph (data dari R bisa dilihat dan analisis di gephi)
dataF_net5 = dataF_net4 %>%
  group_by(sumber) %>%
  count(target, sort = TRUE) %>%
  filter( n >= 10)

class(dataF_net5)

net1 = graph_from_data_frame(d = dataF_net5, directed = FALSE)
class((net1))

plot(net1)

#membuat object graph untuk gephi
#rgexf

library(rgexf)
?write.gexf
write_graph(net1, file = "C:/Users/ASUS/Downloads/kmmi-r/data/bersih/data_covid_net.graphml", format = "graphml")

