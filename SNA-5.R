library(tidyverse)
library(tidytext)
library(textclean)
library(igraph)
library(ggraph)

# data ----
df1 <- read_csv("Documents/WORK/SADASA/Data Science/SADASA'21/UDINUS/SNA Wth R/SNA/cov_tweet.csv")
df1 = df1 %>%
  select(user_name, full_text)

# pre-processing ----
mentioned <- str_extract_all(string = df1$full_text,
                             pattern = "(@[[:alnum:]_]*)", 
                             simplify = TRUE)

mentioned <- data.frame(mentioned)
mentioned <- mentioned %>%
  unite("mention", sep = " ")

df1 = bind_cols(df1, mentioned)
df1$mention = replace_white(df1$mention)
df1$mention = str_trim(string = df1$mention, side = "both")
glimpse(df1)

# membuat data network
## tokenisasi target
df2 = df1 %>%
  unnest_tokens(mentioned, mention, token = "words",
                to_lower = FALSE, drop = FALSE)
df3 = df2 %>%
  select("sumber" = user_name, "target" = mentioned)

## Perkecil data
df3 = df3 %>%
  group_by(sumber) %>%
  count(target) %>%
  filter(n >= 2)
class(df3)
glimpse(df3)

## membuat data network
df_net = graph_from_data_frame(d = df3, directed = FALSE)

# visualisasi
plot(df_net)

ec <- eigen_centrality(graph = df_net,
                       directed = FALSE,
                       weights = NA)$vector
plot(df_net, vertex.size = ec*10, layout = layout_with_fr)

## Layout network
coords <- layout_(df_net, as_star(), normalize())
plot(df_net, vertex.size = ec*10, layout = coords)

plot(df_net, vertex.size = ec*10, layout = layout_with_dh)

## Visualisasi ggraph
df_net %>%
  ggraph(layout = "kk") +
  geom_node_point() +
  geom_edge_link() +
  theme_graph()

### geom_node_point adl nodenya, geom_edge_link adl edgenya

# membuat cluster
wc <- cluster_walktrap(graph = df_net)
col <- wc$membership

df_net %>%
  ggraph(layout = "kk") +
  geom_edge_link(edge_colour = "grey") +
  geom_node_point(aes(size = ec, color = as.factor(col)),
                  show.legend = FALSE) +
  geom_node_text(aes(label = name), repel = FALSE) +
  scale_fill_manual() + theme_graph()

# Load package ----
library(networkD3)

# Create fake data
src <- c("A", "A", "A", "A",
         "B", "B", "C", "C", "D")
target <- c("B", "C", "D", "J",
            "E", "F", "G", "H", "I")  
networkData <- data.frame(src, target)

# Plot
simpleNetwork(networkData)
simpleNetwork(df3)

# Load data
data(MisLinks)
data(MisNodes)

# Plot
forceNetwork(Links = MisLinks,
             Nodes = MisNodes,
             Source = "source",
             Target = "target",
             Value = "value",
             NodeID = "name",
             Group = "group",
             Nodesize = "size",
             opacity = 0.8)


library(echarts4r)
value <- rnorm(10, 10, 2)

nodes <- data.frame(
  name = sample(LETTERS, 10),
  value = value,
  size = value,
  stringsAsFactors = FALSE
)

edges <- data.frame(
  source = sample(nodes$name, 20, replace = TRUE),
  target = sample(nodes$name, 20, replace = TRUE),
  stringsAsFactors = FALSE
)

e_charts() %>%  
  e_graph() %>% 
  e_graph_nodes(nodes, name, value, size) %>%  
  e_graph_edges(edges, source, target) %>% 
  e_tooltip()
