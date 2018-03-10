library(tidyverse)

letters <- read_csv("Data/correspondence-data-1585.csv", 
                    col_types = cols(date = col_date(format = "%Y-%m-%d")))

View(letters)

## Node list

sources <- letters %>%
  distinct(source) %>%
  rename(label = source)
View(sources)

destinations <- letters %>%
  distinct(destination) %>%
  rename(label = destination)

nodes <- full_join(sources, destinations, by="label")
nodes <- nodes %>% rowid_to_column("id")
View(nodes)

## Edge list 

per_route <- letters %>% 
  group_by(source, destination) %>%
  summarise(weight = n()) %>%
  ungroup()
View(per_route)

edges <- per_route %>%
  left_join(nodes, by=c("source"="label")) %>%
  rename(from=id)
View(edges)
edges <- edges %>%
  left_join(nodes, by=c("destination"="label")) %>%
  rename(to=id)
View(edges)
edges <- edges %>% select(from, to, weight)
View(edges)

library(tidygraph)
library(ggraph)

routes_tidy <- tbl_graph(nodes=nodes, edges=edges, directed = T)
class(routes_tidy)
routes_tidy

ggraph(routes_tidy) + geom_edge_link() +geom_node_point() + theme_graph()

ggraph(routes_tidy, layout = "graphopt") +
  geom_node_point() +
  geom_edge_link(aes(width = weight), alpha=0.8) +
  scale_edge_width(range = c(0.2,2)) +
  geom_node_text(aes(label=label), repel = T) + 
  labs(edge_width = "No. of Letters") +
  theme_graph()

ggraph(routes_tidy, layout = "linear") +
  geom_edge_arc(aes(width=weight), alpha = 0.8) + 
  scale_edge_width(range = c(0.2,2)) +
  geom_node_text(aes(label = label)) + 
  labs(edge_width = "No. of letters") + 
  theme_graph()

library(visNetwork) # Interactive graphs
library(networkD3)

visNetwork(nodes, edges)
