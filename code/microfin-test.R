adj_vil1 <- read_csv("Data/adj_allVillageRelationships_HH_vilno_1.csv", 
                     col_names = FALSE)
View(adj_vil1)

adj_vil1.nodes <- as_tibble(colnames(adj_vil1))

adj_vil1 <- adj_vil1 %>% mutate(entities = colnames(adj_vil1))

adj_vil1.edge <- gather(adj_vil1, HH_a, HH_b, X1:X182) %>% filter(HH_b == 1) %>% select(entities, HH_a)
colnames(adj_vil1.edge) <- c("HH_a", "HH_b")

visNetwork(adj_vil1.nodes, adj_vil1.edge)

routes <- tbl_graph(nodes=adj_vil1.nodes, edges=adj_vil1.edge, directed = T)

ggraph(routes, layout = "graphopt") +
  geom_node_point() +
  geom_edge_link(alpha=0.8) +
  scale_edge_width(range = c(0.2,2)) +
  geom_node_text(aes(label=adj_vil1$entities), repel = T) + 
  labs(edge_width = "No. of Letters") +
  theme_graph()

ggraph(routes, layout = "linear") +
  geom_edge_arc(alpha = 0.8) + 
  scale_edge_width(range = c(0.2,2)) +
  geom_node_text(aes(label = adj_vil1$entities)) + 
  labs(edge_width = "No. of letters") + 
  theme_graph()
