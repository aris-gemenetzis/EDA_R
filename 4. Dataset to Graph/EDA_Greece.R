# 1. Build undirected network
library(dplyr)
library(tibble)
data <- read.csv(file="greece.csv", header=TRUE, sep=",")
data
sources <- data %>%
  distinct(from) %>%
  rename(label = from)
sources
destinations <- data %>%
  distinct(to) %>%
  rename(label = to)
destinations
nodes <- full_join(sources, destinations, by = "label")
nodes
nodes <- nodes %>% rowid_to_column("id")
nodes
data = data %>% 
  rename(
    source = from,
    destination = to
  )
data
per_route <- data %>%  
  group_by(source, destination) %>%
  summarise(distance = sum(distance)) %>% 
  ungroup()
per_route

edges <- per_route %>% 
  left_join(nodes, by = c("source" = "label")) %>% 
  rename(from = id)
edges

edges <- edges %>% 
  left_join(nodes, by = c("destination" = "label")) %>% 
  rename(to = id)

edges <- select(edges, from, to, distance)
edges

library(network)
routes_network <- network(edges, vertex.attr = nodes, matrix.type = "bipartite", ignore.eval = FALSE)
routes_network
# matrix.type = "edgelist" for directed network

# 2. Create network graph 
library(tidygraph)
library(ggraph)
graph_routes <- as_tbl_graph(routes_network)
graph_routes
routes_tidy <- tbl_graph(nodes = nodes, edges = edges, directed = TRUE)
routes_tidy
routes_tidy %>% 
  activate(edges) %>% 
  arrange(desc(distance))
ggraph(routes_tidy, layout = "graphopt") + 
  geom_node_point() +
  geom_edge_link(aes(width = distance), alpha = 0.8) + 
  scale_edge_width(range = c(0.2, 2)) +
  geom_node_text(aes(label = label), repel = TRUE) +
  labs(edge_width = "Letters") +
  theme_graph()

# 3. Find shortest path
library(stringr)
routes = data %>% 
  rename(
    from = source,
    to = destination
  )
routes
graph_routes <- as_tbl_graph(routes)
graph_routes
graph_routes <- graph_routes %>%
  activate(nodes) %>%
  mutate(
    title = str_to_title(name),
    label = str_replace_all(title, " ", "\n")
  )
graph_routes
stations <- graph_routes %>%
  activate(nodes) %>%
  pull(title)
stations
from <- which(stations == "Έβρος")
to <-  which(stations == "Καστοριά")
shortest <- graph_routes %>%
  morph(to_shortest_path, from, to, weights = distance)  # shortest distance
shortest
shortest %>%
  mutate(selected_node = TRUE) %>%
  unmorph()
shortest <- shortest %>%
  mutate(selected_node = TRUE) %>%
  activate(edges) %>%
  mutate(selected_edge = TRUE) %>%
  unmorph() 
shortest <- shortest %>%
  activate(nodes) %>%
  mutate(selected_node = ifelse(is.na(selected_node), 1, 2)) %>%
  activate(edges) %>%
  mutate(selected_edge = ifelse(is.na(selected_edge), 1, 2)) %>%
  arrange(selected_edge)

shortest
shortest %>%
  ggraph(layout = "kk") +
  geom_edge_diagonal(aes(alpha = selected_edge), color = "gray") +
  geom_node_text(aes(label = label, color =name, alpha = selected_node ), size = 3) 
