# Load required libraries
library(tidyverse)
library(tidygraph)
library(ggraph)
library(igraph)

# Sample data structure for government procurement
# In reality, you would import your actual data using read_csv() or similar
# Example: tenders <- read_csv("path/to/your/tenders_data.csv")
set.seed(123)
tenders <- tibble(
  tender_id = 1:100,
  agency = sample(c("Health Dept", "Education Dept", "Transport Dept", "Defense Dept", "Treasury"), 100, replace = TRUE),
  supplier = sample(c("ABC Corp", "XYZ Ltd", "123 Industries", "Tech Solutions", "Global Services", 
                      "Local Supplies", "Mega Contractors", "Small Business Inc"), 100, replace = TRUE),
  amount = sample(10000:1000000, 100, replace = TRUE),
  date = sample(seq(as.Date('2020-01-01'), as.Date('2023-12-31'), by="day"), 100),
  award_type = sample(c("Open Tender", "Limited Tender", "Direct Award"), 100, replace = TRUE),
  description = sample(c("IT Services", "Construction", "Consulting", "Office Supplies", "Training"), 100, replace = TRUE)
)

# 1. EXPLORATORY DATA ANALYSIS ---------------------------

# Summary of tenders by agency
agency_summary <- tenders %>%
  group_by(agency) %>%
  summarise(
    tenders_count = n(),
    total_value = sum(amount),
    avg_value = mean(amount),
    suppliers_count = n_distinct(supplier),
    .groups = "drop"
  ) %>%
  arrange(desc(tenders_count))

print("Agency Summary:")
print(agency_summary)

# Summary of tenders by supplier
supplier_summary <- tenders %>%
  group_by(supplier) %>%
  summarise(
    tenders_count = n(),
    total_value = sum(amount),
    avg_value = mean(amount),
    agencies_count = n_distinct(agency),
    .groups = "drop"
  ) %>%
  arrange(desc(tenders_count))

print("Supplier Summary:")
print(supplier_summary)

# Connections between agencies and suppliers
connections <- tenders %>%
  group_by(agency, supplier) %>%
  summarise(
    frequency = n(),
    total_value = sum(amount),
    .groups = "drop"
  ) %>%
  arrange(desc(frequency))

print("Top Connections:")
print(head(connections, 10))

# Visualize top agencies by tender count
ggplot(agency_summary %>% arrange(desc(tenders_count)) %>% head(5), 
       aes(x = reorder(agency, tenders_count), y = tenders_count)) +
  geom_col(fill = "dodgerblue") +
  coord_flip() +
  labs(title = "Top Agencies by Tender Count",
       x = NULL,
       y = "Number of Tenders") +
  theme_minimal()

# Visualize top suppliers by tender count
ggplot(supplier_summary %>% arrange(desc(tenders_count)) %>% head(5), 
       aes(x = reorder(supplier, tenders_count), y = tenders_count)) +
  geom_col(fill = "tomato") +
  coord_flip() +
  labs(title = "Top Suppliers by Tender Count",
       x = NULL,
       y = "Number of Tenders") +
  theme_minimal()

# Tender time series
tenders %>%
  mutate(month = floor_date(date, "month")) %>%
  count(month) %>%
  ggplot(aes(x = month, y = n)) +
  geom_line() +
  geom_point() +
  labs(title = "Tenders Over Time",
       x = "Date",
       y = "Number of Tenders") +
  theme_minimal()

# 2. CREATE NETWORK DATA ---------------------------

# Create nodes (unique agencies and suppliers)
agencies <- tibble(
  id = unique(tenders$agency),
  type = "agency"
)

suppliers <- tibble(
  id = unique(tenders$supplier),
  type = "supplier"
)

nodes <- bind_rows(agencies, suppliers) %>%
  mutate(node_id = row_number())

# Create edges (connections between agencies and suppliers)
edges <- tenders %>%
  select(agency, supplier, amount) %>%
  group_by(agency, supplier) %>%
  summarise(
    weight = n(),
    total_amount = sum(amount),
    avg_amount = mean(amount),
    .groups = "drop"
  ) %>%
  # Join to get node IDs
  left_join(nodes %>% select(id, node_id), by = c("agency" = "id")) %>%
  rename(from = node_id) %>%
  left_join(nodes %>% select(id, node_id), by = c("supplier" = "id")) %>%
  rename(to = node_id)

# Create tidygraph network
procurement_network <- tbl_graph(
  nodes = nodes,
  edges = edges %>% select(from, to, weight, total_amount, avg_amount)
)

# 3. BASIC NETWORK METRICS ---------------------------

# Calculate centrality measures
network_with_metrics <- procurement_network %>%
  activate(nodes) %>%
  mutate(
    degree = centrality_degree(),
    betweenness = centrality_betweenness(),
    closeness = centrality_closeness()
  )

# View nodes with metrics
node_metrics <- network_with_metrics %>%
  activate(nodes) %>%
  as_tibble()

print("Node Metrics:")
print(node_metrics %>% arrange(desc(degree)) %>% head(10))

# 4. NETWORK VISUALIZATION ---------------------------

# Basic network visualization
plot_basic <- ggraph(network_with_metrics, layout = "fr") +
  geom_edge_link(aes(width = weight, alpha = weight)) +
  geom_node_point(aes(color = type, size = degree)) +
  geom_node_text(aes(label = id), repel = TRUE, size = 3) +
  scale_edge_width(range = c(0.5, 3)) +
  scale_edge_alpha(range = c(0.2, 0.8)) +
  scale_size(range = c(3, 8)) +
  scale_color_manual(values = c("agency" = "blue", "supplier" = "red")) +
  theme_graph() +
  labs(title = "Government Procurement Network")

print(plot_basic)

# Color nodes by degree centrality
plot_degree <- ggraph(network_with_metrics, layout = "fr") +
  geom_edge_link(aes(width = weight), alpha = 0.3) +
  geom_node_point(aes(color = degree, size = degree, shape = type)) +
  scale_color_viridis_c() +
  scale_shape_manual(values = c("agency" = 17, "supplier" = 19)) +
  geom_node_text(aes(label = id), repel = TRUE, size = 3) +
  theme_graph() +
  labs(title = "Procurement Network - Colored by Degree Centrality")

print(plot_degree)

# Community detection (with proper handling for directed graphs)
network_with_communities <- network_with_metrics %>%
  convert(to_undirected()) %>%  # Convert to undirected for community detection
  mutate(community = as.factor(group_louvain())) %>%
  activate(nodes)

# Plot communities
plot_communities <- ggraph(network_with_communities, layout = "fr") +
  geom_edge_link(aes(width = weight), alpha = 0.3) +
  geom_node_point(aes(color = community, shape = type), size = 5) +
  scale_shape_manual(values = c("agency" = 17, "supplier" = 19)) +
  geom_node_text(aes(label = id), repel = TRUE, size = 3) +
  theme_graph() +
  labs(title = "Procurement Network - Communities")

print(plot_communities)

# 5. SAVE VISUALIZATIONS ---------------------------
# Uncomment these lines when you want to save the visualizations

# ggsave("basic_network.png", plot_basic, width = 12, height = 10)
# ggsave("degree_network.png", plot_degree, width = 12, height = 10)
# ggsave("community_network.png", plot_communities, width = 12, height = 10)

# 6. NETWORK SUMMARY STATISTICS ---------------------------
# Convert to igraph for some calculations
ig_network <- as.igraph(network_with_metrics)

network_summary <- tibble(
  metric = c("Nodes", "Edges", "Density", "Mean Degree", "Transitivity"),
  value = c(
    gorder(ig_network),
    gsize(ig_network),
    round(graph.density(ig_network), 4),
    round(mean(degree(ig_network)), 2),
    round(transitivity(ig_network), 4)
  )
)

print("Network Summary Statistics:")
print(network_summary)