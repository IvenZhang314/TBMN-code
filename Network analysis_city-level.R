library(readxl)
library(sf)
library(igraph)
library(ggplot2)
library(ggraph)
library(dplyr)
library(scales) 
library(ggrepel)
library(backbone) 
library(edgebundle)
library(tidygraph)
library(geosphere)
library(ggnewscale)
####Network attribute calculation####
edge_city <- read_xlsx("edge_city1724final.xlsx")
node_city <- read_xlsx("node_city1724final.xlsx")

net<-graph_from_data_frame(d=edge_city,vertices=node_city,directed=TRUE)
net_filtered <- delete_edges(net, E(net)[E(net)$weight == 0])

# Global Properties
calc_global_metrics <- function(net, label) {
  v_count <- vcount(net)
  adj_mat <- as_adjacency_matrix(net, attr = "weight", sparse = FALSE)
  dist_mat <- distances(net, mode = "out", weights = 1/E(net)$weight)
  inv_dist <- 1 / dist_mat
  diag(inv_dist) <- 0 
  net_undir <- as_undirected(
    net,
    mode = "collapse",
    edge.attr.comb = list(weight = "sum")
  )
  tibble(
    Year = label,
    Total_Strength = sum(E(net)$weight),
    Network_Density = edge_density(net),
    Avg_Weighted_Clustering = mean(transitivity(net_undir, type = "barrat", 
                                                weights = E(net_undir)$weight, 
                                                isolates = "zero"), na.rm = TRUE),
    Assortativity = assortativity(net, values = strength(net), directed = TRUE),
    Weighted_Reciprocity = sum(pmin(adj_mat, t(adj_mat))) / sum(adj_mat),
    Global_Efficiency = sum(inv_dist) / (v_count * (v_count - 1))
  )
}

net_list <- readRDS("net_list_city.rds")
all_nets <- c(list("Total" = net_filtered), net_list)

final_global_df <- map_dfr(names(all_nets), ~calc_global_metrics(all_nets[[.x]], .x))

final_table_journal <- final_global_df %>%
  mutate(
    Total_Strength = round(Total_Strength, 0),
    across(where(is.double) & !c(Total_Strength), \(x) round(x, 4)) 
  ) %>%
  rename(
    "Year" = Year,
    "Network Strength" = Total_Strength,
    "Network Density" = Network_Density,
    "Avg. Weighted Clustering Coeff." = Avg_Weighted_Clustering,
    "Assortativity (Strength)" = Assortativity,
    "Weighted Reciprocity" = Weighted_Reciprocity,
    "Global Efficiency" = Global_Efficiency
  )

# s-core
library(brainGraph)
library(igraph)
net_undir <- as_undirected(net_filtered, 
                           mode = "collapse", 
                           edge.attr.comb = list(weight = "sum"))
adj_matrix <- as_adjacency_matrix(net_undir, attr = "weight", sparse = FALSE)
s_core_result <- s_core(net_undir, W = adj_matrix)
city_labels <- V(net_undir)$pinyin
df_s_core <- data.frame(
  City = city_labels,         
  s_core = s_core_result     
)
df_final <- df_s_core %>%
  mutate(s_Rank = rank(-s_core, ties.method = "min")) %>%
  mutate(`s-core (Rank)` = paste0(s_core, " (", s_Rank, ")"))


#Local properties
in_strength <- strength(net, mode = "in", weights = E(net)$weight)
out_strength <- strength(net, mode = "out", weights = E(net)$weight)
total_strength <- strength(net, mode = "total", weights = E(net)$weight)
node_betweenness <- betweenness(net_filtered, directed = TRUE, 
                                weights = 1 / E(net_filtered)$weight, 
                                normalized = TRUE)
pr_val <- page_rank(net_filtered, directed = TRUE, damping = 0.85, 
                    weights = E(net_filtered)$weight)$vector
raw_df <- data.frame(
  City = V(net_filtered)$pinyin, 
  In_Raw = as.numeric(in_strength),
  Out_Raw = as.numeric(out_strength),
  Total_Raw = as.numeric(total_strength),
  Bet_Raw = as.numeric(node_betweenness),
  PR_Raw = as.numeric(pr_val)
)



####Fig. 2a####
edges <- read_xlsx("edge_city1724final.xlsx")
nodes <- read_xlsx("node_city1724final.xlsx")
jiangsu_shp <- st_read("江苏省市级2024.shp", options = "ENCODING=UTF-8")
jiangsu_shp <- st_make_valid(jiangsu_shp)
jiangsu_outline <- st_union(jiangsu_shp)

net<-graph_from_data_frame(d=edges,vertices=nodes,directed=TRUE)
net_filtered <- delete_edges(net, E(net)[E(net)$weight == 0])

backbone_simple <- disparity(net_filtered, alpha = 0.05)
simple_edges <- igraph::as_edgelist(backbone_simple, names = TRUE)
eids_in_original_net <- igraph::get_edge_ids(
  net_filtered, 
  c(t(simple_edges))
)

backbone_graph <- subgraph_from_edges(
  net_filtered, 
  eids = eids_in_original_net
)

V(backbone_graph)$weighted_degree <- V(net_filtered)$weighted_degree
V(backbone_graph)$weighted_betweenness <- V(net_filtered)$weighted_betweenness

E(net_filtered)$is_backbone <- FALSE
E(net_filtered)$is_backbone[eids_in_original_net] <- TRUE
edge_table <- igraph::as_data_frame(net_filtered, what = "edges")

# plot
geo_layout_combined <- create_layout(net_filtered, 
                                     layout = 'manual', 
                                     x = V(net_filtered)$x, 
                                     y = V(net_filtered)$y)


p_2a <- ggraph(geo_layout_combined) +
  geom_sf(data = jiangsu_shp, fill = "white", color = "gray10", 
          inherit.aes = FALSE, linewidth = 0.15) +
  geom_sf(data = jiangsu_outline, fill = "transparent", color = "black", linewidth = 0.3)+

  geom_edge_arc(
    aes(filter = is_backbone == FALSE), 
    color = "#B0E4F5", 
    strength = 0.2, 
    linewidth = 0.2,
    alpha = 0.5
  ) +
  new_scale("edge_color") +

  geom_edge_arc(
    aes(color = weight,  filter = is_backbone == TRUE), 
    strength = 0.2, 
    linewidth = 0.4
  ) +
  scale_edge_color_gradient(
    low = "#FFE4E4", high = "#C21D57", 
    name = "Backbone flow",
    limits = c(16, 200), oob = scales::squish,
    breaks = c(16, 200),
    labels = c("Low","High"),
    guide = guide_edge_colorbar(ticks = FALSE, order = 3)
  ) +
  scale_edge_alpha_continuous(
    limits = c(1, 200), range = c(0.5, 0.9), oob = scales::squish, guide = "none"
  ) +
  geom_node_point(
    aes(size = weighted_degree, fill = weighted_betweenness), 
    shape = 21, color = "black", stroke = 0.2
  ) +
  
  scale_size_continuous(
    name = "Strength", range = c(0.8, 4.5), breaks = c(400,600,900,2000), labels = NULL
  ) +
  
  scale_fill_gradient(
    name = "Betweenness", low = "lightyellow", high = "darkred",
    limits = c(1, 28), breaks = c(1, 28), labels = c("Low", "High"),
    guide = guide_colorbar(ticks = FALSE), oob = scales::squish
  ) +
  
  guides(
    size = guide_legend(title = "Strength", order = 1),
    fill = guide_colorbar(title = "Betweenness", ticks = FALSE, order = 2)
  ) +
  
  theme_void(base_size = 8) +
  theme(
    panel.border = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.ticks = element_blank(),
    legend.position = c(0.1, 0.15), 
    legend.spacing.y = unit(0, "cm"), 
    legend.justification = c(0, 0),
    legend.direction = "horizontal",     
    legend.box = "vertical",           
    legend.key.width = unit(0.4, "cm"), 
    legend.key.height = unit(0.2, "cm"),
    legend.title.position = "top",       
    legend.title = element_text(hjust = 0.5), 
    legend.ticks = element_blank() 
  )



####Fig. 2c####
jiangsu_shp <- st_read("江苏省市级2024.shp", options = "ENCODING=UTF-8")
jiangsu_shp <- st_make_valid(jiangsu_shp)
jiangsu_shp$gb <- substr(jiangsu_shp$gb,4,9)

jiangsu_outline <- st_union(jiangsu_shp)
edges <- read_xlsx("edge_city1724final.xlsx")
nodes <- read_xlsx("node_city1724final.xlsx")
net<-graph_from_data_frame(d=edges,vertices=nodes,directed=TRUE)
net_filtered <- delete_edges(net, E(net)[E(net)$weight == 0])

#Leiden algorithm
net_undirected <- as_undirected(net_filtered, 
                                mode = "collapse", 
                                edge.attr.comb = list(weight = "sum"))
# net_undirected <- subgraph_from_edges(
#   net_undirected,
#   eids = which(E(net_undirected)$weight > 10), 
#   delete.vertices = FALSE  
# )
communities_leiden <- cluster_leiden(net_undirected,
                                     objective_function = "modularity", 
                                     weights = E(net_undirected)$weight,
                                     resolution =1)
communities_leiden
q_value <- modularity(
  net_undirected,           
  membership(communities_leiden),     
  weights = E(net_undirected)$weight   
)

membership_vec <- membership(communities_leiden)
community_info_df <- data.frame(
  adcode = nodes$adcode,
  pinyin = nodes$pinyin,
  community = as.factor(membership_vec)
)

citycode <- read_xlsx("江苏省市级行政编码及坐标.xlsx")
jiangsu_shp <- jiangsu_shp %>% left_join(select(citycode,"fullname","adcode"),by=c("name"="fullname"))
community_info_df$adcode <- as.character(community_info_df$adcode)
jiangsu_shp$adcode <- as.character(jiangsu_shp$adcode)
map_data <- jiangsu_shp %>%
  left_join(community_info_df, by = c("adcode"))

library(RColorBrewer)
color_palette <- alpha(paletteer_d("ggsci::category20_d3")[-8],alpha=0.8) 

# plot
p_2c <- ggplot(data = map_data) +
  
  geom_sf(aes(fill = community), 
          color = "grey10",      
          linewidth = 0.15) +     
  
  geom_sf(data = jiangsu_outline, fill = "transparent", color = "black", linewidth = 0.3)+
  
  geom_sf_text(
    data = map_data, 
    aes(label = pinyin), 
    size = 2.2,         
    color = "black"  
  ) +
  
  scale_fill_manual(values = color_palette, 
                    name = "Clusters",
                    na.value = "grey90") + 
  
  guides(
    fill = guide_legend(
      override.aes = list(color = NA),
      nrow = 1,            
      byrow = FALSE,        
      title.position = "top", 
      title.hjust = 0
    )
  ) +
  
  theme_void(base_size = 8) +

  annotate(
    geom = "text",
    x = 121.9, y = 34.3,      
    label = sprintf("Q = %.2f", q_value),
    size = 4,                 
    color = "black",          
    hjust = 1                
  )+
  theme(
    panel.border = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = c(0.1, 0.15),
    legend.justification = c(0, 0),
    legend.key.width = unit(0.4, "cm"),  
    legend.key.height = unit(0.3, "cm")
  )





####Fig. 2e####
library(readxl)
library(igraph)
library(writexl)
library(dplyr)
library(tidyr)
library(pheatmap)
library(blockmodels)
library(RColorBrewer)
library(ComplexHeatmap)
library(circlize)
library(factoextra)
library(cluster)

edge_city <- read_xlsx("edge_city1724final.xlsx")
node_city <- read_xlsx("node_city1724final.xlsx")
net<-graph_from_data_frame(d=edge_city,vertices=node_city,directed=TRUE)
net_filtered <- delete_edges(net, E(net)[E(net)$weight == 0])
V(net_filtered)$name <- V(net_filtered)$pinyin

adj_mat <- as_adjacency_matrix(net_filtered, attr = "weight", sparse = FALSE)
adj_mat <- log1p(adj_mat)

combined_features <- cbind(adj_mat, t(adj_mat))
dissimilarity_matrix <- dist(combined_features, method = "euclidean")
# Hierarchical clustering
hc <- hclust(dissimilarity_matrix, method = "ward.D2")

plot(hc)

# Profile coefficient method determine k
p <- fviz_nbclust(combined_features, FUN = hcut, method = "silhouette", 
                  hc_method = "ward.D2", k.max = 10)

plot_data <- p$data
plot_data$clusters <- as.numeric(as.character(plot_data$clusters))
plot_data_filtered <- subset(plot_data, clusters >= 3)

# Fig. S7c
p_S7c <- ggplot(plot_data_filtered, aes(x = clusters, y = y)) +
  geom_line(color = "black") +           # 连线
  geom_point(color = "black") +          # 数据点
  geom_vline(xintercept = 3, linetype = "dashed", color = "black") +
  labs(title = NULL,
       x = "Number of clusters",
       y = "Average silhouette width") +
  scale_x_continuous(breaks = 3:10) +        # 强制 X 轴显示整数刻度
  theme_classic(base_size = 6)+
  theme(
    axis.line.y = element_line(color = "black",linewidth = 0.2), # 保留左侧Y轴线
    axis.line.x = element_line(color = "black",linewidth = 0.2), # 保留底部X轴线
  )

num_clusters <- 3
block_membership <- cutree(hc, k = num_clusters)

node_roles <- data.frame(
  City = names(block_membership),
  Block_ID = block_membership
) 
node_roles <- node_roles %>% arrange(Block_ID)
global_density <- sum(adj_mat) / (nrow(adj_mat) * (ncol(adj_mat) - 1))

calc_block_density <- function(mat, members, k) {
  density_mat <- matrix(0, nrow = k, ncol = k)
  for (i in 1:k) {
    for (j in 1:k) {
      nodes_i <- which(members == i)
      nodes_j <- which(members == j)
      sub_mat <- mat[nodes_i, nodes_j, drop = FALSE]
      density_mat[i, j] <- sum(sub_mat) / (length(nodes_i) * length(nodes_j)) 
    }
  }
  return(density_mat)
}

block_densities <- calc_block_density(adj_mat, block_membership, num_clusters)

# Sort
node_strength <- rowSums(adj_mat) + colSums(adj_mat)
block_avg <- tapply(node_strength, block_membership, mean)
ranking_order <- names(sort(block_avg, decreasing = TRUE))
new_label_map <- setNames(as.character(1:length(ranking_order)), ranking_order)
block_membership_new <- new_label_map[as.character(block_membership)]
sorted_block_ids <- as.character(1:length(ranking_order))
df_order <- data.frame(
  Block = block_membership_new,       
  Orig_Idx = 1:length(block_membership_new) 
)
df_order$Block_F <- factor(df_order$Block, levels = sorted_block_ids)
new_order <- df_order %>% arrange(Block_F, Orig_Idx) %>% pull(Orig_Idx)
adj_mat_sorted <- adj_mat[new_order, new_order]
block_info <- factor(block_membership_new[new_order], levels = sorted_block_ids)
sorted_cities <- rownames(adj_mat)[new_order]
sorted_blocks <- block_membership_new[new_order]
city_block_df <- data.frame(
  City = sorted_cities,
  Block = sorted_blocks,
  stringsAsFactors = FALSE 
)
col_fun = colorRamp2(c(0,2,4,6), c("#FFF7EC", "#FC9272", "#EF3B2C", "#CB181D"))
block_colors <- c("1" = "#D62728",  
                  "2" = "#55A868",
                  "3" = "#6CA6CD")  

row_ha <- rowAnnotation(
  Block = block_info,
  col = list(Block = block_colors),
  show_annotation_name = FALSE,
  annotation_legend_param = list(
    Block = list(  
      title_gp = gpar(fontsize = 11), 
      labels_gp = gpar(fontsize = 11),                  
      at = c("1", "2", "3"),                             
      labels = c("Core", "Semiperiphery", "Periphery"),   
      
      grid_width = unit(4, "mm"),   
      grid_height = unit(4, "mm"),  
      row_gap = unit(3, "mm")    
    )
  )
)

col_ha <- HeatmapAnnotation(
  Block = block_info,
  col = list(Block = block_colors),
  show_annotation_name = FALSE,
  show_legend = FALSE
)

# Fig. S7a
p_S7a <- Heatmap(adj_mat_sorted,
              name = "Migration\nintensity",
              col = col_fun,
              width = unit(14, "cm"),
              height = unit(14, "cm"),
              
              cluster_rows = FALSE,
              cluster_columns = FALSE,
              
              row_split = block_info,
              column_split = block_info,
              cluster_row_slices = FALSE,
              cluster_column_slices = FALSE,
              
              row_names_gp = gpar(fontsize = 11),    
              column_names_gp = gpar(fontsize = 11), 
              column_names_rot = 40,                 
              
              rect_gp = gpar(col = "white", lwd = 1),
              row_title = NULL,
              column_title = NULL,
              
              left_annotation = row_ha,
              top_annotation = col_ha,
              
              heatmap_legend_param = list(
                ticks = FALSE,
                border = FALSE,
                at = c(0, 6), 
                labels = c("Low", "High"),
                legend_width = unit(5, "cm"),
                legend_height = unit(5, "cm"),
                title_gp = gpar(fontsize = 11), 
                labels_gp = gpar(fontsize = 11)
              )
)



data <- data.frame(
  Block = factor(c("1", "2", "3"), levels = c("1", "2", "3")),
  Intensity = c(75.03907, 56.18068, 53.76051)
)
block_colors <- c("1" = "#D62728",  
                  "2" = "#55A868",
                  "3" = "#6CA6CD")  
# Fig. S7d
p_S7d <- ggplot(data, aes(x = Block, y = Intensity, fill = Block)) +
  geom_bar(stat = "identity", width = 0.4) +
  geom_text(aes(label = sprintf("%.2f", Intensity)), vjust = -0.5, size = 2.2) +
  scale_fill_manual(values = block_colors) +
  labs(x = "Block", y = "Average node strength") +
  theme_classic() +
  theme(
    axis.line.y = element_line(color = "black",linewidth = 0.2), 
    axis.line.x = element_line(color = "black",linewidth = 0.2),
    axis.text.y = element_text(size = 6, color = "black"),
    axis.text.x = element_blank(),              
    axis.ticks.x = element_blank(),             
    axis.title = element_text(size = 6),
    legend.position = "none"
  ) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.08))) 



#####Calculation of coordinates of concentric circles #####
library(igraph)

g <- net_filtered
layers <- as.numeric(as.factor(block_membership_new)) 
n <- igraph::vcount(g)
node_names <- igraph::V(g)$name

radius_map <- c(
  "1" = 0.7,   # core
  "2" = 2,     # semicore
  "3" = 3     # semipheriphery
)

x <- numeric(n); y <- numeric(n)

for(i in unique(layers)) {
  idx <- which(layers == i)
  n_in_layer <- length(idx)
  if(n_in_layer == 0) next
  r <- radius_map[as.character(i)]
  theta <- seq(0, 2*pi, length.out = n_in_layer + 1)[1:n_in_layer]
  x[idx] <- r * cos(theta)
  y[idx] <- r * sin(theta)
}

# Export .net files
outfile <- "satellite_layout_city.net"
node_w <- igraph::degree(g)
edge_w <- igraph::E(g)$weight
if(is.null(edge_w)) edge_w <- rep(1, igraph::ecount(g))

cat(paste("*Vertices", n), file=outfile, sep="\n")
nodes_df <- data.frame(
  id = 1:n,
  label = paste0('"', node_names, '"'),
  x = x, 
  y = y, 
  z = 0, 
  w = node_w
)
write.table(nodes_df, outfile, row.names=FALSE, col.names=FALSE, quote=FALSE, append=TRUE, sep=" ")

cat("*Edges", file=outfile, sep="\n", append=TRUE)
edges_df <- igraph::as_data_frame(g, what="edges")
edges_out <- data.frame(
  from = match(edges_df$from, node_names),
  to   = match(edges_df$to,   node_names),
  w    = edge_w
)
write.table(edges_out, outfile, row.names=FALSE, col.names=FALSE, quote=FALSE, append=TRUE, sep=" ")

#  Export .clu files
clufile <- "satellite_layout_city.clu"
cat(paste("*Vertices", n), file=clufile, sep="\n")
write.table(layers, clufile, row.names=FALSE, col.names=FALSE, quote=FALSE, append=TRUE)


# Import the two exported files into VOSviewer for rendering.

#County-level codes omitted.









