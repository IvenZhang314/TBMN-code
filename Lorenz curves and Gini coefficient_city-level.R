library(readxl)
library(ggplot2)
library(dplyr)
library(igraph)
library(scales)
library(ineq) 
library(ggnewscale)
####Lorenz (Fig. 3a)####
# data paration
edge_county <- read_xlsx("edge_city1724final.xlsx")
node_county <- read_xlsx("node_city1724final.xlsx")

net<-graph_from_data_frame(d=edge_county,vertices=node_county,directed=TRUE)
net_filtered <- delete_edges(net, E(net)[E(net)$weight == 0])

in_strength_values <- strength(net, mode = "in")
out_strength_values <- strength(net, mode = "out")

net_list <- readRDS("net_list_city.rds")
names(net_list) <- 2017:2024

lorenz_data_in <- list()
lorenz_data_out <- list()
gini_table_list <- list()

for (yr in names(net_list)) {
  net <- net_list[[yr]]

  val_in <- strength(net, mode = "in")
  gini_in <- Gini(val_in)
  sorted_val_in <- sort(val_in)
  n_in <- length(sorted_val_in)
  
  lorenz_data_in[[yr]] <- data.frame(
    Year = as.character(yr), 
    p = 0:n_in, 
    L = c(0, cumsum(sorted_val_in)/sum(sorted_val_in)),
    Type = "Inflow"
  )
  
  val_out <- strength(net, mode = "out")
  gini_out <- Gini(val_out)
  sorted_val_out <- sort(val_out)
  n_out <- length(sorted_val_out)
  
  lorenz_data_out[[yr]] <- data.frame(
    Year = as.character(yr),
    p = 0:n_out, 
    L = c(0, cumsum(sorted_val_out)/sum(sorted_val_out)),
    Type = "Outflow"
  )
  
  gini_table_list[[yr]] <- data.frame(
    Year = yr,
    Gini_In = round(gini_in, 4),
    Gini_Out = round(gini_out, 4)
  )
}

plot_data_in <- bind_rows(lorenz_data_in)
plot_data_out <- bind_rows(lorenz_data_out)
gini_summary <- bind_rows(gini_table_list)

red_colors <- rev(viridis::plasma(11))[2:9]
blue_colors <- rev(viridis::viridis(10))[2:9]

# plot
p1 <- ggplot() +

  geom_abline(slope = 1/13, intercept = 0, color = "black", 
              linetype = "dashed", linewidth = 0.3) +

  geom_line(data = plot_data_in, 
            aes(x = p, y = L, color = Year), 
            linewidth = 0.3, alpha = 0.8) +

  scale_color_manual(name = "Inflow", values = red_colors,
                     guide = guide_legend(ncol = 1, title.position = "top", title.hjust = 0.5)
  ) +
  
  new_scale_color() +
  
  geom_line(data = plot_data_out, 
            aes(x = p, y = L, color = Year), 
            linewidth = 0.3, alpha = 0.8) +

  scale_color_manual(name = "Outflow", values = blue_colors,
                     guide = guide_legend(ncol = 1, title.position = "top", title.hjust = 0.5)
  ) +
  
  scale_x_continuous(
    name = "Cumulative number of cities",
    limits = c(0, 13), expand = c(0, 0), breaks = seq(0, 13, 1)
  ) +
  scale_y_continuous(
    name = "Cumulative case flow (%)", 
    limits = c(0, 1), expand = c(0, 0),
    labels = function(x) x * 100, breaks = seq(0, 1, 0.2)
  ) +
  
  theme_classic(base_size = 8) + 
  theme(
    plot.title = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = c(0.05, 0.95), 
    legend.justification = c(0, 1),
    legend.background = element_blank(), 
    legend.box = "horizontal", 
    legend.spacing.x = unit(0.5, "cm"), 
    legend.title = element_text(),
    legend.text = element_text(),
    legend.key = element_blank(),
    legend.key.height = unit(0.3, "cm"),
    legend.key.width = unit(1.0, "cm")
  )



####Gini in (Fig. 3c)####
gini_summary$Year_Fac <- factor(gini_summary$Year)
model <- lm(Gini_In ~ as.numeric(Year), data = gini_summary)
stats <- summary(model)

slope_val <- coef(model)[2]
r2_val <- stats$r.squared
p_val <- stats$coefficients[2, 4]

library(ggtext)
p_text <- ifelse(p_val < 0.001, "p-value < 0.001", paste("p-value =", round(p_val, 3)))
slope_txt <- paste0(round(slope_val * 100, 2), "% yr<sup>-1</sup> ***")

label_text <- paste0(
  slope_txt, "<br>", 
  "R<sup>2</sup> = ", round(r2_val, 2), "<br>",
  p_text
)
windowsFonts(Arial = windowsFont("Arial"))

# plot
p2 <- ggplot(gini_summary, aes(x = as.numeric(Year), y = Gini_In)) +

  geom_smooth(method = "lm", color = "grey40", linetype = "dashed", se = FALSE, linewidth = 0.8) +

  geom_line(color = "black", linewidth = 0.6) +

  geom_point(aes(color = Year_Fac), size = 2.3) + 
  scale_color_manual(values = red_colors) +

  guides(color = "none") +
  
  scale_x_continuous(
    name = "Year",
    breaks = seq(2017, 2024, by = 2),
    labels = seq(2017, 2024, by = 2)
  ) +
  
  scale_y_continuous(name = "Gini index (inflow)",
                     labels = label_number(accuracy = 0.01))+

  annotate("richtext", 
           x = 2017.2, 
           y = 0.715, 
           label = label_text, 
           hjust = 0, vjust = 1, 
           size = 2.5,
           family = "Arial",      
           fill = NA,            
           label.color = NA      
  ) +
  theme_classic(base_size = 8) +
  theme(legend.position = "none")



#####Gini out (Fig. 3d)####
gini_summary$Year_Fac <- factor(gini_summary$Year)
model <- lm(Gini_Out ~ as.numeric(Year), data = gini_summary)
stats <- summary(model)

slope_val <- coef(model)[2]
r2_val <- stats$r.squared
p_val <- stats$coefficients[2, 4]

library(ggtext)
p_text <- ifelse(p_val < 0.001, "p-value < 0.001", paste("p-value =", round(p_val, 3)))
slope_txt <- paste0(round(slope_val * 100, 2), "% yr<sup>-1</sup>")
label_text <- paste0(
  slope_txt, "<br>", 
  "R<sup>2</sup> = ", round(r2_val, 2), "<br>",
  p_text
)


# plot
p3 <- ggplot(gini_summary, aes(x = as.numeric(Year), y = Gini_Out)) +

  geom_smooth(method = "lm", color = "grey40", linetype = "dashed", se = FALSE, linewidth = 0.8) +

  geom_line(color = "black", linewidth = 0.6) +

  geom_point(aes(color = Year_Fac), size = 2.3) + 
  scale_color_manual(values = blue_colors) +

  guides(color = "none") +
  
  scale_x_continuous(
    name = "Year",
    breaks = seq(2017, 2024, by = 2),
    labels = seq(2017, 2024, by = 2)
  ) +
  
  scale_y_continuous(name = "Gini index (outflow)",
                     labels = label_number(accuracy = 0.01))+

  annotate("richtext", 
           x = 2017.2, 
           y = 0.295, 
           label = label_text, 
           hjust = 0, vjust = 1, 
           size = 2.5,
           family = "Arial",      
           fill = NA,            
           label.color = NA      
  ) +
  
  theme_classic(base_size = 8) +
  theme(legend.position = "none")


# County-level codes omitted.








