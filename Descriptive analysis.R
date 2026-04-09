####Fig. 1a####
library(readxl)
library(sf)
library(ggplot2)
library(patchwork)
library(ggspatial)
library(geodata)
library(dplyr)
library(RColorBrewer)

# Read data
df <- read_xlsx("2017-2024逐年发病率.xlsx")%>%
  mutate(`发病率1/10万` = as.numeric(`发病率1/10万`))
city_map <- c(
  "南京市" = "Nanjing",
  "无锡市" = "Wuxi",
  "徐州市" = "Xuzhou",
  "常州市" = "Changzhou",
  "苏州市" = "Suzhou",
  "南通市" = "Nantong",
  "连云港市" = "Lianyungang",
  "淮安市" = "Huaian",
  "盐城市" = "Yancheng",
  "扬州市" = "Yangzhou",
  "镇江市" = "Zhenjiang",
  "泰州市" = "Taizhou",
  "宿迁市" = "Suqian"
)

df_processed <- df %>%
  mutate(pinyin = recode(地区名称, !!!city_map))

df_avg <- df_processed %>%
  group_by(地区名称, pinyin) %>%  
  summarise(平均发病率 = mean(`发病率1/10万`, na.rm = TRUE), .groups = 'drop') %>%
  arrange(平均发病率)

# Albers Equal Area Conic
albers = "+proj=aea +lat_1=25 +lat_2=47 +lon_0=105"
china_provinces <- read_sf("中国_省.geojson") |> st_transform(st_crs(albers))
jiangsu_cities <- st_read("江苏省市级2024.shp") 
jiangsu_cities_data <- left_join(jiangsu_cities, df_avg, by = c("name"="地区名称"))
jiangsu_cities_data <- st_make_valid(jiangsu_cities_data)
jiangsu_outline <- st_union(jiangsu_cities_data)

# mainplot
p_main <- ggplot() +
  geom_sf(data = jiangsu_cities_data, aes(fill = 平均发病率), color = "grey10", linewidth = 0.15) +
  
  scale_fill_distiller(palette = "Blues", direction = 1, 
                       name = "Annual incidence rate (per 100,000)",
                       breaks = c(26, 28, 30, 32),
                       guide = guide_colorbar(direction = "horizontal",
                                              barwidth = unit(10, "lines"), 
                                              barheight = unit(0.7, "lines"),
                                              title.position = "top",
                                              title.hjust = 0.5)) +
  
  geom_sf(
    data = jiangsu_outline,   
    fill = "transparent",      
    color = "black",         
    linewidth = 0.3        
  ) +
  
  coord_sf(
    xlim = c(115, 122), 
    ylim = c(30, 35.5),   
    expand = FALSE         
  ) +
  

  geom_sf_text(
    aes(label = pinyin), 
    data = jiangsu_cities_data, 
    size = 2.2,                
    color = "black",             
    fun.geometry = st_centroid    
  )+
  
  annotation_north_arrow(
    location = "tr", 
    which_north = "true",
    pad_x = unit(0.1, "cm"), 
    pad_y = unit(1, "cm"),
    style = north_arrow_fancy_orienteering() 
  ) +
  
  annotation_scale(
    location = "br",        
    style = "bar", 
    width_hint = 0.3,
    pad_x = unit(1, "cm"),   
    pad_y = unit(0.5, "cm"),  
    text_cex = 1.2
  ) +
  
  theme_bw() + 
  
  theme(
    legend.position = c(1, 0.05),     
    legend.justification = c("right", "bottom"), 
    axis.title = element_blank(),
    axis.text = element_blank(),   
    axis.ticks = element_blank(),  
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank(),
    legend.background = element_rect(fill = "transparent")
  )

# 
china_provinces_modified <- china_provinces %>%
  mutate(highlight = ifelse(name == "江苏省", "Jiangsu", "Other"))

china_provinces_modified <- china_provinces %>%
  mutate(highlight = ifelse(name == "江苏省", "Jiangsu", "Other"))

# Thumbnail
p_cn <- ggplot() +
  geom_sf(
    data = china_provinces_modified, 
    aes(fill = highlight), 
    color = "black",  
    linewidth = 0.2     
  ) +
  
  scale_fill_manual(
    name = NULL,
    values = c("Jiangsu" = "red", "Other" = "white"), 
    breaks = "Jiangsu", 
    labels = "Jiangsu province" 
  ) +
  
  guides(fill = guide_legend(
    keywidth = unit(0.7, "cm"), 
    keyheight = unit(0.35, "cm") 
  )) +
  
  annotation_scale(
    style = "bar",
    location = "bl",      
    width_hint = 0.3,    
    pad_x = unit(0.6, "cm"), 
    pad_y = unit(0.8, "cm"),
    text_cex = 1.2
  ) +
  
  theme_bw() +
  theme(
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank(),
    legend.position = c(0.3, 0.3), 
    legend.background = element_rect(fill = "transparent"), 
    legend.key.width = unit(0.5, 'cm'),  
    legend.key.height = unit(0.3, 'cm'), 
    legend.text = element_text(),
    legend.spacing.x = unit(0.05, 'cm')
  )

p_1a <- p_main + inset_element(
  p_cn,
  left = 0.005,
  bottom = 0.01,
  right = 0.45,
  top = 0.55
)


####Fig. 1b####
library(ggplot2)
library(dplyr)
library(scales)

df_clean <- df_processed %>%
  mutate(年份 = as.factor(年份))
city_order <- c("Lianyungang","Xuzhou", "Suqian","Yancheng", "Huaian", "Yangzhou",
                "Taizhou","Nantong","Nanjing", "Zhenjiang", "Changzhou", "Wuxi", 
                "Suzhou")
p_1b <- ggplot(df_clean, aes(x = factor(地区名称, levels = city_order), 
                           y = forcats::fct_rev(factor(年份)))) +
  
  geom_tile(fill = "transparent", color = NA, linewidth = 0.15) +

  geom_point(aes(size = `发病率1/10万`, fill = `发病率1/10万`), 
             shape = 21, 
             color = "transparent", 
             stroke = 0) +  
  
  scale_fill_gradient(
    low = "#F5C7CE",       
    high = "#C21D57",      
    limits = c(18, 40),    
    oob = scales::squish,          
    breaks = c(20,40),
    name = "Incidence rate\n(per 100,000)"
  ) +
  
  scale_size_continuous(
    range = c(1, 7),      
    limits = c(18, 41),
    breaks = c(20,40),
    name = "Incidence rate\n(per 100,000)"
  ) +

  coord_fixed(ratio = 0.8) + 

  scale_x_discrete(expand = c(0, 0)) +
  scale_y_discrete(expand = c(0, 0)) +

  guides(
    fill = guide_legend(direction = "horizontal", title.position = "left", label.position = "bottom"),
    size = guide_legend(direction = "horizontal", title.position = "left", label.position = "bottom")
  ) +
  
  theme_minimal(base_size = 5.5) + 
  
  theme(
    panel.grid = element_blank(),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.2),
    panel.grid.major = element_line(color = "grey80", linewidth = 0.15), 
    axis.text.x = element_text(angle = 30,size=5.3,hjust = 0.5, vjust = 0.5, color = "black"),#angle = 30,
    axis.text.y = element_text(color = "black",size=5.3),
    axis.title = element_blank(),
    axis.ticks = element_blank(),
    legend.position = "top",
    legend.box = "vertical",
    legend.title = element_text(hjust = 0.5),
    legend.text = element_text(hjust = 0.5)
  )

####Fig. 1c####
library(readxl)
library(dplyr)
library(ggplot2)
library(data.table)
library(RColorBrewer)
library(lubridate)
library(scales)

df <- read_xlsx("大疫情报告2017-2024清洗后4.xlsx")
df$报告卡录入时间 <- as.Date(df$报告卡录入时间) 
setDT(df)

target_types <- c("本地病例", "省际迁移", "省内跨市", "市内跨县")

plot_data <- df %>%
  filter(!is.na(报告卡录入时间)) %>%
  mutate(Date = as.Date(报告卡录入时间)) %>%
  mutate(Month_Date = floor_date(Date, "month")) %>%
  group_by(Month_Date, 迁移类型) %>%
  summarise(case_count = n(), .groups = "drop") %>%
  group_by(Month_Date) %>%
  mutate(total_month_cases = sum(case_count),
         proportion = case_count / total_month_cases) %>%
  ungroup() %>%
  filter(迁移类型 %in% target_types) %>%
  mutate(迁移类型 = factor(迁移类型, levels = target_types))

p_1c <- ggplot(plot_data, aes(x = Month_Date, y = proportion, color = 迁移类型)) +
  geom_line(linewidth = 0.4) +
  scale_color_manual(values = c("本地病例" = "#E41A1C", 
                                "省际迁移" = "#377EB8", 
                                "省内跨市" = "#4DAF4A", 
                                "市内跨县" = "#984EA3")) +
  scale_x_date(date_breaks = "1 year", 
               date_labels = "%Y",
               expand = expansion(mult = c(0.02, 0.02))
  ) + 
  
  scale_y_continuous(
    breaks = c(0, 0.1,0.2, 0.3,0.4,0.5,0.6,0.7),
    labels = function(x) x * 100 
  )+

  labs(
    x = "Month",
    y = "Proportion (%)",
    color = "迁移类型"
  ) +

  theme_classic(base_size = 7) +
  theme(
    axis.line = element_line(color = "black", linewidth = 0.2), 
    axis.ticks = element_line(color = "black", linewidth = 0.2),
    axis.ticks.length = unit(0.1, "cm") ,
    axis.text.x = element_text(color = "black",size=7),
    axis.text.y = element_text(color = "black",size=7),
    
    legend.position = "none", 
    panel.grid.major.y = element_line(color = "grey90",linewidth = 0.1),
    panel.grid.major.x = element_line(color = "grey90",linewidth = 0.1), 
    panel.grid.minor = element_blank()
  )

####Fig. 1d####
library(readxl)
library(writexl)
library(tidyverse)
library(dplyr)
library(ggplot2)
library(gghalves)
library(ggside)
library(ggrastr)
library(patchwork)
# Read data
df1 <- read_xlsx("大疫情报告2017-2024清洗后4.xlsx")

df <- df1 %>% mutate(is_migrant = case_when(迁移类型 == "本地病例" ~ 1,
                                            TRUE ~ 0))
df_local <- df %>% 
  filter(is_migrant == 1) %>%
  filter(性别 %in% c("男", "女"))

df_local$报告年份 <- as.factor(df_local$报告年份)
df_local$性别 <- as.factor(df_local$性别)

summary_df <- df_local %>%
  group_by(报告年份, 性别) %>%
  summarise(
    median_age = median(年龄, na.rm = TRUE),
    q50_ymin = quantile(年龄, 0.25, na.rm = TRUE),
    q50_ymax = quantile(年龄, 0.75, na.rm = TRUE),
    .groups = 'drop'
  ) %>%
  mutate(x_numeric = as.numeric(报告年份))

master_df <- df_local %>%
  left_join(summary_df, by = c("报告年份", "性别")) %>%
  mutate(x_numeric = as.numeric(报告年份))

colors_male <- c("q50" = "#5CACEE")
colors_female <- c("q50" = "#FF69B4") 

width_50 <- 0.1
dodge_gap <- 0

# plot
p_1d <- ggplot(master_df, aes(x = 报告年份, y = 年龄)) +
  geom_rect(
    aes(
      ymin = q50_ymin, ymax = q50_ymax,
      xmin = ifelse(性别 == "男", x_numeric - width_50 - dodge_gap, x_numeric + dodge_gap),
      xmax = ifelse(性别 == "男", x_numeric - dodge_gap, x_numeric + width_50 + dodge_gap),
      fill = ifelse(性别 == "男", colors_male["q50"], colors_female["q50"])
    )
  ) +
  rasterise(
    geom_half_violin(
      data = . %>% filter(性别 == "男"),
      side = "l",
      scale = "area",
      color = NA,
      fill = "#009ACD",
      alpha = 0.2,
      position = position_dodge(0),
      width = 1,
      adjust = 1     
    ),
    dpi = 300
  ) +
  rasterise(
    geom_half_violin(
      data = . %>% filter(性别 == "女"),
      side = "r",
      scale = "area",
      color = NA,
      fill = "#FF1493",
      alpha = 0.2,
      position = position_dodge(0),
      width = 1,
      adjust = 1   
    ),
    dpi = 300
  ) +
  geom_point(
    data = master_df,
    aes(
      x = 报告年份,
      y = median_age,
      group = 性别 
    ),
    color = "black",
    shape = 16,
    size = 0.1,
    position = position_dodge(0.18) 
  ) +
  scale_fill_identity() + 
  
  scale_y_continuous(
    name = "Age (years)",
    limits = c(0, 102),
    breaks = c(0, 25, 50, 75, 100),
    expand = c(0,0)
  ) +
  
  scale_x_discrete(
    name = "Year"
  ) +
  
  labs(title = "Local cases",) + 
  theme_minimal(base_size = 6) +
  theme(
    plot.title = element_text(
      hjust = 0, 
      size = 6,   
      margin = margin(b = 1) 
    ),
    panel.grid.major.y = element_line(color = "grey90", linewidth = 0.1), 
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text.x = element_text(size = 5.5, margin = margin(t = -1)),
    axis.text.y = element_text(size = 5.5, margin = margin(r = -1)),
    axis.title.x = element_blank(),
    axis.title.y = element_text(size = 6, margin = margin(r = 0)),
    
    axis.line = element_line(color = "black", linewidth = 0.2), 
    axis.ticks = element_line(color = "black", linewidth = 0.2)
  )

