library(mlr3verse)
library(mlr3tuning)
library(readxl)
library(writexl)
library(dplyr)
library(R6)
library(explainer)
library(iml)
library(mlr3pipelines)
library(shapviz)
library(ggplot2)
library(future)
library(patchwork)
library(ggExtra)
library(data.table)
####city-level####
#####data preparation#####
df<- read_xlsx("XGBoost变量市级.xlsx")
df_weight <- read_xlsx("edge_city1823final.xlsx")
df <- df %>% left_join(
  df_weight %>% select("sourcecode","targetcode","weight"),by=c("sourcecode","targetcode")
)
df_processed <- subset(df, select = c(Dis,GA,weight,DD,Signal,
                                      Pop_O,Bed_O,NTL_O,Access_O,TND_O,GRP_O,PIP_O,IR_O,
                                      Pop_D,Bed_D,NTL_D,Access_D,TND_D,GRP_D,PIP_D
))
df_processed$weight <- as.numeric(df_processed$weight)
df_processed2 <- df_processed %>% select(-c("TND_O","TND_D","GRP_O","GRP_D","PIP_O","PIP_D"))
# vif
library(car)
model_vif <- lm(weight ~ ., data = df_processed2)
vif_values <- vif(model_vif)
vif_df <- data.frame(
  Variable = names(vif_values),
  VIF = as.numeric(vif_values)
) %>%
  arrange(desc(VIF)) 
print(vif_df)

# Variable      VIF
# 1    Signal 2.925234
# 2       Dis 2.890490
# 3     Pop_O 2.423152
# 4        DD 2.302916
# 5        GA 2.226580
# 6     NTL_O 2.217855
# 7     NTL_D 2.190718
# 8  Access_O 2.092318
# 9     Pop_D 2.002101
# 10     IR_O 2.000051
# 11    Bed_D 1.446322
# 12    Bed_O 1.446157
# 13 Access_D 1.401733


#####model building#####
set.seed(123)
task <- as_task_regr(df_processed2, target = "weight", id = "weight_prediction")

learner <- lrn("regr.xgboost",verbose = 1)

# cpc
measure_cpc_fixed <- R6Class("MeasureCPC_Fixed",
                             inherit = MeasureRegr,
                             public = list(
                               initialize = function() {
                                 super$initialize(
                                   id = "regr.cpc",
                                   range = c(0, 1),
                                   minimize = FALSE,
                                   predict_type = "response"
                                 )
                               }
                             ),
                             
                             private = list(
                               .score = function(prediction, task, ...) {
                                 truth <- as.numeric(prediction$truth)
                                 response <- as.numeric(prediction$response)
                                 if (length(truth) != length(response)) {
                                   stop("Truth and prediction have different lengths")
                                 }
                                 numerator <- 2 * sum(pmin(truth, response, na.rm = TRUE))
                                 denominator <- sum(truth, na.rm = TRUE) + sum(response, na.rm = TRUE)
                                 if (denominator == 0) return(1)
                                 cpc <- numerator / denominator
                                 return(cpc)
                               }
                             )
)

mlr_measures$add("regr.cpc", measure_cpc_fixed)

learner$param_set$values <- list(
  objective = "reg:tweedie",
  booster = "gbtree",
  eval_metric = "rmsle"
)

# define search space
search_space <- ps(
  max_depth = p_int(lower = 2, upper = 6),
  eta = p_dbl(lower = 0.01, upper = 0.2, logscale = TRUE),
  gamma = p_dbl(0.1, 5, logscale = TRUE),
  subsample = p_dbl(lower = 0.4, upper = 0.8),
  colsample_bytree = p_dbl(lower = 0.4, upper = 0.8),
  tweedie_variance_power = p_dbl(lower = 1, upper = 1.4),
  min_child_weight = p_int(lower = 40, upper = 80),
  lambda = p_dbl(lower = 1, upper = 10, logscale = TRUE),
  alpha = p_dbl(lower = 0.1, upper = 5, logscale = TRUE),
  nrounds = p_int(lower = 200, upper =800)
)

# Resampling:5-cv
inner_resampling <- rsmp("repeated_cv", folds = 5)

measure <- list(
  msr("regr.mae"),
  msr("regr.rmsle"),
  msr("regr.rsq"),
  msr("regr.srho"),
  msr("regr.cpc")
)

evals_terminator <- trm("evals", n_evals =300)
stagnation_terminator <- trm("stagnation", iters = 20, threshold = 1e-4)
terminator <- trm("combo", terminators = list(evals_terminator, stagnation_terminator))

tuner <- tnr("mbo")

at <- AutoTuner$new(
  learner = learner,
  resampling = inner_resampling,
  measure = msr("regr.rmsle"),
  search_space = search_space,
  terminator = terminator,
  tuner = tuner,
  store_tuning_instance = TRUE
)

# train:test=7:3
set.seed(1234)
split <- partition(task, ratio = 0.7)

at$train(task, row_ids = split$train)

# optimal hyperparameters
best_params_found <- at$archive$best()$x_domain
print(best_params_found)
# performance on the train set
predictions_on_train <- at$predict(task, row_ids = split$train)
performance_on_train <- predictions_on_train$score(measure)
performance_on_train
# performance on the test set
predictions_on_test <- at$predict(task, row_ids = split$test)
performance_on_test <- predictions_on_test$score(measure)
performance_on_test


#####SHAP#####
xgb_model <- at$model$learner$model

features_for_shap <- data.matrix(
  task$data()[, .SD, .SDcols = -task$target_names]
)

shap_matrix <- predict(
  xgb_model, 
  newdata = features_for_shap, 
  predcontrib = TRUE
)

shap_values <- shap_matrix[, -ncol(shap_matrix)]
baseline    <- shap_matrix[1, ncol(shap_matrix)] 

sv <- shapviz(
  object = shap_values,
  X = task$data()[, .SD, .SDcols = -task$target_names],
  baseline = baseline
)

new_names <- colnames(sv)
new_names[new_names == "Signal"] <- "HBI"
new_names[new_names == "DD"]     <- "CDD"
new_names[new_names == "GA"]     <- "Adj"

new_names[new_names == "Access_D"] <- "Acc (D)"
new_names[new_names == "Access_O"] <- "Acc (O)"

new_names <- gsub("_D$", " (D)", new_names)
new_names <- gsub("_O$", " (O)", new_names)

colnames(sv) <- new_names


######Fig. 4a#####
library(ggplot2)
library(ggbeeswarm)
library(dplyr)
library(tidyr)

shap_data <- as.data.frame(sv$S)
feature_data <- as.data.frame(sv$X)

importance_df1 <- data.frame(
  feature = colnames(shap_data),
  importance = colMeans(abs(shap_data))
) %>%
  arrange(importance) %>% 
  mutate(feature = factor(feature, levels = feature))

feature_data_norm <- feature_data %>%
  mutate(across(everything(), ~ (. - min(., na.rm = TRUE)) / 
                  (max(., na.rm = TRUE) - min(., na.rm = TRUE))))

shap_long <- shap_data %>%
  mutate(id = row_number()) %>%
  pivot_longer(-id, names_to = "feature", values_to = "shap_value")

feature_long <- feature_data_norm %>%
  mutate(id = row_number()) %>%
  pivot_longer(-id, names_to = "feature", values_to = "feature_value")

plot_data <- left_join(shap_long, feature_long, by = c("id", "feature"))
plot_data$feature <- factor(plot_data$feature, levels = levels(importance_df1$feature))
plot_min_x1 <- -1.5 
max_shap_val <- max(plot_data$shap_value)
plot_max_x1 <- max(max_shap_val, 0.8)
plot_range <- plot_max_x1 - plot_min_x1
max_imp <- max(importance_df1$importance)
scale_factor <- plot_range / max_imp * 0.95
importance_df1 <- importance_df1 %>%
  mutate(
    xmin_scaled1 = plot_min_x1, 
    xmax_scaled1= plot_min_x1 + (importance * scale_factor)
  )

# plot
p_4a <- ggplot() +
  geom_rect(data = importance_df1,
            aes(xmin = xmin_scaled1, 
                xmax = xmax_scaled1, 
                ymin = as.numeric(feature) - 0.35, 
                ymax = as.numeric(feature) + 0.35),
            fill = "#17becf",
            alpha = 0.3) +

  geom_vline(xintercept = 0, color = "gray60", linetype = "solid",linewidth = 0.3) +

  rasterise(
    geom_quasirandom(data = plot_data, 
                     aes(x = shap_value, y = feature, color = feature_value),
                     groupOnX = FALSE, 
                     size = 0.8, 
                     alpha = 0.8,
                     varwidth = TRUE),
    dpi = 600 
  ) +

  scale_color_gradientn(
    colors = c("#2A004E", "#8B246D", "#D45E36", "#FFC300"),
    name = "Feature value",
    breaks = c(0, 1), 
    labels = c("Low", "High")
  ) +

  scale_x_continuous(
    name = "SHAP value contribution (beeswarm)",
    limits = c(plot_min_x1, plot_max_x1),
    breaks = c(-1.5,-1,-0.5,0,0.5), 
    sec.axis = sec_axis(~ (. - plot_min_x1) / scale_factor, 
                        name = "Mean SHAP value (feature importance)")
  ) +
  
  labs(y = "Feature") +

  theme_bw(base_size = 7.7) +
  
  theme(
    panel.grid.major.x = element_blank(), 
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_line(color = "gray90", linewidth = 0.3, linetype = "dashed"),
    panel.grid.minor.y = element_blank(),
    
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.3),
    axis.title.x.top = element_text(vjust = 2),
    axis.title.x.bottom = element_text(vjust = -1),
    
    axis.ticks = element_line(linewidth = 0.2), 
    
    legend.position = "right",
    legend.title = element_text(angle = 90, hjust = 0.5),
    legend.title.align = 0.5,
    legend.ticks = element_blank()
  ) +
  guides(color = guide_colorbar(
    title.position = "left", 
    barheight = unit(5, "cm"), 
    barwidth = unit(0.3, "cm")
  ))



#####Fig. S9a#####
library(ggplot2)
library(ggExtra)
library(data.table)
pred_train <- at$predict(task, row_ids = split$train)
pred_test <- at$predict(task, row_ids = split$test)

trans_fun <- function(x) log(x + 1)

df_plot <- rbind(
  data.table(
    obs = trans_fun(pred_train$truth),
    pred = trans_fun(pred_train$response),
    set = "Training set"
  ),
  data.table(
    obs = trans_fun(pred_test$truth),
    pred = trans_fun(pred_test$response),
    set = "Test set"
  )
)

raw_breaks <- c(0, 10, 100, 400)
log_breaks <- trans_fun(raw_breaks)
labels_axis <- as.character(raw_breaks)

make_label <- function(perf_vec) {
  sprintf(
    "R²: %.3f\nRMSLE: %.3f\nSpearman's rho: %.3f\nCPC: %.3f", 
    perf_vec["regr.rsq"],    
    perf_vec["regr.rmsle"],  
    perf_vec["regr.srho"],
    perf_vec["regr.cpc"]     
  )
}

label_train <- make_label(performance_on_train)
label_test <- make_label(performance_on_test)

d_min <- min(c(df_plot$obs, df_plot$pred))
d_max <- max(c(df_plot$obs, df_plot$pred))
d_range <- d_max - d_min

limit_min <- trans_fun(0)
limit_max <- trans_fun(420) 

box_w <- d_range * 0.28 
box_h <- d_range * 0.22

train_x <- limit_min + d_range * 0.05
train_y <- limit_max - d_range * 0.30

test_x <- limit_max - d_range * 0.38
test_y <- limit_min + d_range * 0.05

col_train <- "#6996D8"
col_test <- "#F57878"

# plot
p_S9a <- ggplot(df_plot, aes(x = obs, y = pred, color = set, fill = set)) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "grey50", size = 0.3) +

  geom_point(alpha = 0.6, size = 1, stroke = 0) +
  
  annotate("rect", xmin = train_x, xmax = train_x + box_w, ymin = train_y, ymax = train_y + box_h, 
           fill = "white", color = "black", linetype = "dashed", alpha = 0.6, size=0.3) +
  annotate("text", x = train_x + box_w*0.5, y = train_y + box_h*0.85, label = "Training set", 
           fontface="bold", size=2.4, color = col_train) +
  annotate("text", x = train_x + box_w*0.1, y = train_y + box_h*0.4, label = label_train, 
           hjust = 0, color = "black", lineheight = 1.2, size=2.4) +
  annotate("rect", xmin = test_x, xmax = test_x + box_w, ymin = test_y, ymax = test_y + box_h, 
           fill = "white", color = "black", linetype = "dashed", alpha = 0.6, size=0.3) +

  annotate("text", x = test_x + box_w*0.5, y = test_y + box_h*0.85, label = "Test set", 
           fontface="bold", size=2.4, color = col_test) +

  annotate("text", x = test_x + box_w*0.1, y = test_y + box_h*0.4, label = label_test, 
           hjust = 0, color = "black", lineheight = 1.2, size=2.4) +
  
  scale_x_continuous(breaks = log_breaks, labels = labels_axis, limits = c(limit_min, limit_max)) +
  scale_y_continuous(breaks = log_breaks, labels = labels_axis, limits = c(limit_min, limit_max)) +
  
  labs(x = "Observed flow", y = "Predicted flow") +
  
  scale_color_manual(values = c("Training set" = col_train, "Test set" = col_test)) +
  scale_fill_manual(values = c("Training set" = col_train, "Test set" = col_test)) +
  
  theme_bw(base_size = 8) +
  theme(
    legend.position = "none",
    panel.grid = element_blank(),
    axis.text = element_text(color = "black"),
    axis.title = element_text(),
    panel.border = element_rect(colour = "black", fill=NA, linewidth=0.3)
  )




#####Fig. 4c-d; Fig.S10#####
library(ggplot2)
library(patchwork)
library(shapviz)
library(scales) 

imp_scores <- sv_importance(sv, kind = "no")
ordered_vars <- names(imp_scores)
p_list <- sv_dependence(sv, 
                        v = ordered_vars, 
                        color_var = NULL, 
                        size = 0.5,    
                        alpha = 0.5)
p_list_mod <- lapply(seq_along(p_list), function(i) {
  var_name_str <- ordered_vars[i]
  p <- p_list[[i]]
  p$data$.facet_label <- var_name_str
  p_base <- p +
    geom_hline(yintercept = 0, color = "gray50", linewidth = 0.3, linetype = "dashed") + 
    facet_wrap(~ .facet_label) + 
    labs(title = NULL, x = NULL) +
    theme_bw(base_size = 6) +
    theme(
      strip.background = element_rect(fill = "gray90", color = "black"), 
      strip.text = element_text(size = 6), 
      axis.title.y = element_text(size = 6,margin = margin(r = -1)), 
      axis.text.y = element_text(size = 6,margin = margin(r = -0.5)), 
      axis.text.x = element_text(size = 6,margin = margin(t = -0.5)),
      axis.ticks.length = unit(0.1, "cm"), 
      panel.grid = element_blank(), 
      plot.margin = margin(1, 1, 1, 1)
    )
  
  if (var_name_str == "HBI") {
    p_base <- p_base + 
      scale_x_continuous(labels = label_number(use_grouping = FALSE),n.breaks=3)
  } else {
    p_base <- p_base + 
      scale_x_continuous(labels = scales::label_number(use_grouping = FALSE))
  }
  
  return(p_base)
})

final_plots <- wrap_plots(p_list_mod, ncol = 5)





####city-level####
#####data preparation#####
df<- read_xlsx("XGBoost变量县级.xlsx")
df_weight <- read_xlsx("edge_county1823final.xlsx")
df <- df %>% left_join(
  df_weight %>% select("sourcecode","targetcode","weight"),by=c("sourcecode","targetcode")
)
df_processed <- subset(df, select = c(Dis,GA,weight,DD,Signal,sourcecode,
                                      Pop_O,Bed_O,NTL_O,Access_O,TND_O,PIP_O,IR_O,
                                      Pop_D,Bed_D,NTL_D,Access_D,TND_D,PIP_D
))
df_processed$weight <- as.numeric(df_processed$weight)
df_processed2 <- df_processed %>% select(-c("TND_O","TND_D","PIP_O","PIP_D"))
#vif
library(car)
model_vif <- lm(weight ~ ., data = df_processed2)
# 计算 VIF
vif_values <- vif(model_vif)

vif_df <- data.frame(
  Variable = names(vif_values),
  VIF = as.numeric(vif_values)
) %>%
  arrange(desc(VIF)) 
print(vif_df)
# write_xlsx(vif_df,"vif_county.xlsx")
# Variable      VIF
# 1  Access_O 1.981482
# 2  Access_D 1.974614
# 3     NTL_O 1.871170
# 4     NTL_D 1.845150
# 5       Dis 1.535046
# 6        GA 1.422081
# 7        DD 1.404753
# 8    Signal 1.366540
# 9     Bed_O 1.271690
# 10    Bed_D 1.269990
# 11    Pop_O 1.167129
# 12     IR_O 1.138185
# 13    Pop_D 1.095333

task = as_task_regr(df_processed2, target = "weight")
task$set_col_roles("sourcecode", roles = "group")
task$set_col_roles(c("sourcecode"), remove_from = "feature")

set.seed(1234)
split = partition(task, ratio = 0.7)

measure_cpc_fixed <- readRDS("measure_cpc_fixed.rds")
mlr_measures$add("regr.cpc", measure_cpc_fixed)

measure <- list(
  msr("regr.mae"),
  msr("regr.rmsle"),
  msr("regr.rsq"),
  msr("regr.srho"),
  msr("regr.cpc")
)

learner = lrn("regr.xgboost",verbose = 1)

learner$param_set$values = list(
  objective = "reg:tweedie",
  booster = "gbtree",
  eval_metric = "rmsle"
)

search_space <- ps(
  max_depth = p_int(lower = 11, upper = 13),
  eta = p_dbl(lower = 0.015, upper = 0.03, logscale = TRUE),
  gamma = p_dbl(lower = 0, upper = 0.5),
  subsample = p_dbl(lower = 0.4, upper = 0.5),
  colsample_bytree = p_dbl(lower = 0.8, upper = 0.95),
  tweedie_variance_power = p_dbl(lower = 1.4, upper = 1.5),
  min_child_weight = p_int(lower = 40, upper = 60),
  lambda = p_dbl(lower =3, upper = 20, logscale = TRUE),
  alpha = p_dbl(lower = 0.1, upper = 5, logscale = TRUE),
  nrounds = p_int(lower = 1600, upper =2000)
)

evals_terminator <- trm("evals", n_evals = 300)

stagnation_terminator <- trm("stagnation", iters = 60, threshold = 1e-4)#早停

terminator <- trm("combo", terminators = list(evals_terminator, stagnation_terminator))

inner_resampling <- rsmp("cv", folds = 10)

at = AutoTuner$new(
  learner = learner,
  resampling = inner_resampling, 
  measure = msr("regr.rmsle"),
  search_space = search_space,
  terminator = terminator,
  tuner = tnr("mbo")
)

at$train(task, row_ids = split$train)
# optimal hyperparameters
best_params_found <- at$archive$best()$x_domain
print(best_params_found)
# performance on the train set
predictions_on_train <- at$predict(task, row_ids = split$train)
performance_on_train <- predictions_on_train$score(measure)
performance_on_train
# performance on the test set
predictions_on_test <- at$predict(task, row_ids = split$test)
performance_on_test <- predictions_on_test$score(measure)
performance_on_test

# County-level mapping codes omitted.












