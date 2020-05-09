library(tidyverse)
library(tidymodels)

### Load Data
df_stats <- read.csv("player_stats.csv", as.is = T) %>%
  as_tibble()

df <- select(df_stats, c("rating", "age", "preferred_positions", names(df_stats)[c(29:64)])) %>%
  mutate("preferred_position" = gsub("/.*", "", preferred_positions)) %>%
  select(-preferred_positions)


### Split Data
set.seed(12321)
data_split <- initial_split(df, 0.7)
train <- training(data_split)
test <- testing(data_split)
val_set <- validation_split(train, 0.8)

### Specify Random Forrest Model
rf_model <- 
  rand_forest(mtry = tune(), min_n = tune(), trees = tune()) %>% 
  set_engine("ranger", importance = "impurity", num.threads = 7) %>% 
  set_mode("regression")

rf_recipe <- 
  recipe(rating ~ ., data = df) %>%
  step_medianimpute(all_numeric(), -all_outcomes())

rf_workflow <- 
  workflow() %>%
  add_model(rf_model) %>%
  add_recipe(rf_recipe)

### Fit Model
results <- rf_workflow %>%
  tune_grid(val_set,
            grid = 50,
            control = control_grid(save_pred = F),
            metrics = metric_set(rmse))

#saveRDS(results , "results.rds")
#results <- read_rds("results.rds")

final_wf <- 
  rf_workflow %>% 
  finalize_workflow(select_best(results, "rmse"))

fifa_rf <-
  final_wf %>%
  fit(data = train) 

variable_importance <- fifa_rf %>% 
  pull_workflow_fit() %>%
  vip::vi()

ggplot(variable_importance, aes(x = fct_reorder(Variable, Importance), y = Importance)) +
  geom_col(fill = "forestgreen") + 
  coord_flip() +
  theme_bw() +
  theme(axis.title = element_text(size = 20),
        plot.title = element_text(size = 22, hjust  = 0.5),
        plot.subtitle = element_text(size = 18, hjust = 0.5)) +
  scale_x_discrete(labels = function(x) tools::toTitleCase(gsub("_", " ", x))) +
  labs(x = "Attribute", y = "Variable Importance Score", 
       fill = "FIFA Player Attribute Variable Importance Plot", 
       title = "FIFA Player Attribute Correlation Matrix",
       subtitle = "Random Forest Model")
ggsave("graphics/vip.png", height = 9/1.2, width = 16/1.2)
