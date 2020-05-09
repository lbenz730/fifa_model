library(tidyverse)
library(gganimate)
library(RCurl)
library(egg)

df_stats <- read.csv("player_stats.csv", as.is = T) %>%
  as_tibble()

player_evolution <- function(player) {
  df <- filter(df_stats, name == player) %>%
    select(c(4,6,7, 12, 29:64)) %>%
    gather(attribute, value, -year, -headshot_url, -rating, -age) %>%
    mutate("type" = case_when(
      attribute %in% c("ball_control", "dribbling", "crossing", 
                       "short_pass", "long_pass", "marking", 
                       "slide_tackle", "stand_tackle") ~ "Ball Skills/Defense",
      attribute %in% c("aggression", "reactions", "att_position", "interceptions",
                       "vision", "composure", "creativity") ~ "Mental",
      attribute %in% c("acceleration", "stamina", "strength", "balance", 
                       "sprint_speed", "agility", "jumping") ~ "Physical",
      attribute %in% c("heading", "shot_power", "shot_accuracy", "free_kick_accuracy", 
                       "finishing", "long_shots", "curve", "penalties", "vollies") ~ "Shooting",
      grepl("gk", attribute) ~ "Goalkeeper",
      T ~ "Other"))
  
  df$headshot_url[df$attribute != "aggression"] <- NA
  df$grob_col <- "z"
  df$grob_col[df$attribute != "aggression"] <- NA
  
  
  render <- function(url) {
    if(is.na(url)) {
      return(grob())
    } else
      return(rasterGrob(readPNG(getURLContent(url)), width = unit(0.2, "npc"), height=unit(0.5, "npc")))
  }
  
  p <- ggplot(filter(df, type != "Goalkeeper"), aes(x = attribute, y = value)) +
    facet_wrap(~type, scales = "free_x") + 
    geom_col(aes(fill = value)) +
    scale_fill_gradientn(colors = RColorBrewer::brewer.pal(11, "RdYlGn")) +
    theme_bw() +
    theme(axis.title = element_text(size = 20),
          axis.text.x = element_text(size = 8, angle = 60, hjust = 1),
          plot.title = element_text(size = 24, hjust  = 0.5),
          plot.subtitle = element_text(size = 18, hjust = 0.5),
          strip.text = element_text(size = 12),
          legend.position = "none") + 
    scale_x_discrete(labels = function(x) ifelse(x == "z", "", tools::toTitleCase(gsub("_", " ", x))),
                     breaks = unique(df$attribute)) +
    scale_y_continuous(limits = c(0, 100)) +
    labs(x = "Attribute", 
         y = "Rating",
         title = paste("FIFA Player Evolution:", player),
         subtitle = "Season: {closest_state} | Age: {df$age[df$year == closest_state][1]} | Overall Rating: {df$rating[df$year== closest_state][1]}",
         caption = "Chart: Luke Benz (@recspecs730) | Data: fifaindex.com") +
    geom_custom(aes(x = grob_col, y = 80, data = headshot_url),
                grob_fun = function(url) render(url)) +
    transition_states(year,
                      transition_length = 3,
                      state_length = 2) 
  animate(p, fps = 5, end_pause = 25, height = 400, width = 400 * 16/9)
  anim_save(paste0("gifs/", gsub("\\s", "", player), ".gif"))
}

#player_evolution("Lionel Messi")
#player_evolution("Cristiano Ronaldo")

### Correlation Matrix
df <- select(df_stats, c("rating", "age", names(df_stats)[c(29:64)]))
df_cor <- expand.grid("var1" = colnames(df), "var2" = colnames(df), stringsAsFactors = F)

df_cor <- df_cor %>%
  filter(!str_detect(var1, "gk_"), !str_detect(var2, "gk_")) %>%
  mutate("correlation" = purrr::map2_dbl(var1, var2, function(x,y) cor(df_stats[[x]], df_stats[[y]], use = "na.or.complete")))

ggplot(df_cor, aes(x = var1, y = var2, fill = correlation)) + 
  geom_raster(alpha = 0.8) +
  theme_bw() +
  scale_fill_viridis_c(option = "B") +
  theme(axis.title = element_text(size = 20),
        axis.text.x = element_text(size = 8, angle = 90, hjust = 1),
        plot.title = element_text(size = 22, hjust  = 0.5),
        plot.subtitle = element_text(size = 18, hjust = 0.5)) +
  scale_x_discrete(labels = function(x) tools::toTitleCase(gsub("_", " ", x))) +
  scale_y_discrete(labels = function(x) tools::toTitleCase(gsub("_", " ", x))) +
  labs(x = "", y = "", fill = "Correlation", 
       title = "FIFA Player Attribute Correlation Matrix",
       subtitle = "Non Goalkeeper Attributes")
ggsave("graphics/correlation_matrix.png", height = 9/1.2, width = 16/1.2)
