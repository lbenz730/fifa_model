library(tidyverse)
library(stringr)
library(furrr)
plan(multiprocess)


get_player_links <- function(season, page_num) {
  url <- paste0("https://www.fifaindex.com/players/fifa", season, "/", page_num)
  html <- suppressWarnings(try(scan(url, what = "", sep = "\n")))
  
  if(class(html) == 'try-error') {
    return(NULL) 
  }
  
  ### Player Specific Information
  html <- html[grep("data-playerid", html)]
  
  player_ids <- as.numeric(str_extract(html, "\\d+"))
  player_links <- gsub("<a href=\"", "", gsub("\" title=\".*", "", gsub(".*<figure class=\"player\">", "", html)))
  
  df <- tibble("player_id" = player_ids,
               "link" = player_links,
               "season" = season)
  return(df)
}

get_player_stats <- function(player_id, link, season) {
  url <- paste0("https://www.fifaindex.com", link)
  html <- suppressWarnings(try(scan(url, what = "", sep = "\n")))
  
  if(class(html) == 'try-error') {
    return(NULL) 
  }
  
  ####### Compile Player Attributes
  ### Initial Player Info
  ix <- which(str_detect(html, "badge-dark rating"))[1]
  player_name <- gsub(".*card-header\">", "", gsub("<span.*", "", html[ix]))
  player_country <- gsub("title=\"|\"\\s.*", "", str_extract(html[which(str_detect(html, "link-nation"))], "title=\".*\""))
  player_rating <- str_extract(html[ix], "\\d\\d")
  player_height <- as.numeric(str_extract(html[ix + 4], "\\d+"))
  player_weight <- as.numeric(str_extract(html[ix + 5], "\\d+"))
  preferred_foot <- str_extract(html[ix + 6], "Left|Right")
  player_age <- as.numeric(str_extract(html[ix + 9], "\\d+"))
  player_birthdate <- as.Date(gsub("Sept", "Sep", na.exclude(str_extract(html[ix + 7], paste0(c("Sept",month.abb), "\\. \\d+, \\d+")))) , "%b. %d, %Y")
  if(length(player_birthdate) == 0) {
    player_birthdate <- as.Date(na.exclude(str_extract(html[ix + 7], paste0(month.name, " \\d+, \\d+"))) , "%B %d, %Y")
  }
  if(length(player_birthdate) == 0) {
    player_birthdate <- NA
  }
  preferred_positions <- list(gsub("\".*", "", gsub("=\"", "", unlist(str_split(html[ix + 11], "title"))[-1])))
  player_work_rate_off <- unlist(str_extract_all(html[ix + 12], "Low|Medium|High"))[1]
  player_work_rate_def <- unlist(str_extract_all(html[ix + 12], "Low|Medium|High"))[2]
  weak_foot <- str_count(html[ix + 13], "fas fa-star")
  skill_moves <- str_count(html[ix + 14], "fas fa-star")
  value <- as.numeric(gsub("\\.", "", str_extract(html[ix + 17], "\\d+\\.*\\d*\\.*\\d*")))
  wage <- as.numeric(gsub("\\.", "", str_extract(html[ix + 18], "\\d+\\.*\\d*")))
  
  ### National Team Info
  ix_country <- ix + which(str_detect(html[-c(1:ix)], "card-header") & str_detect(html[-c(1:ix)],  paste0("Côte d'Ivoire|Holland|", player_country)))
  if(length(ix_country) > 0) {
    country_kit_number <- as.numeric(str_extract(html[ix_country + 4], "\\d+"))
    country_position <- gsub(">|<.*", "", str_extract(html[ix_country + 3], ">[A-z]*</span>"))
  } else {
    country_kit_number <- NA
    country_position <- NA
  }
  
  ### Club Team Info
  ix_club <- which(str_detect(html, "card-header") &  str_detect(html, "class=\"link-team\"") & !str_detect(html, paste0("Côte d'Ivoire|Holland|", player_country)))
  player_club <- gsub("\\sFIFA.*", "", gsub("title=\"|\"\\s.*", "", str_extract(html[ix_club], "title=\".*\"")))
  club_kit_number <- as.numeric(str_extract(html[ix_club + 4], "\\d+"))
  club_position <- gsub(">|<.*", "", str_extract(html[ix_club + 3], ">[A-z]*</span>"))
  
  loan_flag <- any(grepl("On loan", html))
  if(loan_flag) {
    loaned_from <- gsub("\\sFIFA.*", "", gsub("title=\"|\"\\s.*", "", str_extract(html[ix_club + 7], "title=\".*\"")))
    joined_club <- as.Date(na.exclude(str_extract(html[ix_club + 9], paste0(c("Sept",month.abb), "\\. \\d+, \\d+"))) , "%b. %d, %Y")
    if(length(joined_club) == 0) {
      joined_club <- as.Date(na.exclude(str_extract(html[ix_club + 9], paste0(month.name, " \\d+, \\d+"))) , "%B %d, %Y")
    }
    contract_length <- as.numeric(str_extract(html[ix_club + 10], "\\d+"))
    
  } else {
    loaned_from <- NA
    joined_club <- as.Date(gsub("Sept", "Sep", na.exclude(str_extract(html[ix_club + 6], paste0(c("Sept",month.abb), "\\. \\d+, \\d+")))) , "%b. %d, %Y")
    if(length(joined_club) == 0) {
      joined_club <- as.Date(gsub("Sept", "Sep", na.exclude(str_extract(html[ix_club + 6], paste0(month.name, " \\d+, \\d+")))) , "%B %d, %Y")
    }
    contract_length <- as.numeric(str_extract(html[ix_club + 7], "\\d+"))
  }
  
  if(length(joined_club) == 0) {
    joined_club <- NA
  }
  
  ### Skill Attributes
  ix_skills <- max(grep("Ball Skills", html))
  
  # Ball Skills
  ball_control <- as.numeric(gsub(">", "",  str_extract(html[ix_skills + 3], ">\\d+")))
  dribbling <- as.numeric(gsub(">", "",  str_extract(html[ix_skills + 5], ">\\d+")))
  
  # Defense
  marking <- as.numeric(gsub(">", "",  str_extract(html[ix_skills + 16], ">\\d+")))
  slide_tackle <- as.numeric(gsub(">", "",  str_extract(html[ix_skills + 18], ">\\d+")))
  if(season >= 11) {
    standing_tackle <- as.numeric(gsub(">", "",  str_extract(html[ix_skills + 20], ">\\d+")))
    offset <- 0
  } else {
    standing_tackle <- NA
    offset <- 2
  }
  
  # Mental
  if(season > 16) {
    aggression <- as.numeric(gsub(">", "",  str_extract(html[ix_skills + 31 - offset], ">\\d+")))
    reactions <- as.numeric(gsub(">", "",  str_extract(html[ix_skills + 33 - offset], ">\\d+")))
    att_position <- as.numeric(gsub(">", "",  str_extract(html[ix_skills + 35 - offset], ">\\d+")))
    interceptions <- as.numeric(gsub(">", "",  str_extract(html[ix_skills + 37 - offset], ">\\d+")))
    vision <- as.numeric(gsub(">", "",  str_extract(html[ix_skills + 39 - offset], ">\\d+")))
    composure <- as.numeric(gsub(">", "",  str_extract(html[ix_skills + 41], ">\\d+")))
  } else if(season < 11) {
    aggression <- as.numeric(gsub(">", "",  str_extract(html[ix_skills + 31 - offset], ">\\d+")))
    composure <- as.numeric(gsub(">", "",  str_extract(html[ix_skills + 33 - offset], ">\\d+")))
    reactions <- as.numeric(gsub(">", "",  str_extract(html[ix_skills + 35 - offset], ">\\d+")))
    vision <- NA
    interceptions <- NA
    att_position <- NA
    offset <- offset + 6
  } else {
    aggression <- as.numeric(gsub(">", "",  str_extract(html[ix_skills + 31 - offset], ">\\d+")))
    reactions <- as.numeric(gsub(">", "",  str_extract(html[ix_skills + 33 - offset], ">\\d+")))
    att_position <- as.numeric(gsub(">", "",  str_extract(html[ix_skills + 35 - offset], ">\\d+")))
    interceptions <- as.numeric(gsub(">", "",  str_extract(html[ix_skills + 37 - offset], ">\\d+")))
    vision <- as.numeric(gsub(">", "",  str_extract(html[ix_skills + 39 - offset], ">\\d+")))
    composure <- NA
    offset <- offset + 2
  }
  
  # Passing
  crossing <- as.numeric(gsub(">", "",  str_extract(html[ix_skills + 52 - offset], ">\\d+")))
  short_pass <- as.numeric(gsub(">", "",  str_extract(html[ix_skills + 54 - offset], ">\\d+")))
  long_pass <- as.numeric(gsub(">", "",  str_extract(html[ix_skills + 56 - offset], ">\\d+")))
  
  # Physical
  acceleration <- as.numeric(gsub(">", "",  str_extract(html[ix_skills + 67 - offset], ">\\d+")))
  stamina <- as.numeric(gsub(">", "",  str_extract(html[ix_skills + 69 - offset], ">\\d+")))
  strength <- as.numeric(gsub(">", "",  str_extract(html[ix_skills + 71 - offset], ">\\d+")))
  if(season >= 11) {
    balance <- as.numeric(gsub(">", "",  str_extract(html[ix_skills + 73 - offset], ">\\d+")))
    sprint_speed <- as.numeric(gsub(">", "",  str_extract(html[ix_skills + 75 - offset], ">\\d+")))
    agility <- as.numeric(gsub(">", "",  str_extract(html[ix_skills + 77 - offset], ">\\d+")))
    jumping <- as.numeric(gsub(">", "",  str_extract(html[ix_skills + 79 - offset], ">\\d+")))
  } else{
    sprint_speed <- as.numeric(gsub(">", "",  str_extract(html[ix_skills + 73 - offset], ">\\d+")))
    balance <- NA
    agility <- NA
    jumping <- NA
    offset <- offset + 6
  }
  
  # Shooting 
  heading <- as.numeric(gsub(">", "",  str_extract(html[ix_skills + 90 - offset], ">\\d+")))
  shot_power <- as.numeric(gsub(">", "",  str_extract(html[ix_skills + 92 - offset], ">\\d+")))
  finishing <- as.numeric(gsub(">", "",  str_extract(html[ix_skills + 94 - offset], ">\\d+")))
  long_shots <- as.numeric(gsub(">", "",  str_extract(html[ix_skills + 96 - offset], ">\\d+")))
  
  if(season >= 11) {
    curve <- as.numeric(gsub(">", "",  str_extract(html[ix_skills + 98 - offset], ">\\d+")))
    penalties <- as.numeric(gsub(">", "",  str_extract(html[ix_skills + 102 - offset], ">\\d+")))
    vollies <- as.numeric(gsub(">", "",  str_extract(html[ix_skills + 104 - offset], ">\\d+")))
    free_kick_accuracy <- as.numeric(gsub(">", "",  str_extract(html[ix_skills + 100 - offset], ">\\d+")))
  } else {
    free_kick_accuracy <- as.numeric(gsub(">", "",  str_extract(html[ix_skills + 98 - offset], ">\\d+")))
    curve <- NA
    vollies <- NA
    penalties <- NA
    offset <- offset + 6
  }
  

  # Goalkeeper
  if(season >= 11) {
    gk_positioning <- as.numeric(gsub(">", "",  str_extract(html[ix_skills + 115 - offset], ">\\d+")))
    gk_diving <- as.numeric(gsub(">", "",  str_extract(html[ix_skills + 117 - offset], ">\\d+")))
    gk_handling <- as.numeric(gsub(">", "",  str_extract(html[ix_skills + 119 - offset], ">\\d+")))
    gk_kicking <- as.numeric(gsub(">", "",  str_extract(html[ix_skills + 121 - offset], ">\\d+")))
    gk_reflexes <- as.numeric(gsub(">", "",  str_extract(html[ix_skills + 123 - offset], ">\\d+")))
  } else {
    gk_reflexes <- as.numeric(gsub(">", "",  str_extract(html[ix_skills + 115 - offset], ">\\d+")))
    gk_handling <- as.numeric(gsub(">", "",  str_extract(html[ix_skills + 117 - offset], ">\\d+")))
    gk_positioning <- as.numeric(gsub(">", "",  str_extract(html[ix_skills + 119 - offset], ">\\d+")))
    gk_diving <- as.numeric(gsub(">", "",  str_extract(html[ix_skills + 121 - offset], ">\\d+")))
    gk_kicking <- NA
  }
  
  df <- tibble("player_id" = player_id,
               "name" = player_name,
               "season" = season,
               "link" = link,
               "rating" = player_rating,
               "club" = player_club,
               "nationality" = player_country,
               "height" = player_height,
               "weight" = player_weight,
               "age" = player_age,
               "birthdate" = player_birthdate,
               "loaned_from" = loaned_from,
               "preferred_foot" = preferred_foot,
               "preferred_positions" = preferred_positions,
               "player_work_rate_off" = player_work_rate_off,
               "player_work_rate_def" = player_work_rate_def,
               "weak_foot" = weak_foot,
               "skill_moves" = skill_moves,
               "value" = value,
               "wage" = wage,
               "country_kit_number" = country_kit_number,
               "country_position" = country_position,
               "club_kit_number" = club_kit_number,
               "club_position" = club_position,
               "joined_club" = joined_club,
               "contract_length" = contract_length,
               "ball_control" = ball_control,
               "dribbling" = dribbling,
               "marking" = marking,
               "slide_tackle" = slide_tackle,
               "stand_tackle" = standing_tackle,
               "agression" = aggression,
               "reactions" = reactions,
               "att_position" = att_position,
               "interceptions" = interceptions,
               "vision" = vision,
               "composure" = composure,
               "crossing" = crossing,
               "short_pass" = short_pass,
               "long_pass" = long_pass,
               "acceleration" = acceleration,
               "stamina" = stamina,
               "strength" = strength,
               "balance" = balance,
               "sprint_speed" = sprint_speed,
               "agility" = agility,
               "jumping" = jumping,
               "heading" = heading,
               "shot_power" = shot_power,
               "finishing" = finishing,
               "long_shots" = long_shots,
               "curve" = curve,
               "free_kick_accuracy" = free_kick_accuracy,
               "penalties" = penalties,
               "gk_positioning" = gk_positioning,
               "gk_diving" = gk_diving,
               "gk_handling" = gk_handling,
               "gk_kicking" = gk_kicking,
               "gk_reflexes" = gk_reflexes)
  
  return(df)
}

season <- 7:20
df_links <- future_map_dfr(season, function(s) future_map2_dfr(rep(s, 1), 1, get_player_links))
df_stats <- df_links %>%
  as.list() %>%
  future_pmap_dfr(get_player_stats)

anti_join(df_links, df_stats)

player_id <- df_links$player_id[12]
link <- df_links$link[12]
season <- df_links$season[12]

### Stats for 05 Season
### Debug something about 06 player links
