library(tidyverse)
library(stringr)
library(furrr)
plan(multiprocess(workers = parallel::detectCores() - 1))


get_player_links <- function(season, page_num) {
  url <- paste0("https://www.fifaindex.com/players/fifa", season, "/", page_num)
  html <- suppressWarnings(try(scan(url, what = "", sep = "\n")))
  
  if(class(html) == 'try-error') {
    return(NULL) 
  }
  
  ### Player Specific Information
  html <- suppressWarnings(html[grep("data-playerid", html)])
  
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
  country_string <- paste0(c("Côte d'Ivoire", "Holland", "Republic Of Ireland", ""), collapse = "|")
  
  if(class(html) == 'try-error') {
    return(NULL) 
  }
  
  ####### Compile Player Attributes
  ### Initial Player Info
  ix <- which(str_detect(html, "badge-dark rating"))[1]
  player_name <- gsub(".*card-header\">", "", gsub("<span.*", "", html[ix]))
  player_country <- gsub("title=\"|\"\\s.*", "", str_extract(html[which(str_detect(html, "link-nation"))], "title=\".*\""))
  img_html <- html[str_detect(html, player_name) & str_detect(html, "png") & str_detect(html, "class=\"player")]
  img_html <- gsub("\".*", "", str_extract(img_html, paste0("/static/FIFA", season, "/images/players/.*\\.png\" data-")))
  player_headshot <- paste0("https://www.fifaindex.com", img_html)
  player_rating <- as.numeric(str_extract(html[ix], "\\d\\d"))
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
  if(weak_foot == 0) {
    weak_foot <- NA
  }
  if(skill_moves == 0) {
    skill_moves <- NA 
  }
  value <- as.numeric(gsub("\\.", "", str_extract(html[ix + 17], "\\d+\\.*\\d*\\.*\\d*")))
  wage <- as.numeric(gsub("\\.", "", str_extract(html[ix + 18], "\\d+\\.*\\d*")))
  
  ### National Team Info
  ix_country <- ix + which(str_detect(html[-c(1:ix)], "card-header") & str_detect(html[-c(1:ix)], paste0("title=\"(", country_string, player_country, ") FIFA")))
  if(length(ix_country) > 0) {
    country_kit_number <- as.numeric(str_extract(html[ix_country + 4], "\\d+"))
    country_position <- gsub(">|<.*", "", str_extract(html[ix_country + 3], ">[A-z]*</span>"))
  } else {
    country_kit_number <- NA
    country_position <- NA
  }
  
  ### Club Team Info
  ix_club <- which(str_detect(html, "card-header") &  str_detect(html, "class=\"link-team\"") & !str_detect(html, paste0("title=\"(", country_string, player_country, ") FIFA")))
  if(length(ix_club) > 1) {
    player_country <- ifelse(as.numeric(season) <= 19, 
                             gsub("\\sFIFA.*", "", gsub("title=\"|\"\\s.*", "", str_extract(html[ix_club[2]], "title=\".*\""))),
                             gsub("\\sFIFA.*", "", gsub("title=\"|\"\\s.*", "", str_extract(html[ix_club[1]], "title=\".*\""))))
    ix_club <- ifelse(as.numeric(season) <= 19, ix_club[1], ix_club[2])
  }
  player_club <- gsub("\\sFIFA.*", "", gsub("title=\"|\"\\s.*", "", str_extract(html[ix_club], "title=\".*\"")))
  club_kit_number <- as.numeric(str_extract(html[ix_club + 4], "\\d+"))
  club_position <- gsub(">|<.*", "", str_extract(html[ix_club + 3], ">[A-z]*</span>"))
  if(length(player_club) == 0) {
    player_club <- NA 
    club_kit_number <- NA
    club_position <- NA
  }
  
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
  if(length(contract_length) == 0) {
    contract_length <- NA
  }
  
  
  ### Skill Attributes
  ix_skills <- max(grep("Ball Skills", html))
  
  # Ball Skills
  ball_control <- as.numeric(gsub(">", "",  str_extract(html[ix_skills + 3], ">\\d+")))
  dribbling <- as.numeric(gsub(">", "",  str_extract(html[ix_skills + 5], ">\\d+")))
  
  # Defense
  marking <- as.numeric(gsub(">", "",  str_extract(html[ix_skills + 16], ">\\d+")))
  slide_tackle <- as.numeric(gsub(">", "",  str_extract(html[ix_skills + 18], ">\\d+")))
  if(as.numeric(season) >= 11) {
    standing_tackle <- as.numeric(gsub(">", "",  str_extract(html[ix_skills + 20], ">\\d+")))
    offset <- 0
  } else {
    standing_tackle <- NA
    offset <- 2
  }
  
  # Mental
  if(as.numeric(season) > 16) {
    aggression <- as.numeric(gsub(">", "",  str_extract(html[ix_skills + 31 - offset], ">\\d+")))
    reactions <- as.numeric(gsub(">", "",  str_extract(html[ix_skills + 33 - offset], ">\\d+")))
    att_position <- as.numeric(gsub(">", "",  str_extract(html[ix_skills + 35 - offset], ">\\d+")))
    interceptions <- as.numeric(gsub(">", "",  str_extract(html[ix_skills + 37 - offset], ">\\d+")))
    vision <- as.numeric(gsub(">", "",  str_extract(html[ix_skills + 39 - offset], ">\\d+")))
    composure <- as.numeric(gsub(">", "",  str_extract(html[ix_skills + 41], ">\\d+")))
    creativity <- NA
  } else if(as.numeric(season) < 11 & as.numeric(season) > 5) {
    aggression <- as.numeric(gsub(">", "",  str_extract(html[ix_skills + 31 - offset], ">\\d+")))
    composure <- as.numeric(gsub(">", "",  str_extract(html[ix_skills + 33 - offset], ">\\d+")))
    reactions <- as.numeric(gsub(">", "",  str_extract(html[ix_skills + 35 - offset], ">\\d+")))
    vision <- NA
    interceptions <- NA
    att_position <- NA
    creativity <- NA
    offset <- offset + 6
  } else if(as.numeric(season) > 11) {
    aggression <- as.numeric(gsub(">", "",  str_extract(html[ix_skills + 31 - offset], ">\\d+")))
    reactions <- as.numeric(gsub(">", "",  str_extract(html[ix_skills + 33 - offset], ">\\d+")))
    att_position <- as.numeric(gsub(">", "",  str_extract(html[ix_skills + 35 - offset], ">\\d+")))
    interceptions <- as.numeric(gsub(">", "",  str_extract(html[ix_skills + 37 - offset], ">\\d+")))
    vision <- as.numeric(gsub(">", "",  str_extract(html[ix_skills + 39 - offset], ">\\d+")))
    composure <- NA
    creativity <- NA
    offset <- offset + 2
  } else {
    aggression <- as.numeric(gsub(">", "",  str_extract(html[ix_skills + 31 - offset], ">\\d+")))
    composure <- as.numeric(gsub(">", "",  str_extract(html[ix_skills + 35 - offset], ">\\d+")))
    reactions <- as.numeric(gsub(">", "",  str_extract(html[ix_skills + 33 - offset], ">\\d+")))
    creativity <- as.numeric(gsub(">", "",  str_extract(html[ix_skills + 37 - offset], ">\\d+")))
    att_position <- NA
    vision <- NA
    interceptions <- NA
    offset <- offset + 4
  }
  
  # Passing
  crossing <- as.numeric(gsub(">", "",  str_extract(html[ix_skills + 52 - offset], ">\\d+")))
  short_pass <- as.numeric(gsub(">", "",  str_extract(html[ix_skills + 54 - offset], ">\\d+")))
  long_pass <- as.numeric(gsub(">", "",  str_extract(html[ix_skills + 56 - offset], ">\\d+")))
  
  # Physical
  if(as.numeric(season) >= 11) {
    acceleration <- as.numeric(gsub(">", "",  str_extract(html[ix_skills + 67 - offset], ">\\d+")))
    stamina <- as.numeric(gsub(">", "",  str_extract(html[ix_skills + 69 - offset], ">\\d+")))
    strength <- as.numeric(gsub(">", "",  str_extract(html[ix_skills + 71 - offset], ">\\d+")))
    balance <- as.numeric(gsub(">", "",  str_extract(html[ix_skills + 73 - offset], ">\\d+")))
    sprint_speed <- as.numeric(gsub(">", "",  str_extract(html[ix_skills + 75 - offset], ">\\d+")))
    agility <- as.numeric(gsub(">", "",  str_extract(html[ix_skills + 77 - offset], ">\\d+")))
    jumping <- as.numeric(gsub(">", "",  str_extract(html[ix_skills + 79 - offset], ">\\d+")))
  } else if(as.numeric(season) > 5){
    acceleration <- as.numeric(gsub(">", "",  str_extract(html[ix_skills + 67 - offset], ">\\d+")))
    stamina <- as.numeric(gsub(">", "",  str_extract(html[ix_skills + 69 - offset], ">\\d+")))
    strength <- as.numeric(gsub(">", "",  str_extract(html[ix_skills + 71 - offset], ">\\d+")))
    sprint_speed <- as.numeric(gsub(">", "",  str_extract(html[ix_skills + 73 - offset], ">\\d+")))
    balance <- NA
    agility <- NA
    jumping <- NA
    offset <- offset + 6
  } else {
    acceleration <- as.numeric(gsub(">", "",  str_extract(html[ix_skills + 67 - offset], ">\\d+")))
    sprint_speed <- as.numeric(gsub(">", "",  str_extract(html[ix_skills + 69 - offset], ">\\d+")))
    stamina <- as.numeric(gsub(">", "",  str_extract(html[ix_skills + 71 - offset], ">\\d+")))
    strength <- as.numeric(gsub(">", "",  str_extract(html[ix_skills + 73 - offset], ">\\d+")))
    balance <- as.numeric(gsub(">", "",  str_extract(html[ix_skills + 75 - offset], ">\\d+")))
    agility <- NA
    jumping <- NA
    offset <- offset + 4
  }
  
  # Shooting 
  if(as.numeric(season) >= 11) {
    heading <- as.numeric(gsub(">", "",  str_extract(html[ix_skills + 90 - offset], ">\\d+")))
    shot_power <- as.numeric(gsub(">", "",  str_extract(html[ix_skills + 92 - offset], ">\\d+")))
    finishing <- as.numeric(gsub(">", "",  str_extract(html[ix_skills + 94 - offset], ">\\d+")))
    long_shots <- as.numeric(gsub(">", "",  str_extract(html[ix_skills + 96 - offset], ">\\d+")))
    curve <- as.numeric(gsub(">", "",  str_extract(html[ix_skills + 98 - offset], ">\\d+")))
    penalties <- as.numeric(gsub(">", "",  str_extract(html[ix_skills + 102 - offset], ">\\d+")))
    vollies <- as.numeric(gsub(">", "",  str_extract(html[ix_skills + 104 - offset], ">\\d+")))
    free_kick_accuracy <- as.numeric(gsub(">", "",  str_extract(html[ix_skills + 100 - offset], ">\\d+")))
    shot_accuracy <- NA
  } else if(as.numeric(season) > 5) {
    heading <- as.numeric(gsub(">", "",  str_extract(html[ix_skills + 90 - offset], ">\\d+")))
    shot_power <- as.numeric(gsub(">", "",  str_extract(html[ix_skills + 92 - offset], ">\\d+")))
    finishing <- as.numeric(gsub(">", "",  str_extract(html[ix_skills + 94 - offset], ">\\d+")))
    long_shots <- as.numeric(gsub(">", "",  str_extract(html[ix_skills + 96 - offset], ">\\d+")))
    free_kick_accuracy <- as.numeric(gsub(">", "",  str_extract(html[ix_skills + 98 - offset], ">\\d+")))
    curve <- NA
    vollies <- NA
    penalties <- NA
    shot_accuracy <- NA
    offset <- offset + 6
  } else {
    heading <- as.numeric(gsub(">", "",  str_extract(html[ix_skills + 90 - offset], ">\\d+")))
    shot_accuracy <- as.numeric(gsub(">", "",  str_extract(html[ix_skills + 92 - offset], ">\\d+")))
    shot_power <- as.numeric(gsub(">", "",  str_extract(html[ix_skills + 94 - offset], ">\\d+")))
    finishing <- NA
    long_shots <- NA
    free_kick_accuracy <- NA
    curve <- NA
    vollies <- NA
    penalties <- NA
    offset <- offset + 10
  }
  
  
  # Goalkeeper
  if(as.numeric(season) >= 11) {
    gk_positioning <- as.numeric(gsub(">", "",  str_extract(html[ix_skills + 115 - offset], ">\\d+")))
    gk_diving <- as.numeric(gsub(">", "",  str_extract(html[ix_skills + 117 - offset], ">\\d+")))
    gk_handling <- as.numeric(gsub(">", "",  str_extract(html[ix_skills + 119 - offset], ">\\d+")))
    gk_kicking <- as.numeric(gsub(">", "",  str_extract(html[ix_skills + 121 - offset], ">\\d+")))
    gk_reflexes <- as.numeric(gsub(">", "",  str_extract(html[ix_skills + 123 - offset], ">\\d+")))
    gk_rushing <- NA
  } else if(as.numeric(season) > 5){
    gk_reflexes <- as.numeric(gsub(">", "",  str_extract(html[ix_skills + 115 - offset], ">\\d+")))
    gk_handling <- as.numeric(gsub(">", "",  str_extract(html[ix_skills + 117 - offset], ">\\d+")))
    gk_positioning <- as.numeric(gsub(">", "",  str_extract(html[ix_skills + 119 - offset], ">\\d+")))
    gk_diving <- as.numeric(gsub(">", "",  str_extract(html[ix_skills + 121 - offset], ">\\d+")))
    gk_kicking <- NA
    gk_rushing <- NA
  } else {
    gk_reflexes <- as.numeric(gsub(">", "",  str_extract(html[ix_skills + 115 - offset], ">\\d+")))
    gk_handling <- as.numeric(gsub(">", "",  str_extract(html[ix_skills + 119 - offset], ">\\d+")))
    gk_positioning <- as.numeric(gsub(">", "",  str_extract(html[ix_skills + 121 - offset], ">\\d+")))
    gk_rushing <- as.numeric(gsub(">", "",  str_extract(html[ix_skills + 117 - offset], ">\\d+")))
    gk_kicking <- NA
    gk_diving <- NA
    
  }
  
  df <- tibble("player_id" = player_id,
               "name" = player_name,
               "season" = season,
               "year" = as.numeric(paste0("20", season)),
               "page_url" = url,
               "headshot_url" = player_headshot,
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
               "aggression" = aggression,
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
               "creativity" = creativity,
               "heading" = heading,
               "shot_power" = shot_power,
               "finishing" = finishing,
               "long_shots" = long_shots,
               "curve" = curve,
               "free_kick_accuracy" = free_kick_accuracy,
               "shot_accuracy" = shot_accuracy,
               "penalties" = penalties,
               "gk_positioning" = gk_positioning,
               "gk_diving" = gk_diving,
               "gk_handling" = gk_handling,
               "gk_kicking" = gk_kicking,
               "gk_reflexes" = gk_reflexes,
               "gk_rushing" = gk_rushing)
  
  return(df)
}


### Scrape Player Stats from 2005-2020
seasons <- c(paste0(0, 5:9), 14:20)
for(s in seasons) {
  print(paste0("Scraping Player Links for 20", s, " Season"))
  df_links <- future_map2_dfr(rep(s, 700), 1:700, get_player_links)
  write_csv(df_links, paste0("links/player_links_20", s, ".csv"))
}

for(s in seasons) {
  print(paste0("Scraping Player Stats for 20", s, " Season"))
  df_links <- read_csv(paste0("links/player_links_20", s, ".csv"))
  df_stats <- df_links %>%
    as.list() %>%
    future_pmap_dfr(get_player_stats, .progress = T)
  df_stats$preferred_positions <- unlist(lapply(df_stats$preferred_positions, function(x) {paste(x, collapse = "/")}))
  write_csv(df_stats, paste0("stats/player_stats_20", s, ".csv"))
  print(nrow(df_links) == nrow(df_stats))
}

### Save Results
df_stats <- map_dfr(dir("stats", full.names = T), function(file) read_csv(file, col_types = cols(season = col_character())))
write_csv(df_stats, "player_stats.csv")
