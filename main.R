library(readr)
library(dplyr)
library(ggplot2)

#Gathering relevant play-by-play files for J.R. "Swish" Smith and Nick "Swaggy P" Young based on the teams they played for during the 2012-13 and 2013-14 seasons
SWISH_GAMES <- list.files(path = "/2012-13", pattern = "*NYK+") %>% c(list.files(path = "/2013-14", pattern = "*NYK+"))
SWAGGYP_GAMES <- list.files(path = "/2012-13", pattern = "*PHI+") %>% c(list.files(path = "/2013-14", pattern = "*LAL+"))

#Data frame to be built throughout calculations for predictive model
smith_shots_df <- data.frame()
young_shots_df <- data.frame()

#Initializing of streak vectors for both J.R. Smith and Nick Young
smith_hot_streaks <- vector(mode="list")
smith_cold_streaks <- vector(mode="list")
young_hot_streaks <- vector(mode="list")
young_cold_streaks <- vector(mode="list")
#After all final calculations and processing, the above vectors are organized in the following fashion:
    #Number of times of making next shot given making/missing last n shots... 
        # lastname_hot/cold_streaks[[n]]['makes'] 
    #Number of times of missing next shot given making/missing last n shots P... 
        # lastname_hot/cold_streaks[[n]]['misses']
    #Probability of making next shot given making/missing last n shots... 
        # P(make|n makes/misses) = lastname_hot/cold_streaks[[n]]['make_next']
    #Probability of missing next shot given making/missing last n shots... 
        # P(miss|n makes/misses) = lastname_hot/cold_streaks[[n]]['miss_next']
    #Average shot distance for a hot/cold streak of length n...
        # lastname_hot/cold_streaks[[n]]['miss_next']
    #Average points scored for a hot/cold streak of length n...
        # lastname_hot/cold_streaks[[n]]['pts']
    
#Determing streak statistics for J.R. Smith
for (gm in 1:length(SWISH_GAMES)) {
  file <- paste("/2012-13/", SWISH_GAMES[gm], sep = "")
  if (!file.exists(file)) file <- paste("/2013-14/", SWISH_GAMES[gm], sep = "")
  game <- read_csv(file, col_types = cols())
  result <- calculate_streaks("J.R. Smith", game, smith_shots_df)
  smith_shots_df <- result$shots
  smith_hot_streaks <- consolidate_streaks(smith_hot_streaks, result$hot)
  smith_cold_streaks <- consolidate_streaks(smith_cold_streaks, result$cold)
}

#Determining streak statistics for Nick Young
for (gm in 1:length(SWAGGYP_GAMES)) {
  file <- paste("/2012-13/", SWAGGYP_GAMES[gm], sep = "")
  if (!file.exists(file)) file <- paste("/2013-14/", SWAGGYP_GAMES[gm], sep = "")
  game <- read_csv(file, col_types = cols())
  result <- calculate_streaks("Nick Young", game, young_shots_df)
  young_shots_df <- result$shots
  young_hot_streaks <- consolidate_streaks(young_hot_streaks, result$hot)
  young_cold_streaks <- consolidate_streaks(young_cold_streaks, result$cold)
}

#Converting total points and total shot distances into averages
smith_hot_streaks <- average_streaks(smith_hot_streaks)
smith_cold_streaks <- average_streaks(smith_cold_streaks)
young_hot_streaks <- average_streaks(young_hot_streaks)
young_cold_streaks <- average_streaks(young_cold_streaks)

#Plot graphs of shot distance, expected points, and shot probability against streak length
smith_plots <- plot_streaks("J.R. Smith", smith_hot_streaks, smith_cold_streaks)
young_plots <- plot_streaks("Nick Young", young_hot_streaks, young_cold_streaks)

#Delete irrelevant clutter variables from the global environment
rm(gm, file, result, game, SWAGGYP_GAMES, SWISH_GAMES)
