#This function calculates the conditional probabilites for hot/cold streaks per game
calculate_streaks <- function(name, game, shot_df) {
  
  #Variables for hot streaks, cold streaks, and vectors containing probabilities & counts for both streaks
  hot_count <- 0
  hot_vec <- vector(mode="list")
  cold_count <- 0 
  cold_vec <- vector(mode="list")
  game$streak_num <- 0 #Create another column containing the streak number
  
  for (row in 1:nrow(game)) {
    
    #Calculating hot streaks
    if (game$event_type[row] == "shot" && game$result[row] == "made" && game$player[row] == name) {
      if (hot_count >= 1) { #If still making shots, adjust probability and counts for current hot streak
        game$streak_num[row] <- hot_count #Inputting relevant hot streak information
        hot_vec[[hot_count]]['makes'] <- hot_vec[[hot_count]]['makes'] + 1
        hot_vec[[hot_count]]['make_next'] <- hot_vec[[hot_count]]['makes'] / (hot_vec[[hot_count]]['makes'] + hot_vec[[hot_count]]['misses'])
        hot_vec[[hot_count]]['miss_next'] <- hot_vec[[hot_count]]['misses'] / (hot_vec[[hot_count]]['makes'] + hot_vec[[hot_count]]['misses'])
        hot_vec[[hot_count]]['total_shot_dist'] <- hot_vec[[hot_count]]['total_shot_dist'] + game$shot_distance[row]
        hot_vec[[hot_count]]['total_pts'] <- hot_vec[[hot_count]]['total_pts'] + game$points[row]
      }
      if (cold_count != 0) { #If breaking out of a cold streak, adjust probability and counts for that cold streak
        game$streak_num[row] <- -cold_count #Inputting relevant cold streak information
        cold_vec[[cold_count]]['makes'] <- cold_vec[[cold_count]]['makes'] + 1
        cold_vec[[cold_count]]['miss_next'] <- cold_vec[[cold_count]]['misses'] / (cold_vec[[cold_count]]['misses'] + cold_vec[[cold_count]]['makes'])
        cold_vec[[cold_count]]['make_next'] <- cold_vec[[cold_count]]['makes'] / (cold_vec[[cold_count]]['makes'] + cold_vec[[cold_count]]['misses'])
        cold_vec[[cold_count]]['total_shot_dist'] <- cold_vec[[cold_count]]['total_shot_dist'] + game$shot_distance[row]
        cold_vec[[cold_count]]['total_pts'] <- cold_vec[[cold_count]]['total_pts'] + game$points[row]
      }
      cold_count <- 0 #Resetting cold streak since made shot
      hot_count <- hot_count + 1 #Incrementing hot shooting streak
      if (length(hot_vec) < hot_count) { #If surpassing longest known shooting streak in current game, create new vector
        hot_vec[[hot_count]] <- c(0, 0, 0, 0, 0, 0)
        names(hot_vec[[hot_count]]) <- c("makes", "misses", "make_next", "miss_next", "total_shot_dist", "total_pts")
      }
      shot_df <- rbind(shot_df, game[row, c('type', 'shot_distance', 'converted_x', 'converted_y', 'streak_num', 'result')]) #Add relevant variables to ongoing data frame
    }
    
    #Calculating cold streaks 
    if (game$event_type[row] == "miss" && game$player[row] == name) {
      if (cold_count >= 1) { #If still missing shots, adjust probability and counts for that cold streak
        game$streak_num[row] <- -cold_count #Inputting relevant cold streak information
        cold_vec[[cold_count]]['misses'] <- cold_vec[[cold_count]]['misses'] + 1
        cold_vec[[cold_count]]['miss_next'] <- cold_vec[[cold_count]]['misses'] / (cold_vec[[cold_count]]['misses'] + cold_vec[[cold_count]]['makes'])
        cold_vec[[cold_count]]['make_next'] <- cold_vec[[cold_count]]['makes'] / (cold_vec[[cold_count]]['makes'] + cold_vec[[cold_count]]['misses'])
        cold_vec[[cold_count]]['total_shot_dist'] <- cold_vec[[cold_count]]['total_shot_dist'] + game$shot_distance[row]
        cold_vec[[cold_count]]['total_pts'] <- cold_vec[[cold_count]]['total_pts'] + game$points[row]
      }
      if (hot_count != 0) { #If ending a hot streak, adjust probability and counts for that hot streak
        game$streak_num[row] <- hot_count
        hot_vec[[hot_count]]['misses'] <- hot_vec[[hot_count]]['misses'] + 1
        hot_vec[[hot_count]]['make_next'] <- hot_vec[[hot_count]]['makes'] / (hot_vec[[hot_count]]['makes'] + hot_vec[[hot_count]]['misses'])
        hot_vec[[hot_count]]['miss_next'] <- hot_vec[[hot_count]]['misses'] / (hot_vec[[hot_count]]['makes'] + hot_vec[[hot_count]]['misses'])
        hot_vec[[hot_count]]['total_shot_dist'] <- hot_vec[[hot_count]]['total_shot_dist'] + game$shot_distance[row]
        hot_vec[[hot_count]]['total_pts'] <- hot_vec[[hot_count]]['total_pts'] + game$points[row]
      }
      hot_count <- 0 #Reset hot streak to zero since missed shot
      cold_count <- cold_count + 1 #Increment cold shooting streak
      if (length(cold_vec) < cold_count) { #If surpassing longest known cold streak in current game, create new vector
        cold_vec[[cold_count]] <- c(0, 0, 0, 0, 0, 0)
        names(cold_vec[[cold_count]]) <- c("makes", "misses", "make_next", "miss_next", "total_shot_dist", "total_pts")
      }
      shot_df <- rbind(shot_df, game[row, c('type', 'shot_distance', 'converted_x', 'converted_y', 'streak_num', 'result')]) #Add relevant variables to ongoing data frame
    }
  }
  
  return(list("hot" = hot_vec, "cold" = cold_vec, "shots" = shot_df))
}

