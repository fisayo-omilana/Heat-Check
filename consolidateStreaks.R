#This function helps keep a running total of the conditional probabilites for hot/cold streaks
#It follows a similar aggregation format to the calculate_streaks() function
consolidate_streaks <- function(streaks, game_result) {
  
  if(length(game_result) == 0) return(streaks) #Edge case if player makes/misses zero field goals
  
  for(n in 1:length(game_result)) {
    if (length(streaks) < n) {
      streaks[[n]] <- c(0, 0, 0, 0, 0, 0)
      names(streaks[[n]]) <- c("makes", "misses", "make_next", "miss_next", "total_shot_dist", 'total_pts')
    }
    streaks[[n]]['makes'] <- streaks[[n]]['makes'] + game_result[[n]]['makes']
    streaks[[n]]['misses'] <- streaks[[n]]['misses'] + game_result[[n]]['misses']
    streaks[[n]]['make_next'] <- streaks[[n]]['makes'] / (streaks[[n]]['makes'] + streaks[[n]]['misses'])
    streaks[[n]]['miss_next'] <- streaks[[n]]['misses'] / (streaks[[n]]['makes'] + streaks[[n]]['misses'])
    streaks[[n]]['total_shot_dist'] <- streaks[[n]]['total_shot_dist'] + game_result[[n]]['total_shot_dist']
    streaks[[n]]['total_pts'] <- streaks[[n]]['total_pts'] + game_result[[n]]['total_pts']
  }
  
  return(streaks)
}
