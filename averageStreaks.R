#This functions modifies the streak vector by calculating the average expected points and shot distances
#for hot/cold steraks of varying lengths
average_streaks <- function(streaks) {
  
  for(n in 1:length(streaks)) {
    streaks[[n]][5] <- streaks[[n]][5] / (streaks[[n]]['makes'] + streaks[[n]]['misses'])
    streaks[[n]][6] <- streaks[[n]][6] / (streaks[[n]]['makes'] + streaks[[n]]['misses'])
    names(streaks[[n]]) <- c("makes", "misses", "make_next", "miss_next", "shot_dist", 'pts')
  }
  
  return(streaks)
}
