#This function creates plots of shooting percentages, shot distance from basket, and expected points
#based on consecutive shots made/missed on a hot/cold streak, respectively.
plot_streaks <- function(name, hot_streaks, cold_streaks) {
  
  hot_pct <- c()
  cold_pct <- c()
  hot_dist <- c()
  cold_dist <- c()
  hot_pts <- c()
  for (i in 1:4) {
    hot_pct <- c(hot_pct, hot_streaks[[i]]['make_next'])
    cold_pct <- c(cold_pct, cold_streaks[[i]]['miss_next'])
    hot_dist <- c(hot_dist, hot_streaks[[i]]['shot_dist'])
    cold_dist <- c(cold_dist, cold_streaks[[i]]['shot_dist'])
    hot_pts <- c(hot_pts, hot_streaks[[i]]['pts'])
  }
  pct_plot <- ggplot() + geom_line(aes(x = 1:4, y = hot_pct), color = "red") + geom_line(aes(x = 1:4, y = cold_pct), color = "blue") +
    labs(title = paste(name, "'s Next Shot Outcome by Streak Length", sep = ""), subtitle = "Red = Stay Hot (Make Next Shot %) || Blue = Stay Cold (Miss Next Shot %)", x = "Consecutive Shots", y = "Percentage") +
    theme(plot.subtitle=element_text(face="bold", hjust=0.5))
  dist_plot <- ggplot() + geom_line(aes(x = 1:4, y = hot_dist), color = "red") + geom_line(aes(x = 1:4, y = cold_dist), color = "blue") +
    labs(title = paste(name, "'s Average Shot Distance by Streak Length", sep = ""), subtitle = "Red = Hot (Consecutive Shots Made) || Blue = Cold (Consecutive Shots Missed)", x = "Consecutive Shots", y = "Shot Distance") +
    theme(plot.subtitle=element_text(face="bold", hjust=0.5))
  pts_plot <- ggplot() + geom_line(aes(x = 1:4, y = hot_pts), color = "red") +
    labs(title = paste(name, "'s Average Points Scored by Hot Streak Length", sep = ""), x = "Consecutive Shots", y = "Points Scored")
  
  return(list("pct" = pct_plot, "dist" = dist_plot, "pts" = pts_plot))
  
}