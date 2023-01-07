
# Plot play dots with individual sack probability
source('plotting.R')

plot_play_with_probability_with_colours = function(game_id, play_id, probability_df, output_file_name = NULL) {
  
  # probability_df should be a df with three columns: frame_id, rush_id (or something) and prob
  
  # Debugging variables
  # sample_play_probabilities = players_with_predicted %>%
  #   filter(game_play_id == '2021110100-1182') %>%
  #   select(game_play_id, frameId, rush_id, prob)
  # 
  # game_id = 2021110100
  # play_id = 1182
  # probability_df = sample_play_probabilities
  
  # Collect data
  tracking_data = fe_tracking %>% filter(gameId == game_id, playId == play_id)
  play_metadata = plays %>% filter(gameId == game_id, playId == play_id)
  pff_metadata = pff %>% filter(gameId == game_id, playId == play_id)
  
  ball_tracking = tracking_data %>% filter(team == 'football')
  offense_tracking = tracking_data %>% filter((pff_role %in% c('Pass', 'Pass Block')) | (!is.na(pff_nflIdBlockedPlayer)))
  defense_tracking = tracking_data %>% filter(pff_role %in% c('Pass Rush')) %>% 
    left_join(probability_df, by=c('game_play_id', 'frameId', 'nflId' = 'rush_id')) %>%
    arrange(nflId, frameId) %>%
    group_by(nflId) %>% 
    mutate(prob = na.locf(prob, fromLast=TRUE, na.rm = FALSE),
           prob = na.locf(prob, fromLast=FALSE, na.rm = FALSE)) # fill na's such that frames outside the pass rush 
  
  probability_df_with_jersey = defense_tracking %>% 
    select(game_play_id, frameId, nflId, prob, jerseyNumber) %>%
    mutate(jerseyNumber = as.character(jerseyNumber))
  
  MIN_X = min(tracking_data$xNorm)
  MAX_X = max(tracking_data$xNorm)
  MIN_Y = min(tracking_data$yNorm)
  MAX_Y = max(tracking_data$yNorm)
  
  X_PADDING = 5
  Y_PADDING = 10
  DOT_SIZE = 11
  NUMBER_SIZE = 7
  
  #### PLOT 1 - THE DOT PLOT
  # Get relevant information
  play_description = play_metadata$playDescription
  absolute_yardline = play_metadata$absoluteYardlineNumber
  
  dot_plot = ggplot(tracking_data, aes(x = yNorm, y = xNorm)) +
    vertical_field_theme() + 
    
    # Add annotation showing the event
    geom_label(x = 160/6, y = MAX_X + 5, aes(label = paste('Event:', eventRecent))) + 
    
    ylim(c(MIN_X - X_PADDING, MAX_X + X_PADDING)) + 
    xlim(c(MIN_Y - Y_PADDING, MAX_Y + Y_PADDING)) + 
    guides(colour = 'none') + 
    
    # Add path and labels
    geom_point(data = defense_tracking, aes(fill = prob), size = DOT_SIZE, shape=22) +
    geom_point(data = offense_tracking, fill = '#EAFEFF', size = DOT_SIZE, shape=21) + 
    geom_text(aes(label = jerseyNumber), size = NUMBER_SIZE) + 
    scale_fill_gradient2(low='green', mid='yellow', high='red', midpoint=0.3) +
    geom_point(data = ball_tracking, fill = 'brown', shape=18, size = 5) + 
    
    # Add title containing play information
    labs(title = paste0("Play ", game_id, '-', play_id),
         subtitle = play_description) + 
    
    # Add animations
    transition_time(frameId)
  
  animated_dot_plot = animate(dot_plot,
                              fps = 5,
                              nframes = max(tracking_data$frameId),
                              renderer = magick_renderer())
  
  #### PLOT 2 - THE PROBABILITY PLOT
  probability_plot = ggplot(probability_df_with_jersey, aes(x = frameId, y = prob)) + 
    theme_minimal() + 
    geom_line(aes(colour=jerseyNumber)) +
    labs(title = 'Hit, Hurry or Sack Probability Throughout Play') +
    ylim(c(0,1)) +
    transition_reveal(frameId) 
  
  animated_probability_plot = animate(probability_plot,
                                      fps = 5,
                                      nframes = max(probability_df_with_jersey$frameId),
                                      renderer = magick_renderer())
  
  new_gif = image_append(c(animated_dot_plot[1], animated_probability_plot[1]))
  for (i in 2:max(tracking_data$frameId)) {
    combined_image = image_append(c(animated_dot_plot[i], animated_probability_plot[i]))
    new_gif = c(new_gif, combined_image)
    
  }
  
  if (!is.null(output_file_name)) {
    image_write(new_gif, path = output_file_name, format = "gif")
    
  }
  
  return(new_gif)
  
}

# Sample plays
generate_play_art_from_id = function(game_play_id, predictions_df, output_file_name=NULL) {
  # Function to get the required probabilities from dataframe and plot
  
  play_and_id = strsplit(game_play_id, split='-')
  play_probs = predictions_df %>% 
    filter(game_play_id == game_play_id) %>% 
    select(game_play_id, frameId, rush_id, prob)
  
  play_art = plot_play_with_probability_with_colours(play_and_id[[1]][1], play_and_id[[1]][2], 
                                                     play_probs, output_file_name)
  return(play_art)
}

# Contains sack
play1_art = generate_play_art_from_id('2021091203-601', players_with_predicted, 'output/plots/play-2021091203-601-burrow-instant-sack.gif')
play1_art

# Lots of rushers
play2_art = generate_play_art_from_id('2021092613-2552', players_with_predicted, 'output/plots/play-2021092613-2552-garoppolo-td.gif')
play2_art

# Super protected
play3_art = generate_play_art_from_id('2021091907-1540', players_with_predicted, 'output/plots/play-2021091907-1540-hurts-rollout.gif')
play3_art


# Other plays of interest: 
#' 2021092607-2923
#' 2021102408-3744
#' 2021103108-1651
#' 2021110100-1182


play4_art = generate_play_art_from_id('2021092607-2923', players_with_predicted, 'output/plots/play-2021092607-2923-roethlisberger-protected.gif')
play4_art

play5_art = generate_play_art_from_id('2021102408-3744', players_with_predicted, 'output/plots/play-2021102408-3744.gif')
play5_art

play6_art = generate_play_art_from_id('2021103108-1651', players_with_predicted, 'output/plots/play-2021103108-1651.gif')
play6_art

play7_art = generate_play_art_from_id('2021110100-1182', players_with_predicted, 'output/plots/play-2021110100-1182.gif')
play7_art

play8_art = generate_play_art_from_id('2021091600-1991', players_with_predicted, 'output/plots/play-2021091600-1991-early-pressure.gif')
play8_art


no_save = generate_play_art_from_id('2021102408-353', players_with_predicted)
no_save




