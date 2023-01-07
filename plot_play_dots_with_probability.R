
# Plot play dots alongside sack probability
library(magick)

source('plotting.R')

plot_play_with_probability = function(game_id, play_id, probability_df, output_file_name = NULL) {
  
  # Note, probability_df should be a data frame with two columns: frame_id and some 2nd column

  # game_id = 2021103108
  # play_id = 1651
  # probability_df = sample_play1_probabilities
  
  # Collect data
  tracking_data = fe_tracking %>% filter(gameId == game_id, playId == play_id)
  play_metadata = plays %>% filter(gameId == game_id, playId == play_id)
  pff_metadata = pff %>% filter(gameId == game_id, playId == play_id)
  
  MIN_X = min(tracking_data$xNorm)
  MAX_X = max(tracking_data$xNorm)
  MIN_Y = min(tracking_data$yNorm)
  MAX_Y = max(tracking_data$yNorm)
  
  X_PADDING = 5
  Y_PADDING = 10
  DOT_SIZE = 9
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
    geom_point(aes(colour = team), size = DOT_SIZE) +
    geom_text(aes(label = jerseyNumber), size = NUMBER_SIZE) + 
    
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
  probability_plot = ggplot(probability_df, aes(x = frame_id, y = prob)) + 
    theme_minimal() + 
      geom_line(aes(group = 1)) +
      labs(title = 'Sack Probability Throughout Play') +
      ylim(c(0,1)) +
      transition_reveal(frame_id) 
  
  animated_probability_plot = animate(probability_plot,
                              fps = 5,
                              nframes = max(probability_df$frame_id),
                              renderer = magick_renderer())
  
  new_gif = image_append(c(animated_dot_plot[1], animated_probability_plot[1]))
  for (i in 2:max(tracking_data$frameId)) {
    combined_image = image_append(c(animated_dot_plot[i], animated_probability_plot[i]))
    new_gif = c(new_gif, combined_image)
    
  }
  
  return(new_gif)
  
}

sample_play1_probabilities = test_with_predicted %>%
   filter(game_play_id == '2021103106-1333') %>%
   select(frame_id, prob)

sample_play1_double = plot_play_with_probability(2021103106, 1333, sample_play1_probabilities)


sample_play2_probabilities = test_with_predicted %>%
  filter(game_play_id == '2021092607-2923') %>%
  select(frame_id, sack_prob)

sample_play2_double = plot_play_with_probability(2021092607, 2923, sample_play2_probabilities)

sample_play3_probabilities = test_with_predicted %>%
  filter(game_play_id == '2021102408-3744') %>%
  select(frame_id, sack_prob)

sample_play3_double = plot_play_with_probability(2021102408, 3744, sample_play3_probabilities)

