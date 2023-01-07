
# Plotting functions
# Contains horizontal and vertical field themes, as well as other plots showing changing probabilities

# Create a separate NFL field theme

horizontal_field_theme = function() {
  
  list(
    geom_segment(x = 0, xend = 0, y = 0, yend = 160/3, colour = 'black'), 
    geom_segment(x = 10, xend = 10, y = 0, yend = 160/3, colour = 'red'), 
    geom_segment(x = 15, xend = 15, y = 0, yend = 160/3, colour = 'black'), 
    geom_segment(x = 20, xend = 20, y = 0, yend = 160/3, colour = 'black'), 
    geom_segment(x = 25, xend = 25, y = 0, yend = 160/3, colour = 'black'), 
    geom_segment(x = 30, xend = 30, y = 0, yend = 160/3, colour = 'black'), 
    geom_segment(x = 35, xend = 35, y = 0, yend = 160/3, colour = 'black'), 
    geom_segment(x = 40, xend = 40, y = 0, yend = 160/3, colour = 'black'), 
    geom_segment(x = 45, xend = 45, y = 0, yend = 160/3, colour = 'black'), 
    geom_segment(x = 50, xend = 50, y = 0, yend = 160/3, colour = 'black'), 
    geom_segment(x = 55, xend = 55, y = 0, yend = 160/3, colour = 'black'), 
    geom_segment(x = 60, xend = 60, y = 0, yend = 160/3, colour = 'black'), 
    geom_segment(x = 65, xend = 65, y = 0, yend = 160/3, colour = 'black'), 
    geom_segment(x = 70, xend = 70, y = 0, yend = 160/3, colour = 'black'), 
    geom_segment(x = 75, xend = 75, y = 0, yend = 160/3, colour = 'black'), 
    geom_segment(x = 80, xend = 80, y = 0, yend = 160/3, colour = 'black'), 
    geom_segment(x = 85, xend = 85, y = 0, yend = 160/3, colour = 'black'), 
    geom_segment(x = 90, xend = 90, y = 0, yend = 160/3, colour = 'black'), 
    geom_segment(x = 95, xend = 95, y = 0, yend = 160/3, colour = 'black'), 
    geom_segment(x = 100, xend = 100, y = 0, yend = 160/3, colour = 'black'), 
    geom_segment(x = 105, xend = 105, y = 0, yend = 160/3, colour = 'black'), 
    geom_segment(x = 110, xend = 110, y = 0, yend = 160/3, colour = 'red'), 
    geom_segment(x = 120, xend = 120, y = 0, yend = 160/3, colour = 'black'), 
    geom_segment(x = 0, xend = 120, y = 0, yend = 0, colour = "black"), 
    geom_segment(x = 0, xend = 120, y = 160/3, yend = 160/3, colour = "black"), 
    geom_segment(x = 10, xend = 110, y = 26.67 - 3.0833, yend = 26.67 - 3.08333, colour = "black", linetype = "dashed"), 
    geom_segment(x = 10, xend = 110, y = 26.67 + 3.0833, yend = 26.67 + 3.08333, colour = "black", linetype = "dashed"), 
    xlim(c(-10, 140)), 
    ylim(c(-10, 160/3 + 10)), 
    theme_minimal(), 
    theme(axis.text.x = element_blank(),
          axis.text.y = element_blank(),
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          axis.ticks = element_blank(),
          panel.grid = element_blank()) 
  )
  
}

vertical_field_theme = function() {
  
  list(
    geom_segment(y = 0, yend = 0, x = 0, xend = 160/3, colour = 'black'), 
    geom_segment(y = 10, yend = 10, x = 0, xend = 160/3, colour = 'red'), 
    geom_segment(y = 15, yend = 15, x = 0, xend = 160/3, colour = 'black'), 
    geom_segment(y = 20, yend = 20, x = 0, xend = 160/3, colour = 'black'), 
    geom_segment(y = 25, yend = 25, x = 0, xend = 160/3, colour = 'black'), 
    geom_segment(y = 30, yend = 30, x = 0, xend = 160/3, colour = 'black'), 
    geom_segment(y = 35, yend = 35, x = 0, xend = 160/3, colour = 'black'), 
    geom_segment(y = 40, yend = 40, x = 0, xend = 160/3, colour = 'black'), 
    geom_segment(y = 45, yend = 45, x = 0, xend = 160/3, colour = 'black'), 
    geom_segment(y = 50, yend = 50, x = 0, xend = 160/3, colour = 'black'), 
    geom_segment(y = 55, yend = 55, x = 0, xend = 160/3, colour = 'black'), 
    geom_segment(y = 60, yend = 60, x = 0, xend = 160/3, colour = 'black'), 
    geom_segment(y = 65, yend = 65, x = 0, xend = 160/3, colour = 'black'), 
    geom_segment(y = 70, yend = 70, x = 0, xend = 160/3, colour = 'black'), 
    geom_segment(y = 75, yend = 75, x = 0, xend = 160/3, colour = 'black'), 
    geom_segment(y = 80, yend = 80, x = 0, xend = 160/3, colour = 'black'), 
    geom_segment(y = 85, yend = 85, x = 0, xend = 160/3, colour = 'black'), 
    geom_segment(y = 90, yend = 90, x = 0, xend = 160/3, colour = 'black'), 
    geom_segment(y = 95, yend = 95, x = 0, xend = 160/3, colour = 'black'), 
    geom_segment(y = 100, yend = 100, x = 0, xend = 160/3, colour = 'black'), 
    geom_segment(y = 105, yend = 105, x = 0, xend = 160/3, colour = 'black'), 
    geom_segment(y = 110, yend = 110, x = 0, xend = 160/3, colour = 'red'), 
    geom_segment(y = 120, yend = 120, x = 0, xend = 160/3, colour = 'black'), 
    geom_segment(y = 0, yend = 120, x = 0, xend = 0, colour = "black"), 
    geom_segment(y = 0, yend = 120, x = 160/3, xend = 160/3, colour = "black"), 
    geom_segment(y = 10, yend = 110, x = 26.67 - 3.0833, xend = 26.67 - 3.08333, colour = "black", linetype = "dashed"), 
    geom_segment(y = 10, yend = 110, x = 26.67 + 3.0833, xend = 26.67 + 3.08333, colour = "black", linetype = "dashed"), 
    ylim(c(-10, 120)), 
    xlim(c(-10, 160/3 + 10)), 
    theme_minimal(), 
    theme(axis.text.x = element_blank(),
          axis.text.y = element_blank(),
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          axis.ticks = element_blank(),
          panel.grid = element_blank()) 
  )
  
}


plot_play_dots = function(game_id, play_id, vert = TRUE, zoom = TRUE, output_file_name = NULL) {
  
  # game_id = 2021090900
  # play_id = 97
  
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
  
  print(paste("Min X:", MIN_X, '---- Max X:', MAX_X))
  print(paste("Min Y:", MIN_Y, '---- Max Y:', MAX_Y))
  
  # Get relevant information
  play_description = play_metadata$playDescription
  absolute_yardline = play_metadata$absoluteYardlineNumber
  
  if (vert) {
    animated_plot = ggplot(tracking_data, aes(x = yNorm, y = xNorm)) +
      vertical_field_theme() + 
    
      # Add annotation showing the event
      geom_label(x = 160/6, y = MAX_X + 5, aes(label = paste('Event:', eventRecent)))
    
    if (zoom) {
      animated_plot = animated_plot + 
        ylim(c(MIN_X - X_PADDING, MAX_X + X_PADDING)) + 
        xlim(c(MIN_Y - Y_PADDING, MAX_Y + Y_PADDING))
    }
    
  
  } else {
    animated_plot = ggplot(tracking_data, aes(x = xNorm, y = yNorm)) +
      horizontal_field_theme() + 
      
      # Add annotation showing the event
      geom_label(x = 25, y = MAX_Y + 5, aes(label = paste('Event:', eventRecent))) 
    
    if (zoom) {
      animated_plot = animated_plot + 
      xlim(c(MIN_X - X_PADDING, MAX_X + X_PADDING)) + 
      ylim(c(MIN_Y - Y_PADDING, MAX_Y + Y_PADDING))
    }
    
  }
  
  
  if (zoom) {
    DOT_SIZE = 9
    NUMBER_SIZE = 7
  } else{
    DOT_SIZE = 5
    NUMBER_SIZE = 4
  }
  
  animated_plot = animated_plot +
    guides(colour = 'none') + 

    # Add path and labels
    geom_point(aes(colour = team), size = DOT_SIZE) +
    geom_text(aes(label = jerseyNumber), size = NUMBER_SIZE) + 
    
    # Add title containing play information
    labs(title = paste0("Play ", game_id, '-', play_id),
         subtitle = play_description) + 
    
    # Add animations
    transition_time(frameId)
  
  if (!is.null(output_file_name)) {
    animated_play_art = animate(animated_plot, fps = 10, nframes = max(tracking_data$frameId))
    anim_save(output_file_name, animation = animated_play_art)
  } else {
    animated_play_art = animate(animated_plot, fps = 10, nframes = max(tracking_data$frameId))
    return(animated_play_art)
  }
  
}

plot_play_with_team_level_pressure_probability = function(game_id, play_id, probability_df, output_file_name = NULL) {
  
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

plot_play_with_rusher_level_pressure_probability = function(game_id, play_id, probability_df, output_file_name = NULL) {
  
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


# Sample regular plots
# play1_vert = plot_play_dots(2021090900, 97, vert=TRUE)
# play1_horiz = plot_play_dots(2021090900, 97, vert=FALSE)

# Sample plots showing the prob of QB pressure (you need to provide your own dataframe containing the probability of pressure at each frame)
# sample_play1_probabilities = test_with_predicted %>%
#   filter(game_play_id == '2021103106-1333') %>%
#   select(frame_id, prob)
# 
# sample_play1_double = plot_play_with_team_level_hurry_probability(2021103106, 1333, sample_play1_probabilities)

# Sample plots showing the prob of QB pressure (you need to provide your own dataframe containing the probability of pressure at each frame for each rusher)
# play1_art = generate_play_art_from_id('2021091203-601', players_with_predicted, 'output/plots/play-2021091203-601-burrow-instant-sack.gif')
# play1_art
