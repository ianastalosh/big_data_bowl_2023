
# Experimentation of different weighting functions
frame_weight_df = data.frame(frame = 1:100) %>% 
  mutate(sharp_hyperbola = 1/(frame + 1),
         medium_hyperbola = 1/(0.05*frame + 1),
         weak_hyperbola = 1/(0.01*frame + 1),
         lcdf = (1 + exp(-6.35/11.3)) / (1 + exp((frame + 6.35)/11.3)),
         dll = 1 / (1 + (frame/11.13)^2.14),
         gaussian = exp(-frame^2/256.14)
         )

melt_for_plotting = melt(frame_weight_df, id.vars = 'frame') 

weighting_functions = ggplot(melt_for_plotting, aes(x = frame, y = value)) + 
  geom_line(aes(colour = variable), linewidth=1) + 
  theme_minimal() + 
  labs(title = 'Possible Weighting Functions',
       subtitle = 'How much do we want to weight early frames over later frames?',
       x = 'Frame',
       y = 'Weight')

OUTPUT_WEIGHTING_FUNCTIONS_FILEPATH = 'output/plots/weightings/different_weighting_functions.png'
ggsave(OUTPUT_WEIGHTING_FUNCTIONS_FILEPATH, weighting_functions, height=6, width=8, bg='white')

# Plot only the DLL function
dll_weighting = ggplot(frame_weight_df, aes(x = frame, y = dll)) + 
  geom_line(linewidth = 1, colour='purple') + 
  theme_minimal() + 
  labs(title = 'Downward Log Logistic Function',
       subtitle = 'To be used for weighting pressure throughout the play',
       x = 'Frame',
       y = 'Weight')

DLL_WEIGHTING_FUNCTIONS_FILEPATH = 'output/plots/coerce_plots/dll_weighting_function.png'
ggsave(DLL_WEIGHTING_FUNCTIONS_FILEPATH, dll_weighting, height=6, width=8, bg='white')
