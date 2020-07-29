library(tidyverse)
library(gganimate)

# Load data

chart_tidy <- read_csv("C:\\Users\\username\\path\\file.csv")

# Limit to top 10 of any thing for each time period

chart_formatted <- chart_tidy %>%
  group_by(year) %>%
  # The * 1 makes it possible to have non-integer ranks while sliding
  mutate(rank = rank(-value),
         Value_rel = value/value[rank==1],
         Value_lbl = paste0(" ",round(value))) %>%
  group_by(thing) %>% 
  filter(rank <=10) %>%
  ungroup()

# Create animation

anim <- ggplot(chart_formatted, aes(rank, group = thing, 
                                  fill = as.factor(thing), color = as.factor(thing))) +
  geom_tile(aes(y = value/2,
                height = value,
                width = 0.9), alpha = 0.8, color = NA) +
  geom_text(aes(y = 0, label = paste(thing, " ")), vjust = 0.2, hjust = 1) +
  geom_text(aes(y=value,label = Value_lbl, hjust=0)) +
  coord_flip(clip = "off", expand = FALSE) +
  scale_y_continuous(labels = scales::comma) +
  scale_x_reverse() +
  guides(color = FALSE, fill = FALSE) +
  theme(axis.line=element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        legend.position="none",
        panel.background=element_blank(),
        panel.border=element_blank(),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        panel.grid.major.x = element_line( size=.1, color="grey" ),
        panel.grid.minor.x = element_line( size=.1, color="grey" ),
        plot.title=element_text(size=25, hjust=0.5, face="bold", colour="grey", vjust=-1),
        plot.subtitle=element_text(size=18, hjust=0.5, face="italic", color="grey"),
        plot.caption =element_text(size=8, hjust=0.5, face="italic", color="grey"),
        plot.background=element_blank(),
        plot.margin = margin(2,2, 2, 4, "cm")) +
  transition_states(year, transition_length = 4, state_length = 1, wrap = FALSE) +
  view_follow(fixed_x = TRUE)  +
  labs(title = 'Campaign Activity Per Day : {closest_state}',  
       subtitle  =  "Top Campaigns",
       caption  = "#imadedis") 

# Render as GIF

animate(anim, 200, fps = 20,  width = 1200, height = 1000, 
        renderer = gifski_renderer("filename.gif"), end_pause = 15, start_pause =  15) 
