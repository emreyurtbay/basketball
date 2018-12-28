# Basketball Visualization
# data : /Users/emreyurtbay/Documents/Datasets/bball12-19-18.csv

################################################################################
# Import Data and Clean
################################################################################

library(tidyverse)
basketball <- read_csv("/Users/emreyurtbay/Documents/Datasets/bball12-19-18.csv")
basketball <- basketball %>% drop_na()

#################################################################################
# Create Columns and Filter Data
#################################################################################

# Add points per game 
basketball <- mutate(basketball, 
                     PPG = basketball$PTS/basketball$GP)

# Filter out players who have played less than the Median number of games played
playedMins <- filter(basketball, basketball$MIN > median(basketball$MIN))

#################################################################################
# Plot PPG vs. FG%
#################################################################################
ggplot(data = playedMins, 
       
       # PLot PPG VS FG %
       aes(x = playedMins$PPG,
           y = playedMins$`FG%`)) + 
  
  # Make Scatterplot
  geom_point() +
  
  # Hi Percentage Volume Scorers
  
  geom_text(aes(label=ifelse((PPG>20 & playedMins$`FG%` > 0.5),
                             as.character(Player), '')),
            hjust=0, 
            vjust=-0.5,
            size = 3,
            color = "Blue") +
  
  # Add Labels and Title
  xlab("Points per Game") +
  ylab("Field Goal Percentage") + 
  ggtitle("Points per Game vs. Field Goal Percentage : 12-19-18")