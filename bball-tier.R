# Basketball Tier List
# data : /Users/emreyurtbay/Documents/Datasets/tiers-bball/FantasyPros_2018_Overall_NBA_Rankings.csv
# Code Based off Boris Chen's, using Tidyverse commands  
################################################################################
# Import Data and Clean
################################################################################

library(tidyverse)
library(mclust)
basketball <- read_csv("/Users/emreyurtbay/Documents/Datasets/tiers-bball/FantasyPros_2018_Overall_NBA_Rankings.csv")

# Remove NA values
basketball <- basketball %>% drop_na()

# Number of Agglomerated Expert picks
num_experts = 6

# get the top 50 players
basketball_top <- filter(basketball, basketball$Rank <= 50)


#################################################################################
# Clustering
#################################################################################
k = 9 # Number of Clusters desired
clusterDF <- select(basketball_top, Avg)

# Run clustering algorithm
mod <- Mclust(clusterDF, G = k)
summary(mod)

# Add the tier to the Player
basketball_top <- mutate(
  basketball_top,
  
  # Get classification
  Tier = as.factor(mod$classification)
)

##################################################################################
# Plotting
##################################################################################
plt <- ggplot(data = basketball_top,
              
              # Plot the Consensus Rank vs the Average Rank
              aes(x = -basketball_top$Rank, 
                  y = basketball_top$Avg,
                  
                  # Color based upon Tier
                  color = basketball_top$Tier))+
 
   # Point Plot
  geom_point()+
  
  # Add Player Name
  geom_text(aes(label = basketball_top$Player),
            
            # Fix Where a player's name should be placed on the plot
            # nchar counts the amount of characters in a string
            y = (basketball_top$Avg + 1.96*basketball_top$`Std Dev`/sqrt(num_experts) + nchar(basketball_top$Player)/2.7), 
            
            # Font size 
            size = 3)+
  
  # Add error bars based off the Formula for confidence interval
  geom_errorbar(aes(ymin = basketball_top$Avg - 1.96*basketball_top$`Std Dev`/sqrt(num_experts), 
                    ymax = basketball_top$Avg + 1.96*basketball_top$`Std Dev`/sqrt(num_experts),
                    
                    # Fix the width of the end bars
                    width = 0.2),
                
                # Fix the width of the bar
                size = 0.8)+
  
  # Add an x and y axis label
  ylab("Expert Average Pick")+
  xlab("Expert Consensus Pick")+
  
  # Fix legend
  theme(legend.justification=c(1.5,1.25), legend.position=c(1,1))+
  
  # Fix axis length so we can see all of the names
  ylim(c(0, 70))+
  
  # Color palette based off of Tier
  scale_colour_discrete(name="Tier")+
  
  # Flip X and Y in the final graph
  coord_flip()+
  
  # Add a title
  ggtitle("NBA Fantasy Leaders (All Players) : Thursday, December 27th")

# Print Plot to Screen
plt
