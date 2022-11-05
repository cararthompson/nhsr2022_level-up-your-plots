# Libraries
library(tidyverse)

# Setting up the data ----
penguins <- palmerpenguins::penguins %>%
  # To vary the banana quantity (if you're reading this before the workshop, 
  # trust me this will make sense...!)
  mutate(banana_quantity = case_when(species == "Adelie" & island == "Biscoe" ~ 1,
                                     species == "Adelie" & island == "Dream" ~ 0.5,
                                     species == "Adelie" & island == "Torgersen" ~ 0.1,
                                     TRUE ~ 1))

# Building the plot ----
ggplot(penguins,
       aes(x = bill_depth_mm,
           y = bill_length_mm,
           colour = species)) +
  geom_point(aes(alpha = banana_quantity)) +
  labs(title = "Banana loaf tastes best when baked with ripe or over-ripe bananas",
       subtitle = "The Palmer Penguins carried out an experiment using bananas of different ripeness.
The Adelie penguins were given unripe bananas, Gentoos were given over-ripe 
bananas and Chinstraps were given yellow bananas.
Each penguin was left to choose their own cooking time.",
       x = "Baking time",
       y = "Yumminess",
       caption = "Data from {palmerpenguins}; misused for illustration purposes.")

