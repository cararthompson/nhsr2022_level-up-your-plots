# Libraries
library(tidyverse)

# Setting up the data ----
penguins <- palmerpenguins::penguins %>%
  filter(!is.na(bill_length_mm)) %>%
  mutate(banana_quantity = case_when(species == "Adelie" & island == "Biscoe" ~ 1,
                                     species == "Adelie" & island == "Dream" ~ 0.5,
                                     species == "Adelie" & island == "Torgersen" ~ 0.1,
                                     TRUE ~ 1))

# Creating a colour scheme ----
banana_colours <- list("Adelie" = "#89973d",
                       "Chinstrap" = "#e8b92f",
                       "Gentoo" = "#a45e41")

# Building the plot ----
ggplot(penguins,
       # aesthetics that apply to all sets of data
       aes(x = bill_depth_mm,
           y = bill_length_mm,
           colour = species)) +
  geom_point(aes(alpha = banana_quantity)) +
  scale_colour_manual(values = banana_colours) +
  scale_alpha(range = c(0.2, 0.9)) +
  labs(title = paste0("Banana loaf tastes best when baked with ripe or over-ripe bananas"),
       subtitle = "The Palmer Penguins carried out an experiment using bananas of different ripeness.
The Adelie penguins were given unripe bananas, Gentoos were given over-ripe 
bananas and Chinstraps were given yellow bananas.
Each penguin was left to choose their own cooking time.",
       x = "Baking time",
       y = "Yumminess",
       caption = "Data from {palmerpenguins}; misused for illustration purposes.") +
  theme_minimal(base_size = 12)
