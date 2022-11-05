# Libraries
library(tidyverse)

# Setting up the data ----
penguins <- palmerpenguins::penguins %>%
  filter(!is.na(bill_length_mm)) %>%
  mutate(banana_quantity = case_when(species == "Adelie" & island == "Biscoe" ~ 1,
                                     species == "Adelie" & island == "Dream" ~ 0.5,
                                     species == "Adelie" & island == "Torgersen" ~ 0.1,
                                     TRUE ~ 1))

penguin_summaries <- palmerpenguins::penguins %>%
  group_by(species) %>%
  summarise(bill_depth_mm = mean(bill_depth_mm, na.rm = TRUE),
            bill_length_mm = mean(bill_length_mm, na.rm = TRUE)) %>%
  mutate(commentary = case_when(species == "Adelie" ~
                                  "The Adelie penguins tried varying the amount of banana in the mix. Turns out, even a hint of green banana is detrimental to yumminess!",
                                species == "Gentoo" ~
                                  "Over-ripe bananas and typically shorter baking times.",
                                TRUE ~ "Ripe bananas and slightly longer cooking times."))


# Establish a colour scheme ----
banana_colours <- list("Adelie" = "#89973d",
                       "Chinstrap" = "#e8b92f",
                       "Gentoo" = "#a45e41")


# |- Build on it to add light and dark text colours ----
dark_text <- monochromeR::generate_palette(
  banana_colours$Chinstrap, "go_darker",
  n_colours = 2)[2]

light_text <-  monochromeR::generate_palette(
  dark_text, "go_lighter",
  n_colours = 3)[2]

banana_colours <- list("Adelie" = "#89973d",
                       "Chinstrap" = "#e8b92f",
                       "Gentoo" = "#a45e41",
                       "dark_text" = dark_text,
                       "light_text" = light_text)

# Plot it! ----
ggplot(penguins,
       # aesthetics that apply to all sets of data
       aes(x = bill_depth_mm,
           y = bill_length_mm,
           colour = species)) +
  geom_point(aes(alpha = banana_quantity)) +
  ggtext::geom_textbox(data = penguin_summaries,
                       aes(label = paste0("**Team ", species, "**",
                                          "<br><span style = \"color:",
                                          banana_colours$light_text,
                                          "\">", commentary, "</span>")),
                       family = "Cabin",
                       size = 4,
                       width = unit(12, "line"),
                       alpha = 0.9,
                       box.colour = NA) +
  scale_colour_manual(values = banana_colours) +
  scale_alpha(range = c(0.2, 0.9)) +
  labs(title = paste0("Banana loaf tastes best when baked with <span style=\"color:", 
                      banana_colours$Chinstrap, "\">ripe</span> or<br><span style=\"color:", 
                      banana_colours$Gentoo, "\">over-ripe</span> bananas"),
       subtitle = "The Palmer Penguins carried out an experiment using bananas of different ripeness. \nEach penguin was left to choose their own cooking time.",
       x = "Baking time",
       y = "Yumminess",
       caption = "Data from {palmerpenguins}; misused for illustration purposes.") +
  guides("none") +
  theme_minimal(base_size = 12) +
  theme(text = element_text(family = "Cabin", colour = banana_colours$light_text),
        plot.title = ggtext::element_markdown(size = 18, family = "Poppins", 
                                              colour = banana_colours$dark_text, face = "bold"),
        panel.grid = element_line(colour = "#F6F6F5"),
        axis.text = element_text(size = 6, 
                                 colour = banana_colours$light_text),
        plot.caption = element_text(size = 6),
        legend.position="none")

# Exporting the plot ----
ggsave(filename = "penguin-bakeoff.png", 
       width = 8.5, height = 7.5, dpi = 400, bg = "#FFFFFF")

