library(hexSticker)
library(tidyverse)
library(touringplans)
library(broom)

seven_dwarfs <- seven_dwarfs_train_2018 %>%
  filter(wait_hour == 17)

seven_dwarfs_with_ps <- glm(
  park_extra_magic_morning ~ park_ticket_season + park_close + park_temperature_high,
  data = seven_dwarfs,
  family = binomial()
) %>%
  augment(type.predict = "response", data = seven_dwarfs)
p <- ggplot() +
  geom_histogram(
    data = seven_dwarfs_with_ps %>% filter(park_extra_magic_morning == 1),
    aes(x = .fitted),
    fill = "#533146",
    bins = 40
  ) +
  geom_histogram(
    data = seven_dwarfs_with_ps %>% filter(park_extra_magic_morning == 0),
    aes(
      x = .fitted,
      y = -after_stat(count)
    ),
    fill = "#C2BEB6",
    bins = 40
  ) +
  labs(x = "",
       y = "") +
  theme_minimal() +
  theme(legend.position = "none",
        axis.text = element_blank(),
        panel.grid = element_blank())

sticker(p,
        package = "STA 779", 
        p_size = 15,
        s_x = 1,
        s_y = .75,
        s_width = 2,
        s_height = 1.2,
        filename = "images/icon.png",
        h_color = "#533146",
        h_fill = "#F4F7FF",
        p_color = "#533146")
