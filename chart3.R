library("ggplot2")
library("tidyverse")
library("dplyr")
library("scales")

spl_df <- read.csv("2022-2023-All-Checkouts-SPL-Data.csv",
                   stringsAsFactors = FALSE)

collins_df <- spl_df %>%
  filter(str_detect(Creator, "Suzanne")) %>% 
  filter(str_detect(Creator, "Collins"))

modify_x_labels <- c("Catching Fire", "Mockingjay")

# Distinguish between titles: 'Catching Fire' & 'Mockingjay'
catching_fire_df <- collins_df %>% 
  filter(str_detect(Title, "(?i)Catching Fire") | str_detect(Title, "(?i)Mockingjay")) %>%
  mutate(has_mockingjay = str_detect(Title, "Mockingjay")) %>%
  group_by(has_mockingjay, MaterialType) %>%
  summarize(total_checkouts = sum(Checkouts, na.rm = TRUE))

ggplot(catching_fire_df) +
  geom_col(aes(
    x = has_mockingjay,
    y = total_checkouts,
    color = MaterialType
  )) + 
  scale_x_discrete(labels = modify_x_labels
  ) +
  labs(
    title = "Comparing 'Catching Fire' & 'Mockingjay' Checkout Material Type (2022-23)",
    x = "Book", 
    y = "Total Number of Checkouts",
    color = "Material Type"
  ) +
  scale_y_continuous(breaks = seq(0, 1100, 100
  ))
