library("ggplot2")
library("tidyverse")
library("dplyr")
library("scales")

spl_df <- read.csv("~/Desktop/2022-2023-All-Checkouts-SPL-Data.csv",
                   stringsAsFactors = FALSE)

spl_df <- spl_df %>% mutate(date = paste0(CheckoutYear, "-", CheckoutMonth, "-01" ))
spl_df$date <- as.Date(spl_df$date, format = "%Y-%m-%d")


collins_df <- spl_df %>%
  filter(str_detect(Creator, "Suzanne")) %>% 
  filter(str_detect(Creator, "Collins"))

type_checkouts <- collins_df %>%
  group_by(MaterialType, date) %>%
  summarize(num_checkouts = sum(Checkouts, na.rm = TRUE))

ggplot(type_checkouts) +
  geom_line(aes(
    x = date,
    y = num_checkouts,
    color = MaterialType
  )) +
  geom_point(aes(
    x = date,
    y = num_checkouts,
    color = MaterialType
  )) +
  labs(
    title = "Suzzane Collins SPL Checkouts from 2022-23", 
    x = "Month", 
    y = "Number of Checkouts",
    color = "Material Type"
  )