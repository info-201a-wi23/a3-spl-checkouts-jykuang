library("ggplot2")
library("tidyverse")
library("dplyr")
library("scales")

spl_df <- read.csv("~/Desktop/2022-2023-All-Checkouts-SPL-Data.csv",
                   stringsAsFactors = FALSE)

spl_df <- spl_df %>% mutate(date = paste0(CheckoutYear, "-", CheckoutMonth, "-01"))
spl_df$date <- as.Date(spl_df$date, format = "%Y-%m-%d")


usage_class_checkouts <- spl_df %>%
  group_by(UsageClass, date) %>%
  summarize(number_of_checkouts = sum(Checkouts, na.rm = TRUE))

ggplot(usage_class_checkouts) +
  geom_line(aes(
    x = date,
    y = number_of_checkouts,
    color = UsageClass
  )) +
  geom_point(aes(
    x = date,
    y = number_of_checkouts,
    color = UsageClass
  )) +
  scale_y_continuous(labels = label_number_si()) +
  labs(
    title = "SPL Physical vs. Digital Usage Checkouts (2022-23)",
    x = "Month",
    y = "Number of Checkouts",
    color = "Item Usage Checkout"
  )