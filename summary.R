library("dplyr")
library("tidyverse")

spl_df <- read.csv("~/Desktop/2022-2023-All-Checkouts-SPL-Data.csv",
  stringsAsFactors = FALSE)



# 1 Number of checkouts for each item (digital vs physical)
type_checkouts <- spl_df %>%
  group_by(UsageClass) %>%
  summarize(num_checkouts = sum(Checkouts, na.rm = TRUE))


# Month with the most & least checkouts for a book that you're interested in?
# Mockingjay (Year: 2022-23)

# 2a Month with most checkouts of book: Mockingjay
month_most_checkouts <- spl_df %>%
  filter(str_detect(Creator, "Suzanne")) %>%
  filter(str_detect(Creator, "Collins")) %>%
  filter(str_detect(Title, "Mockingjay")) %>%
  group_by(CheckoutMonth) %>%
  summarize(total_checkouts = sum(Checkouts, na.rm = TRUE)) %>%
  filter(total_checkouts == max(total_checkouts, na.rm = T)) %>%
  pull(CheckoutMonth)

# 2b Month with least checkouts of book: Mockingjay
month_least_checkouts <- spl_df %>%
  filter(str_detect(Creator, "Suzanne")) %>%
  filter(str_detect(Creator, "Collins")) %>%
  filter(str_detect(Title, "Mockingjay")) %>%
  group_by(CheckoutMonth) %>%
  summarize(total_checkouts = sum(Checkouts, na.rm = TRUE)) %>%
  filter(total_checkouts == min(total_checkouts, na.rm = T)) %>%
  pull(CheckoutMonth)


# Month with the most/least checkouts for ebooks

# 3a Month with most ebook checkouts
most_ebook_checkout_month <- spl_df %>%
  filter((str_detect(MaterialType, "EBOOK"))) %>%
  group_by(CheckoutMonth) %>%
  summarize(total_checkouts = sum(Checkouts, na.rm = TRUE)) %>%
  filter(total_checkouts == max(total_checkouts, na.rm = T)) %>%
  pull(CheckoutMonth)

# 3b Month with least ebook checkouts
least_ebook_checkout_month <- spl_df %>%
  filter((str_detect(MaterialType, "EBOOK"))) %>%
  group_by(CheckoutMonth) %>%
  summarize(total_checkouts = sum(Checkouts, na.rm = TRUE)) %>%
  filter(total_checkouts == min(total_checkouts, na.rm = T)) %>%
  pull(CheckoutMonth)


# 4 How has the number of print book checkouts changed over time?
# Difference of print book checkouts from January 2022 to January 2023

spl_df <- spl_df %>% mutate(date = paste0(CheckoutYear, "-", CheckoutMonth, "-01"))

spl_df$date <- as.Date(spl_df$date, format = "%Y-%m-%d")

january_2022_checkouts <- spl_df %>%
  filter(str_detect(MaterialType, "BOOK")) %>%
  group_by(date) %>%
  summarize(number_of_checkouts = sum(Checkouts, na.rm = TRUE)) %>%
  filter(str_detect(date, "2022-01-01")) %>%
  pull(number_of_checkouts)

january_2023_checkouts <- spl_df %>%
  filter(str_detect(MaterialType, "BOOK")) %>%
  group_by(date) %>%
  summarize(number_of_checkouts = sum(Checkouts, na.rm = TRUE)) %>%
  filter(str_detect(date, "2023-01-01")) %>%
  pull(number_of_checkouts)

# Final: Difference of print book checkouts from 2022 to 2023
difference <- january_2022_checkouts - january_2023_checkouts


# 5 Total number of checkouts from 2022-23
total_num_checkouts <- spl_df %>%
  summarize(number_of_checkouts = sum(Checkouts, na.rm = TRUE))
