library(tidyverse)
library(lubridate)
library(ggplot2)
library(knitr)
library(forcats)
library(viridis)


df <- readxl::read_excel("pullup_challenge.xlsx",col_names = TRUE)

pullups <- df %>% gather(key = date, value = pullups, 5:25)

pullups <- pullups %>% within({
  date <- as_date(date, "%m/%d/%Y")
  name <- factor(name)
  group <- factor(group)
})

#individual total
sum_per_person <- pullups %>%
  group_by(name) %>%
  summarise(total = sum(pullups)) %>%
  arrange(desc(total))

# total by group
group_total <- pullups %>%
  group_by(group) %>%
  summarise(total = sum(pullups)) %>%
  arrange(desc(total))

# total by group by day
group_total_by_day <- pullups %>%
  group_by(date, group) %>%
  summarise(total = sum(pullups),
            average_per_person = mean(pullups)) %>%
  arrange(desc(total)) %>%
  ungroup()

