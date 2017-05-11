library(tidyverse)
library(lubridate)

df <- readxl::read_excel("pullup_challenge.xlsx",col_names = TRUE)

pullups <- df %>% gather(key = date, value = pullups, 5:25)

pullups <- pullups %>% within({
  date <- parse_date_time(date, "%m/%d/%Y")
  name <- factor(name)
  group <- factor(group)
})

#individual total
pullups %>%
  group_by(name) %>%
  summarise(total = sum(pullups)) %>%
  arrange(desc(total))

# total by group
pullups %>%
  group_by(group) %>%
  summarise(total = sum(pullups)) %>%
  arrange(desc(total))

# total by group by day
pullups %>%
  group_by(date, group) %>%
  summarise(total = sum(pullups),
            average_per_person = mean(pullups)) %>%
  arrange(desc(total)) %>%
  ggplot(aes(x = date, y = total, color = group))+
  geom_line()

mean(pullups$pullups, na.rm = TRUE)
sd(pullups$pullups, na.rm = TRUE)
