library(tidyverse)


marks <- read_csv('data/raw_data/c25_and_c29.csv')

summary_stats <- marks %>%
  pivot_longer(cols = c(C29, C25),
               names_to = 'paper',
               values_to = 'mark') %>%
  filter(!is.na(mark)) %>%
  group_by(paper) %>%
  summarise(mean_mark = mean(mark),
            median_mark = median(mark))
summary_stats  

write_csv(summary_stats, 'results/c25_and_c29_summary_stats.csv')
