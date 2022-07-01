
# Setup -------------------------------------------------------------------

# make folders if they don't already exist
required_dirs <- c('data','data/raw_data', 'data/processed_data', 
                   'figures', 'results')
for(d in (required_dirs)){
  if(!dir.exists(d)){
    dir.create(d)
  }
}

# Code by Dave Hemprich-Bennett, leaning heavily 
# on the book R for Data Science https://r4ds.had.co.nz/relational-data.html

# read in packages

library(dplyr)
library(readr)
library(magrittr)
library(readxl)
library(janitor)

## julia should put some nice comments here, ah well

cand_df <- read_csv('data/raw_data/Candidate_nos_B_C.csv') %>%
  rename(candidate_nr = `Candidate no`) %>%
  mutate(candidate_nr = as.character(candidate_nr))

b16 <- read_excel('data/raw_data/adjusted_marks_altering/B16.xlsx') %>%
  rename(candidate_nr = `Candidate nr`)
c18 <- read_excel('data/raw_data/adjusted_marks_altering/C18.xlsx') %>%
  rename(candidate_nr = `Candidate nr`)


b16_ordered <- cand_df %>%
  filter(paper == 'B') %>%
  select(candidate_nr) %>%
  left_join(b16)

write_csv(b16_ordered, path = 'data/processed_data/b16_ordered.csv')


c18_ordered <- cand_df %>%
  filter(paper == 'C') %>%
  select(candidate_nr) %>%
  left_join(c18)

write_csv(c18_ordered, path = 'data/processed_data/c18_ordered.csv')
