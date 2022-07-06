
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

library(tidyverse)
library(readxl)
library(janitor)

# make a vector of all file paths in directory, matching pattern
marksheet_paths <- list.files(path = 'data/raw_data',
                              pattern = 'Marksheet.xlsx',
                              recursive = T,
                              include.dirs = T, 
                              full.names = T)

# # remove b8 and b19 and b17
#marksheet_paths <- marksheet_paths[-grep('B8|B19|B17|B18', marksheet_paths)]


# as formatting has been an issue with reading in some files, because
# some had incorrectly named sheets, print the name of each file and give
# an error immediately after if it fails
for(f in 1:length(marksheet_paths)){
  print(marksheet_paths[f])
  read_excel(marksheet_paths[f], sheet = 'Marks')
}

# read all of the files at the above paths
spreadsheets_list <- lapply(marksheet_paths, 
                            function(x) 
                              read_excel(x, sheet = 'Marks'))


# Tidy up the spreadsheets ------------------------------------------------

# make a list to save the new, bare-bones spreadsheets into
cleaned_spreadsheets <- list()
# using what's known as a 'for' loop, do the same thing to 
# all of the files in spreadsheets_list, and save the altered
# sheets into the list cleaned_spreadsheets
for(i in 1:length(spreadsheets_list)){
  # get the files name ('path')
  full_path <- marksheet_paths[i]
  # remove the full directory and file name from the path, leaving
  # just the paper name
  paper_name <- gsub('.+/| .+', '', full_path)
  
  # select the spreadsheet to work on
  current_sheet <- spreadsheets_list[[i]] %>%
    clean_names()
  
  # remove the first 9 rows because they're in the way
  current_sheet <- current_sheet[-seq(1:9),]
  
  # Tidy stuff, save it to the list cleaned_spreadsheets
  cleaned_spreadsheets[[i]] <- current_sheet %>%
    # rename an annoying column name
    rename(candidate_no = cand_no_4) %>%
    # select only the columns that we want
    select(candidate_no, raw_mark, raw_score, adjusted_mark) %>%
    # make a column for the paper name and a column for if its a b or c paper
    mutate(paper = paper_name) %>%
    mutate(paper_series = gsub('[0-9]|E', '', paper_name)) %>%
    # remove any rows where the candidate number is '0' as these rows are
    # excel being a bellend, not real data
    filter(candidate_no != 0) %>%
    # also filter out any na rows
    filter(!is.na(candidate_no)) %>%
    # filter out any rows with the codes that aren't wanted here
    filter(!candidate_no %in% c("DESA", "DECA", "DECB",
                                   "DEEA", "VESB"))
  
}

# combine all of the tidied spreadsheets into one sheet to rule them all
combined_sheet <- bind_rows(cleaned_spreadsheets)

# any items in the below data frame are duplicates
duplicates <- combined_sheet %>%
  select(candidate_no, paper) %>%
  filter(duplicated(.))

if(nrow(duplicates) > 0){
  write_csv(duplicates, path  = 'data/processed_data/duplicate_ids.csv')
}

# filter to retain only rows which have a value of 'na' in any of the columns.
# This is to allow us to identify and debug any problems
badrows <- combined_sheet %>%
  filter(is.na(raw_mark)| is.na(raw_score)|
           is.na(candidate_no)| is.na(adjusted_mark)) 

write_csv(badrows, path = 'data/processed_data/missing_values.csv')

badrows %>%
  group_by(paper) %>%
  summarise(n_problem_rows = n()) %>%
  write_csv(path = 'results/n_problem_rows.csv')


# Getting candidate number orders -----------------------------------------

cand_df <- read_csv('data/raw_data/candidate_nos.csv') %>%
  mutate(student_no = as.character(student_no))

# Organise and save the data ---------------------------------------------


exam_types <- c('B', 'C')

for(i in 1:length(exam_types)){
  
  given_exam <- exam_types[i]
  
  # get the candidate numbers of the paper in question
  candidates <- cand_df %>%
    filter(paper == given_exam) %>%
    select(student_no)
  
  
  # A few commands doing subtle variations on the same thing, so I've only
  # commented the first set for ease
  
  rawmarks <- candidates %>%
    # combine the candidate numbers that were read in above and then filtered
    # for this set of exams with the results for that letter B/C
    left_join(combined_sheet, by = c('student_no' = 'candidate_no')) %>% 
    # retain only the columns that we need
    select(student_no, raw_mark, paper) %>%
    # transform the data from 'wide' format into long format
    pivot_wider(names_from = paper, values_from = raw_mark)
  
  # save it as a csv
  write_csv(x = rawmarks,
            # automatically generate a filepath for it based on 
            # the variables in this loop iteration
            path = paste0('data/processed_data/', given_exam, '_rawmarks.csv'), 
            # include zero characters (there are zero characters between the ''
            # instead of NAs in the output csv)
            na ='')
  
  
  
  rawscores <- candidates %>%
    left_join(combined_sheet, by = c('student_no' = 'candidate_no')) %>% 
    select(student_no, raw_score, paper) %>%
    pivot_wider(names_from = paper, values_from = raw_score)
  
  write_csv(x = rawscores,
            path = paste0('data/processed_data/', given_exam, '_rawscores.csv'), 
            na ='')
  
  
  
  adjmarks <- candidates %>%
    left_join(combined_sheet, by = c('student_no' = 'candidate_no')) %>% 
    select(student_no, adjusted_mark, paper) %>%
    pivot_wider(names_from = paper, values_from = adjusted_mark)
  
  write_csv(x = adjmarks,
            path = paste0('data/processed_data/', given_exam, '_adjmarks.csv'), 
            na ='')
}


# Dave playing with summary stats -----------------------------------------



long_data <- combined_sheet %>%
  pivot_longer(c(raw_mark, raw_score, adjusted_mark), names_to = 'value_type')

all_distribution <- ggplot(long_data, aes( y = value, x = paper, fill = paper_series)) +
  geom_boxplot()+
  theme_bw() + 
  facet_wrap(. ~ value_type, ncol = 1, scales = 'free') + 
  scale_fill_viridis_d()
ggsave('figures/boxplot.pdf', all_distribution)




# Make summaries of what modules each student took ------------------------

papers_sat <- combined_sheet %>%
  # grab only the columns we want
  select(candidate_no, paper_series, paper, ) %>%
  # make a duplicate column for when we pivot_wider
  mutate(fill_col = paper) %>%
  # make the data wide, rather than long
  pivot_wider(names_from = paper, values_from = fill_col) %>%
  unite(col = temp_col, -c(candidate_no, paper_series), sep = ',') %>%
  # we now have a column with all the papers that candidate sat, but 
  # a tonne of unwanted NAs and columns. Lets fix that
  mutate(temp_col = gsub('NA,|NA', '', temp_col)) %>%
  # now separate that one column out into 6 columns
  separate(temp_col, into = paste('paper', seq(1,6), sep = '_'),
           sep = ',')

write_csv(papers_sat, path = 'data/processed_data/papers_sat.csv')


three_yp <- read_excel('data/raw_data/3YP.xlsx') %>%
  rename(student_no = `Candidate No`) %>%
  mutate(student_no = as.character(student_no))

cand_df %>%
  filter(paper == 'B') %>%
  select(student_no) %>%
  left_join(three_yp) %>%
  write_csv('data/processed_data/3yps_ordered.csv',
            na ='')
  
lab_marks <- read_excel('data/raw_data/Lab marks B.xlsx') %>%
  rename(candidate_no = `Candidate nr`)


cand_df %>%
  filter(paper == 'B') %>%
  select(student_no) %>%
  left_join(lab_marks, by = c('student_no' = 'candidate_no' )) %>%
  write_csv('data/processed_data/lab_marks_ordered.csv',
            na = '')
