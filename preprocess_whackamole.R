library(gsheet)
library(tidyverse)
source("utils/loadrawdata.R")

options("digits.secs"=6)
# Load data from directories
D <- LoadFromDirectory("data")

save(D, file = 'data_whack_raw.rda', compress=TRUE)
#load('data_whack_raw.rda')

# Load data from Google Sheets
L <- gsheet2tbl('https://docs.google.com/spreadsheets/d/1k0Z2F5E3WeDRdzlxepo-tuQasdhFwQRGoYVBtmXHdWA/edit#gid=237988817')

#############
# Format D
#############
D <- D %>% rename(Condition = i2, Participant = i1)

D <- D %>% mutate(Participant = as.numeric(Participant))

D = D %>% mutate(Timestamp = as.POSIXct(Timestamp, format = "%Y-%m-%d %H:%M:%OS"),
                 Framecount = as.integer(Framecount)) %>%
  arrange(Timestamp)

#############
# Merge
#############
D <- D %>% left_join(L, by=c('Condition' = 'Condition', 'Participant' = 'Participant'))

#############
# Save to RDA
#############
# Split into 4 

save(D %>% , file = 'data_whack1.rda', compress=TRUE)
