library(tidyverse)
library(plotly)



nbPlayer = D %>%
  summarize(ParticipantId = unique(ParticipantId)) %>%
  count()
