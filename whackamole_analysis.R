library(plotly)
library(tidyverse)
options("digits.secs"=6)

fig <- plot_ly() %>%
  config(scrollZoom = TRUE, displaylogo = FALSE, modeBarButtonsToRemove = c("pan2d","select2d","hoverCompareCartesian", "toggleSpikelines","zoom2d","toImage", "sendDataToCloud", "editInChartStudio", "lasso2d", "drawclosedpath", "drawopenpath", "drawline", "drawcircle", "eraseshape", "autoScale2d", "hoverClosestCartesian","toggleHover", "")) %>%
  layout(dragmode = "pan", showlegend=T, xaxis=list(mirror=T, ticks='outside', showline=T), yaxis=list(mirror=T, ticks='outside', showline=T))

load('data_whack.rda')

#############
# Summaries
#############

#############
# Number of participants in June
#############

nbPlayer = D %>%
  summarize(Participant = unique(Participant)) %>%
  count()

#############
#  2) For how long time did each participant play?
#############
 TimeSpend = D %>%
    select(Participant, GameTimeSpent) %>%
    filter(GameTimeSpent!="NA") %>%
    group_by(Participant)%>%
    summarise(sum(as.integer(GameTimeSpent))) %>%
    rename(GameTimeSpentS = "sum(as.integer(GameTimeSpent))")%>%
    mutate(GameTimeSpentH=GameTimeSpentS/3600 )
  
#############
# 3) What were the participants’ mean world gaze position?
#############

  GazePosition = D %>%
    select(Participant,contains("WorldGazeHitPosition"))  %>%
    filter(WorldGazeHitPositionX!="NA") %>%
    group_by(Participant)%>%
    summarize(MeanGazePositionX= mean(as.numeric(WorldGazeHitPositionX), na.rm=TRUE),
              MeanGazePositionY= mean(as.numeric(WorldGazeHitPositionY), na.rm=TRUE),
              MeanGazePositionZ= mean(as.numeric(WorldGazeHitPositionZ), na.rm=TRUE))
   
#############
# 4) What was the lowest, highest and mean gaze confidence for each participant?
#############
  GazeConfidence = D %>%
    select(Participant, GazeConfidence)%>%
    filter(GazeConfidence!="NA")%>%
    group_by(Participant)%>%
    summarize(LowestGazeConfidence= min(as.numeric(GazeConfidence), na.rm=TRUE),
              HighestGazeConfidence= max(as.numeric(GazeConfidence), na.rm=TRUE),
              MeanGazeConfidence= mean(as.numeric(GazeConfidence), na.rm=TRUE))  

#############
# 5) Please take a look at the “saccades” package for R and investigate how we can use it for the Whack-A-Mole eye tracking data.
#############

 
  
  
  
  
  
  
  
