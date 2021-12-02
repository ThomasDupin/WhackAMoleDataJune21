library(plotly)
library(tidyverse)
library("devtools")
install_github("tmalsburg/saccades/saccades", dependencies=TRUE, force=TRUE)
library(saccades)
library(rgl)
library(reshape2)
library(tidyverse)
library("devtools")
install_github("tidymodels/tidymodels")
library(tidymodels)

library(plotly)
library(kernlab)
library(pracma) #For meshgrid()²
library(lubridate)
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

SessionStart = D %>%
  select(Participant, SessionID, Event, Timestamp) %>%
  filter(Event =="Game Started") 

SessionEnd = D %>%
  select(Participant, SessionID, Event, Timestamp) %>%
  filter(Event =="Game Finished") 

PlayTime = merge(SessionStart,SessionEnd,"SessionID") %>%
  mutate(Duration = difftime(Timestamp.y,Timestamp.x )) %>%
  group_by(Participant.x)%>%
  summarise(timePlayed=sum(Duration))%>%
  rename(Participant = "Participant.x")
  
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

#############
# A) How high frequency is the eye tracker running at? Hz = Samples per second.
#############

############
# Change the time value, from 0 sec at the first event to 2min at the last
###########

SessionTimeChange = D %>%
  select(Participant, SessionID, Event, Timestamp, Condition) %>%
  filter(SessionID=="7012cac88a9dab2d7acee4480cd1014e")



SessionStart = D %>%
  select(Participant, SessionID, Event, Timestamp) %>%
  filter(Event =="Game Started",SessionID=="7012cac88a9dab2d7acee4480cd1014e" ) 


SessionEnd = D %>%
  select(Participant, SessionID, Event, Timestamp) %>%
  filter(Event =="Game Finished",SessionID=="7012cac88a9dab2d7acee4480cd1014e") 



DateRange <- SessionTimeChange[SessionTimeChange$Timestamp > '2021-06-29 13:46:56.7651' & SessionTimeChange$Timestamp < '2021-06-29 13:48:58.6758', ]

DataHz = DateRange %>%
  summarise(Participant,
            SessionID,
            Event,
            Timestamp = difftime( Timestamp, SessionStart$Timestamp),
            Condition)%>%
  mutate(time= format(Timestamp, format = "%M:%S"))



# Use time stamp to calculate how long a test took

DateRange$Timestamp <- as.POSIXlt(DateRange$Timestamp, format = "%m/%d/%Y %H:%M:%S:%OS")




# convert the timer into time in seconds
DataHz$time <- format(DateRange$Timestamp, format = "%H:%M:%S:%OS")

DataHz$time <- round(DateRange$Timestamp$sec, 0) + DateRange$Timestamp$min * 60 + DateRange$Timestamp$hour * 3660

DateHz = DataHz %>% 
  filter(Event == "Game Started")%>% 
    select(SessionID, Timestamp) %>% rename(Timestamp_start = Timestamp) %>%
    right_join(DataHz) %>%
    mutate(time_since_start = time - Timestamp_start)
 


############
# calculate the HZ
###########

SampleHzSession = D %>%
  select(Participant, SessionID, Event,Condition, Timestamp) %>%
    filter(Event =="Sample")%>%
    mutate(minute= format(Timestamp, format = "%M:%S"))%>%
    group_by(SessionID, minute)%>%
    summarise(
              Participant= Participant,
              Hz_Rate = sum(Event=="Sample"),
              Condition = Condition
              )%>%
  group_by(SessionID) %>%
  summarise(Hz= mean(Hz_Rate),
            Participant= Participant,
            Condition= Condition)


SampleHz <- unique(SampleHzSession)

###############
#Below for one session
##################
 
SampleHzOneSession = D %>%
  select(Participant, SessionID, Event,Condition, Timestamp) %>%
  filter(Event =="Sample", SessionID=="057864beeebb3ecb4d1cb631914c376a")%>%
  mutate(minute= format(Timestamp, format = "%M:%S"))%>%
  group_by(minute)%>%
  summarise(
    Participant= Participant,
    Hz_Rate = sum(Event=="Sample")
  )
  

SampleHzOneSessionUnique <- unique(SampleHzOneSession)


############
# Graph with Hz over time for a participant
###########


DisplayHz <- SampleHzOneSessionUnique 


# One trace (more performant, but less interactive)
DisplayHz %>%
  plot_ly(x = ~minute(m), y = ~Hz_Rate, type = "scatter", mode="line")
  

# Multiple traces (less performant, but more interactive)
plot_ly(DisplayHz, x = ~minute, y = ~Hz_Rate) %>%
  add_lines(color = ~ordered(SessionID))

# The split argument guarantees one trace per group level (regardless 
# of the variable type). This is useful if you want a consistent
# visual property over multiple traces 
# plot_ly(econ, x = ~mnth, y = ~uempmed) %>%
#   add_lines(split = ~yr, color = I("black"))





#########################
# NOT WORKING AT ALL
########################


SessionTimeChange = D %>%
  select(Participant, SessionID, Event, Timestamp, Condition) %>%
  filter(SessionID=="7012cac88a9dab2d7acee4480cd1014e")



SessionStart = D %>%
  select(Participant, SessionID, Event, Timestamp) %>%
  filter(Event =="Game Started",SessionID=="7012cac88a9dab2d7acee4480cd1014e" ) 


SessionEnd = D %>%
  select(Participant, SessionID, Event, Timestamp) %>%
  filter(Event =="Game Finished",SessionID=="7012cac88a9dab2d7acee4480cd1014e") 



DateRange <- SessionTimeChange[SessionTimeChange$Timestamp > '2021-06-29 13:46:56.7651' & SessionTimeChange$Timestamp < '2021-06-29 13:48:58.6758', ]

DataHz = DateRange %>%
  summarise(Participant,
            SessionID,
            Event,
            Timestamp = difftime( Timestamp, SessionStart$Timestamp),
            Condition)%>%
  mutate(time= format(Timestamp, format = "%S"))


SessionHz <- DataHz%>%
  mutate(minute= format(time, format = "%M:%S"))%>%
  group_by(minute)%>%
  summarise(
    Hz_Rate = sum(Event=="Sample")
  )



SessionHz <- D%>%
  mutate(minute= format(Timestamp, format = "%M:%S"))%>%
  group_by(minute)%>%
  summarise(
    Hz_Rate = sum(Event=="Sample")
  )


#  filter(Event =="Sample", SessionID=="057864beeebb3ecb4d1cb631914c376a")%>%

SessionHzUnique <- unique(SessionHz)

#########################
# NOT WORKING AT ALL
########################

 

############
# B) how many fixations on left/right side of whack-a-mole wall?
#############


################
# This is to get the data of a particular session or a participant

GazeDirection = D %>%
  select(Participant,contains("WorldGazeHitPosition"),SessionID)%>%
  filter(WorldGazeHitPositionX!="NA")%>%
  filter(SessionID=="55aa012d4ac2315806e052fa911c9343")%>%
  summarise(GazeX= as.numeric(WorldGazeHitPositionX),
            GazeY= as.numeric(WorldGazeHitPositionY),
            GazeZ= as.numeric(WorldGazeHitPositionZ))
  

GazeDirection = D %>%
  select(Participant,contains("WorldGazeHitPosition"),SessionID)%>%
  filter(WorldGazeHitPositionX!="NA")%>%
  filter(Participant =='1')

########################################



###############
# Draw a point with the coordonate of the gaze, this work for 1 session not the whole data of a participant.
#############

data <- GazeDirection
  
  plot3d( 
    x=data$`GazeX`, y=data$`GazeY`, z=data$`GazeZ`, 
    type = 's', 
    radius = .01,
    xlab="GazeX", ylab="GazeY", zlab="GazeZ")


  ###############
  # Draw a point with the coordonate of the gaze, not like above its one participant but the data throught all his sessions
  ############# 
  
  GazeDirectionWhole = D %>%
    select(Participant,contains("WorldGazeHitPosition"),SessionID)%>%
    filter(WorldGazeHitPositionX!="NA", Participant=="1")%>%
    group_by(Participant)%>%
    summarise(GazeX= WorldGazeHitPositionX,
              GazeY= WorldGazeHitPositionY,
              GazeZ= WorldGazeHitPositionZ)
    

  data <- GazeDirectionWhole  
  
  plot3d( 
    x=data$`GazeX`, y=data$`GazeY`, z=data$`GazeZ`, 
    type = 's', 
    radius = .01,
    xlab="GazeX", ylab="GazeY", zlab="GazeZ")
  


####################
# Here this is for the heatmap data for the eye tracking, during one session
###################


data <- GazeDirection


mesh_size <- .02
margin <- 0
X <- data %>% select(GazeX, GazeY)
y <- data %>% select(GazeZ)

model <- svm_rbf(cost = 1.0) %>% 
  set_engine("kernlab") %>% 
  set_mode("regression") %>% 
  fit(GazeZ ~ GazeX + GazeY, data = data)

x_min <- min(X$GazeX) - margin
x_max <- max(X$GazeX) - margin
y_min <- min(X$GazeY) - margin
y_max <- max(X$GazeY) - margin
xrange <- seq(x_min, x_max, mesh_size)
yrange <- seq(y_min, y_max, mesh_size)
xy <- meshgrid(x = xrange, y = yrange)
xx <- xy$X
yy <- xy$Y
dim_val <- dim(xx)
xx1 <- matrix(xx, length(xx), 1)
yy1 <- matrix(yy, length(yy), 1)
final <- cbind(xx1, yy1)
pred <- model %>%
  predict(final)

pred <- pred$.pred
pred <- matrix(pred, dim_val[1], dim_val[2])

fig <- plot_ly(data, x = ~GazeX, y = ~GazeY, z = ~GazeZ ) %>% 
  add_markers(size = 5) %>% 
  add_surface(x=xrange, y=yrange, z=pred, alpha = 0.65, type = 'mesh3d', name = 'pred_surface')
fig

################
# try using the saccade package to find some fixation
#
#This work for only one session of one participant
###############


GameStartTimestamp = D %>%
  select(SessionID, Timestamp,Event) %>%
  filter(Event =="Game Started",SessionID=="55aa012d4ac2315806e052fa911c9343" ) 



fixationSession = D %>%
  select(Participant,contains("WorldGazeHitPosition"), SessionID, Event, Timestamp) %>%
  filter(SessionID=="55aa012d4ac2315806e052fa911c9343")


fixationSessionRange <- fixationSession[fixationSession$Timestamp > '2021-06-28 13:50:30.06' & fixationSession$Timestamp < '2021-06-28 13:52:31.9802', ]

GazeDataSession = fixationSessionRange %>%
  summarise(x= as.numeric(WorldGazeHitPositionX),
            y= as.numeric(WorldGazeHitPositionY),
            trial= SessionID,
            time = difftime( Timestamp, GameStartTimestamp$Timestamp)
            ) %>%
  filter(x!="NA",y!="NA")
 
  

fixations <- subset(detect.fixations(GazeDataSession), event=="fixation") 
  
head(fixations)

##################
# Display the fixations
#################
ggplot(fixations, aes(x, y)) +
  geom_point(size=0.7) +
  coord_fixed() +
  facet_wrap(~trial)


###########
# Fixation duration over time 
# This is the duration of a fixation over the time during one session, its not really accurate.
##########

fixations %>%
  plot_ly(x = ~start, y = ~dur, type = 'scatter', mode = 'lines') 
  

############
#This works for one participant at the time, its the same thing than above but
# the data is from all the session of one participant
############
GazeDataParticipant= D%>%
  select(Participant,contains("WorldGazeHitPosition"),Timestamp)%>%
  filter(WorldGazeHitPositionX!="NA")%>%
  filter(Participant=="14")%>%
  summarise(time = Timestamp,
            x= as.numeric(WorldGazeHitPositionX),
            y= as.numeric(WorldGazeHitPositionY),
            trial= Participant)

fixationsParticipant <- subset(detect.fixations(GazeDataParticipant), event=="fixation")
head(fixationsParticipant)


ggplot(fixationsParticipant, aes(x, y)) +
  geom_point(size=0.7) +
  coord_fixed() +
  facet_wrap(~trial)


############
#This summarise everything, the data of the participant number 3 is not accurate at all so i ignore the data
############
GazeData= D%>%
  select(Participant,contains("WorldGazeHitPosition"),Timestamp)%>%
  filter(WorldGazeHitPositionX!="NA", Participant!="3")%>%
  summarise(time = Timestamp,
            x= as.numeric(WorldGazeHitPositionX),
            y= as.numeric(WorldGazeHitPositionY),
            trial= Participant)
 


fixations <- subset(detect.fixations(GazeData), event=="fixation")
head(fixations)



ggplot(fixations, aes(x, y)) +
  geom_point(size=0.7) +
  coord_fixed() +
  facet_wrap(~trial)


############
# C) how long fixation durations on the left/right side of the whack-a-mole wall?
#############





############
# D) Investigate how to calculate “saccade peak velocity”.
#############
#####
# For the saccade peak velocity when can re use the datatable fixations from before.There is a peak vx and vy
# Here i choose to get a brand new dataframe 
GameStartTimestamp = D %>%
  select(SessionID, Timestamp,Event) %>%
  filter(Event =="Game Started",SessionID=="55aa012d4ac2315806e052fa911c9343" ) 



fixationSession = D %>%
  select(Participant,contains("WorldGazeHitPosition"), SessionID, Event, Timestamp) %>%
  filter(SessionID=="55aa012d4ac2315806e052fa911c9343")


fixationSessionRange <- fixationSession[fixationSession$Timestamp > '2021-06-28 13:50:30.06' & fixationSession$Timestamp < '2021-06-28 13:52:31.9802', ]

GazeStat = fixationSessionRange %>%
  summarise(x= as.numeric(WorldGazeHitPositionX),
            y= as.numeric(WorldGazeHitPositionY),
            trial= SessionID,
            time = difftime( Timestamp, GameStartTimestamp$Timestamp)
  ) %>%
  filter(x!="NA",y!="NA")



DataFixations <- subset(detect.fixations(GazeStat), event=="fixation") 


ggplot(DataFixations, aes(x, y)) +
  geom_point(size=0.7) +
  coord_fixed() +
  facet_wrap(~trial)


#########
# Some Stats with peak velocity on X and on Y
########
statsVelocity <- calculate.summary(DataFixations)
round(statsVelocity, digits=2)


Velocity= DataFixations %>%
  summarise(peakVx = DataFixations$peak.vx,
         peakVy = DataFixations$peak.vy)



peakVelocity = Velocity %>%
  rowwise() %>%
  mutate(peakVelocity = sqrt(sum(peakVx^2 + peakVy^2)))



###########
# Bring all the session together to check the fixations 
###########


Participant1= D%>%
  select(Participant,contains("WorldGazeHitPosition"),Timestamp)%>%
  filter(WorldGazeHitPositionX!="NA")%>%
  filter(Participant=="6")%>%
  summarise(time = Timestamp,
            x= as.numeric(WorldGazeHitPositionX),
            y= as.numeric(WorldGazeHitPositionY),
            trial= Participant)

fixationsParticipant1 <- subset(detect.fixations(Participant1), event=="fixation")

fixationsParticipant1 %>%
  plot_ly(x = ~start, y = ~dur, type = 'scatter', mode = 'lines') 


############
# Plot to see eye movement based on the duration
############

library(plotly)
install.packages("quantmod")
library(quantmod)

getSymbols("AAPL",src='yahoo')



GameStartTimestamp = D %>%
  select(SessionID, Timestamp,Event) %>%
  filter(Event =="Game Started",SessionID=="55aa012d4ac2315806e052fa911c9343" ) 



fixationSession = D %>%
  select(Participant,contains("WorldGazeHitPosition"), SessionID, Event, Timestamp) %>%
  filter(SessionID=="55aa012d4ac2315806e052fa911c9343")


fixationSessionRange <- fixationSession[fixationSession$Timestamp > '2021-06-28 13:50:30.06' & fixationSession$Timestamp < '2021-06-28 13:52:31.9802', ]

GazeStat = D %>%
  select(Participant,contains("WorldGazeHitPosition"), SessionID, Event, Timestamp) %>%
  filter(SessionID=="55aa012d4ac2315806e052fa911c9343")%>%
  summarise(x= as.numeric(WorldGazeHitPositionX),
            y= as.numeric(WorldGazeHitPositionY),
            trial= SessionID,
            time = Timestamp
  ) %>%
  filter(x!="NA",y!="NA")



DataFixations <- subset(detect.fixations(GazeStat), event=="fixation") 



fig <- plot_ly() %>% add_trace(data = DataFixations, x=~x, y=~y, frame=~start)


fig


col_count = df_vis %>% filter(!is.na(WallColumnCount)) %>% select(WallColumnCount)
row_count = df_vis %>% filter(!is.na(WallRowCount)) %>% select(WallRowCount)
Wall_moles <- expand.grid(1:tail(col_count, n=1)[,1], 1:tail(row_count, n=1)[,1]) %>%
  dplyr::rename(x = Var1, y = Var2)

fig <- vistemplate %>%
  add_trace(name="Spawn Points", data=Wall_moles,
            x=~x, y=~y, type='scatter',mode='markers',symbol=I('o'),marker=list(size=32),hoverinfo='none')

  
plot_ly() %>% add_trace(data = GazeStat, x=~x, y=~y, frame=~time)


