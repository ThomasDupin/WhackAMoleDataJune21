library(plotly)
library(tidyverse)
install_github("tmalsburg/saccades/saccades", dependencies=TRUE, force=TRUE)
library(saccades)
library(rgl)
library(reshape2)
library(tibble)
install_github("tidymodels/tidymodels")
library(tidymodels)
library(plotly)
library(kernlab)
library(pracma) #For meshgrid()²
library(lubridate)
install.packages("quantmod")
library(quantmod)
install.packages("devtools")
library(devtools) # Make sure that the devtools library is loaded
install_github("firasfneish/CI-package")
library(CI)
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
# I'm gettin the time at the game started event and the time at the game finished event
# After i just have to do the difference between those two time to get the session duration
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
# Here i have the data across all sessions
  GazePosition = D %>%
    select(Participant,contains("WorldGazeHitPosition"))  %>%
    filter(WorldGazeHitPositionX!="NA") %>%
    group_by(Participant)%>%
    summarize(MeanGazePositionX= mean(as.numeric(WorldGazeHitPositionX), na.rm=TRUE),
              MeanGazePositionY= mean(as.numeric(WorldGazeHitPositionY), na.rm=TRUE),
              MeanGazePositionZ= mean(as.numeric(WorldGazeHitPositionZ), na.rm=TRUE))


# Here is the data for each session of participant, i don't know why have e-01 or more for the X colomn
GazePositionSession = D %>%
  select(Participant,contains("WorldGazeHitPosition"),SessionID)  %>%
  filter(WorldGazeHitPositionX!="NA") %>%
  group_by(Participant, SessionID)%>%
  summarize(MeanGazePositionX= mean(as.numeric(WorldGazeHitPositionX), na.rm=TRUE),
            MeanGazePositionY= mean(as.numeric(WorldGazeHitPositionY), na.rm=TRUE),
            MeanGazePositionZ= mean(as.numeric(WorldGazeHitPositionZ), na.rm=TRUE))
   
#############
# 4) What was the lowest, highest and mean gaze confidence for each participant?
#############
# same process than above but this time we are checking for the min max and the mean of the gaze confidence
  GazeConfidence = D %>%
    select(Participant, GazeConfidence)%>%
    filter(GazeConfidence!="NA")%>%
    group_by(Participant)%>%
    summarize(LowestGazeConfidence= min(as.numeric(GazeConfidence), na.rm=TRUE),
              HighestGazeConfidence= max(as.numeric(GazeConfidence), na.rm=TRUE),
              MeanGazeConfidence= mean(as.numeric(GazeConfidence), na.rm=TRUE))

# Same process than above this data frame is for each session of the participant
GazeConfidenceSession = D %>%
  select(Participant, GazeConfidence,SessionID)%>%
  filter(GazeConfidence!="NA")%>%
  group_by(Participant,SessionID)%>%
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
# calculate the HZ
###########
# Here i retreive the sessions, and i count the number of event " sample " inside each minute 
# after that i just do a mean 
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
# Here i got more precise data than above, it allow me to check for one session, and to get the sample at each minute
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


# Graph with the sample over the time for one session
DisplayHz %>%
  plot_ly(x = ~minute, y = ~Hz_Rate, type = "scatter", mode="line")
  


############
# B) how many fixations on left/right side of whack-a-mole wall?
#############


################
# This is to get the data of a particular session or a participant

#### This one work for only one session at the time
GazeDirection = D %>%
  select(Participant,contains("WorldGazeHitPosition"),SessionID)%>%
  filter(WorldGazeHitPositionX!="NA")%>%
  filter(SessionID=="55aa012d4ac2315806e052fa911c9343")%>%
  summarise(GazeX= as.numeric(WorldGazeHitPositionX),
            GazeY= as.numeric(WorldGazeHitPositionY),
            GazeZ= as.numeric(WorldGazeHitPositionZ))

# Below it's the code for the plot
data <- GazeDirection

  plot3d( 
  x=data$`GazeX`, y=data$`GazeY`, z=data$`GazeZ`, 
  type = 's', 
  radius = .01,
  xlab="GazeX", ylab="GazeY", zlab="GazeZ")

##################################################################################
# Draw a point with the coordonate of the gaze, not like above its one participant but the data throught all his sessions
############# 
  
  GazeDirectionWhole = D %>%
    select(Participant,contains("WorldGazeHitPosition"),SessionID)%>%
    filter(WorldGazeHitPositionX!="NA", Participant=="1")%>%
    group_by(Participant)%>%
    summarise(GazeX= WorldGazeHitPositionX,
              GazeY= WorldGazeHitPositionY,
              GazeZ= WorldGazeHitPositionZ)
    
###### The plot is quit slow because there is too much data
  data <- GazeDirectionWhole  
  
  plot3d( 
    x=data$`GazeX`, y=data$`GazeY`, z=data$`GazeZ`, 
    type = 's', 
    radius = .01,
    xlab="GazeX", ylab="GazeY", zlab="GazeZ")
  


####################
# Here this is for the heatmap data for the eye tracking, during one session
###################
GazeDirectionHeatMap = D %>%
    select(Participant,contains("WorldGazeHitPosition"),SessionID)%>%
    filter(WorldGazeHitPositionX!="NA")%>%
    filter(SessionID=="55aa012d4ac2315806e052fa911c9343")%>%
    summarise(GazeX= as.numeric(WorldGazeHitPositionX),
              GazeY= as.numeric(WorldGazeHitPositionY),
              GazeZ= as.numeric(WorldGazeHitPositionZ))

data <- GazeDirectionHeatMap


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

# Same process as usual we use the code below to get perfectly one session of data

GameStartTimestamp = D %>%
  select(SessionID, Timestamp,Event) %>%
  filter(Event =="Game Started",SessionID=="55aa012d4ac2315806e052fa911c9343" ) 



fixationSession = D %>%
  select(Participant,contains("WorldGazeHitPosition"), SessionID, Event, Timestamp) %>%
  filter(SessionID=="55aa012d4ac2315806e052fa911c9343")


fixationSessionRange <- fixationSession[fixationSession$Timestamp > '2021-06-28 13:50:30.06' & fixationSession$Timestamp < '2021-06-28 13:52:31.9802', ]

#Below there is the data frame of one session from the event game start to game finished

GazeDataSession = fixationSessionRange %>%
  summarise(x= as.numeric(WorldGazeHitPositionX),
            y= as.numeric(WorldGazeHitPositionY),
            trial= SessionID,
            time = difftime( Timestamp, GameStartTimestamp$Timestamp)
            ) %>%
  filter(x!="NA",y!="NA")
 
  
# with the sacade package we can get some saccades information, we have to use the data frame as i used above with the same columns


fixations <- subset(detect.fixations(GazeDataSession)) 
  
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

fixationsParticipant <- subset(detect.fixations(GazeDataParticipant))
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
 


fixations <- subset(detect.fixations(GazeData))
head(fixations)



ggplot(fixations, aes(x, y)) +
  geom_point(size=0.7) +
  coord_fixed() +
  facet_wrap(~trial)



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



DataFixations <- subset(detect.fixations(GazeStat)) 


ggplot(DataFixations, aes(x, y)) +
  geom_point(size=0.7) +
  coord_fixed() +
  facet_wrap(~trial)


#########
# Some Stats with peak velocity on X and on Y i used some function of the library saccades
########
statsVelocity <- calculate.summary(DataFixations)
round(statsVelocity, digits=2)

# this data frame contain the peak vx and vy
Velocity= DataFixations %>%
  summarise(peakVx = DataFixations$peak.vx,
         peakVy = DataFixations$peak.vy)


# with this one we add the peak velocity who is the combination of vx and vy
peakVelocity = Velocity %>%
  rowwise() %>%
  mutate(peakVelocity = sqrt(sum(peakVx^2 + peakVy^2)))



############
# Plot to see eye movement based on the duration
############

##########
# Plot the eye movement based on the fixation
#########

GazeStatFixation = fixationSessionRange %>%
  summarise(x= as.numeric(WorldGazeHitPositionX),
            y= as.numeric(WorldGazeHitPositionY),
            trial= SessionID,
            time = difftime( Timestamp, GameStartTimestamp$Timestamp)
  ) %>%
  filter(x!="NA",y!="NA")

DataFixations <- subset(detect.fixations(GazeStatFixation)) 
DataFixationsUpdated <- filter(DataFixations, dur>0.34353489 & dur<0.46293334)

## Above we decided to remove the fixation who where not long enough, they can come from noise on the data.
# so we remove the really short fixation
# To do that we use the function bellow to plot the density of the fixation so we can see where there is the most

Lines <- DataFixations%>% ungroup() %>% group_by(dur) %>%
  
  summarize(perc_ci = qt(0.95 + (1 - 0.95)/2, df = length(dur) - 1) * sd(dur)/sqrt(length(dur)),
            
            frust_ci = qt(0.95 + (1 - 0.95)/2, df = length(dur) - 1) * sd(dur)/sqrt(length(dur)),
            
            perc_mean = mean(dur),
            
            frust_mean = mean(dur))


hey <- CI_t(DataFixations$dur, ci = 0.95)


plot(density(DataFixations$dur))

ShapiroTest <- shapiro.test(DataFixations$dur)
# w = 0.73673
# p-value < 2.2e-16
#0.000000000000000000000002 so not normally distributed
#


########
# Here is the plot for the fixation
########


fig <- plot_ly() %>% add_trace(data = DataFixationsUpdated, x=~x, y=~y, frame=~start)  %>%
  add_trace(name="Spawn Points", data=MoleWallXY,
            x=~X, y=~Y, type='scatter',mode='markers',symbol=I('o'),marker=list(size=32),hoverinfo='none')

fig



########
# GAZE PATH PLOT
#######

# function to delete NA
delete.na <- function(DF, n=0) {
  DF[rowSums(is.na(DF)) <= n,]
}


#########
# This is for the coord of the background mole
#########
MoleWall <- D %>% 
  select(WallRowCount,WallColumnCount) %>%
  summarize(
    x = WallColumnCount,
    y = WallRowCount
  )
  
MoleWallXY <- D %>% 
  select(MolePositionWorldX,MolePositionWorldY) %>%
  mutate(XY= paste(MolePositionWorldX,MolePositionWorldY))%>%
  summarize(
    X = as.numeric(MolePositionWorldX),
    Y = as.numeric(MolePositionWorldY),
    XY = XY
  )


MoleWallXY <- unique(MoleWallXY)
MoleWallXY <- delete.na(MoleWallXY)

####
# With that i have inside MoleWallXY the coord of all the mole so i can use them for the backgrounds


#############
# To get one session of data from the even game started
#############
# Here i get the starting game timestamp for the session

GameStartTimestamp = D %>%
  select(SessionID, Timestamp,Event) %>%
  filter(Event =="Game Started",SessionID=="55aa012d4ac2315806e052fa911c9343" ) 



fixationSession = D %>%
  select(Participant,contains("WorldGazeHitPosition"), SessionID, Event, Timestamp,contains("CurrentMoleToHitIndex")) %>%
  filter(SessionID=="55aa012d4ac2315806e052fa911c9343")


fixationSessionRange <- fixationSession[fixationSession$Timestamp > '2021-06-28 13:50:30.0699' , ]

#Inside FixationSessionRange i have a complete session of data from the event gameStarted to gameFinished



#######
# Datafram with all the mole 
######
# Inside MoleData i have all the mole defined inside so i can after use them to add all the mole at each time 
# with that plotly understand that each mole are individual
MoleData <- D%>%
  select(SessionID, MolePositionWorldX,MolePositionWorldY )%>%
  filter(SessionID =="55aa012d4ac2315806e052fa911c9343")%>%
  summarise(XMole = as.numeric(MolePositionWorldX),
            YMole =as.numeric(MolePositionWorldY),
            Size = 0,
            id = paste(XMole, YMole, sep = ""))


MoleData <- unique(MoleData)
MoleData <- delete.na(MoleData)


#### Above i'm filtering the data so everything is clean

#### Below i'm recovering the data for the gaze stat, i'm using separate to get the time clean. 
# With this method i have 1.1 1.2 1.3 .... of data and at each sample i have the X and Y


GazeStat = fixationSessionRange %>%
  mutate(time = as.character(difftime( Timestamp, GameStartTimestamp$Timestamp))) %>%
  separate(col=time,sep="[.]",into=c("i1","i2"), remove=F) %>%
  separate(col=i2,sep="",into=c("i3","i4","i5","i6"), remove=F) %>%
  summarise(x= as.numeric(WorldGazeHitPositionX),
            y= as.numeric(WorldGazeHitPositionY),
            time = as.numeric(paste(i1,".",i4,sep = ""))
  )

GazeStatWithoutNa <- delete.na(GazeStat)

GazeStatReduced <- GazeStatWithoutNa[!duplicated(GazeStatWithoutNa[,c('time')]),]

GazeStatReduced <- GazeStatReduced[which(1:nrow(GazeStatReduced) %% 9 == 0) , ]

#### Above i'm cleaning my datafram, removing the NA, removing the duplicate and reducing the data.
### I'm reducing the data using the multiple method, i onyl keep the line who their row number where divided by 9 = 0

#######
# Below i'm gettin the data for the mole activated, i'm using the same method for the time with the two separate.
# The data will contain the X and Y index and the X and Y coord of a mole

MoleActivated <- D%>%
  select(contains("CurrentMoleToHitIndex"), Timestamp, SessionID, MolePositionWorldX,MolePositionWorldY, Event )%>%
  filter(SessionID =="55aa012d4ac2315806e052fa911c9343", Event=="Mole Spawned")%>%
  mutate(time = as.character(difftime( Timestamp, GameStartTimestamp$Timestamp))) %>%
  separate(col=time,sep="[.]",into=c("i1","i2"), remove=F) %>%
  separate(col=i2,sep="",into=c("i3","i4","i5","i6"), remove=F) %>%
  summarise(xIndex= as.numeric(CurrentMoleToHitIndexX),
            yIndex= as.numeric(CurrentMoleToHitIndexY),
            XMole=as.numeric( MolePositionWorldX),
            YMole=as.numeric(MolePositionWorldY),
            time = as.numeric(paste(i1,".",i4,sep = ""))
  )%>%
  filter(time >=0)


### cleaning and arrange the data removing the duplicated line
MoleActivated <- unique(MoleActivated)

MoleActivated <- arrange(MoleActivated,time) 

# Deleting the line who have more than 3 NA because sometime it happen
MoleActivated <- delete.na(MoleActivated, 3)

### Replacing the remaining NA with 0 
MoleActivated[is.na(MoleActivated)] <- 0

# Here i'm adding the size if the coord are equal to 0 the size will be 0 because there is no mole, if the coord are other than 0
# it mean that we have a mole so i pass the size to 32
MoleActivated$size <- ifelse(MoleActivated$xIndex == 0 , 0,32)
 


#############
# Here i'm binding the two data fram together for plotly and i arrange the data fram by time
GazeStatActiveMole <- bind_rows(GazeStatReduced, MoleActivated) 

GazeStatActiveMole <- arrange(GazeStatActiveMole,time)

####
# i summarise the dataframe in order to get exactly what i want
  
GazeStatActiveMole <- GazeStatActiveMole %>%
  filter(time >= 0.1) %>%
  summarise(x = x,
            y = y, 
            time = time,
            XMole= XMole,
            YMole = YMole,
            size = size)


### transforming all the NA in 0 so i can process them later
GazeStatActiveMole$size[is.na(GazeStatActiveMole$size)] <- 0

###############
# Here i'm using the data fram MoleData, i'm grouping all the data by time and at each group i add the definiton of all the mole
# So plolty understand well that they are individuals

GazeStatActiveMole <- GazeStatActiveMole %>%
  group_by(time)%>%
  group_modify(~add_row(xIndex = MoleData$xIndex, yIndex = MoleData$yIndex, XMole = MoleData$XMole, YMole =MoleData$YMole, .x))%>%
  ungroup()


# transforming the NA inside the col size to 0
GazeStatActiveMole$size[is.na(GazeStatActiveMole$size)] <- 0

# Below i'm filling the data because i have some NA inside the gaze stat cols ( X and Y ) so i'm filling everything because
# because it's not working with NA

GazeStatActiveMole <- GazeStatActiveMole %>% fill(x, .direction = "downup") %>% fill(y, .direction = "downup")

GazeStatActiveMole$XMole[is.na(GazeStatActiveMole$XMole)] <- 0
GazeStatActiveMole$YMole[is.na(GazeStatActiveMole$YMole)] <- 0

# Here i create the ID for each mole so they all have something different to make plotly understand that they are individuals
GazeStatActiveMole <- GazeStatActiveMole %>%
  mutate(id = paste(XMole,YMole,sep = ""))
# Replacing the NA
GazeStatActiveMole$id[is.na(GazeStatActiveMole$id)] <- 0.00

## I'm ordering the data to make the plot works, i think ordering like that is the best way
GazeStatActiveMole <- arrange(GazeStatActiveMole,time, YMole, XMole,size) 

# for now i have to slice the data because there is to much to deal with
GazeStatActiveMole <- GazeStatActiveMole %>%
  slice(1177:1808)

#GazeStatActiveMole <- unique(GazeStatActiveMole)
fig <- plot_ly() %>% add_trace(name="Patient Gaze",data = GazeStatActiveMole, x=~x, y=~y, frame=~time)%>%
  add_trace(name="Spawn Points", data=MoleWallXY,
            x=~X, y=~Y, type='scatter',mode='markers',symbol=I('o'),marker=list(size=32),hoverinfo='none')%>%
  add_trace(name="Active Mole", data=GazeStatActiveMole,
            x=~XMole, y=~YMole, type='scatter',mode='markers',frame=~time, marker=list(size=~size))
 
fig










###### Mole and event test


WOW <- D%>%
  select(contains("CurrentMoleToHitIndex"), Timestamp, SessionID, MolePositionWorldX,MolePositionWorldY,Event )%>%
  filter(SessionID =="55aa012d4ac2315806e052fa911c9343", Event =="Mole Spawned")%>%
  mutate(time = as.character(difftime( Timestamp, GameStartTimestamp$Timestamp))) %>%
  separate(col=time,sep="[.]",into=c("i1","i2"), remove=F) %>%
  separate(col=i2,sep="",into=c("i3","i4","i5","i6"), remove=F) %>%
  summarise(xIndex= as.numeric(CurrentMoleToHitIndexX),
            yIndex= as.numeric(CurrentMoleToHitIndexY),
            XMole=as.numeric( MolePositionWorldX),
            YMole=as.numeric(MolePositionWorldY),
            Event = Event
  )


### cleaning and arrange the data removing the duplicated line
WOW <- unique(WOW)

 

# Deleting the line who have more than 3 NA because sometime it happen
WOW <- delete.na(WOW, 3)

### Replacing the remaining NA with 0 
WOW[is.na(WOW)] <- 0








###### 
# TEST FOR THE MOLE 
# Here i work with a small data frame and a small amount of mole it's the same process than before

MOLETEST <- data.frame(xIndex=as.numeric() , yIndex=as.numeric(), XMole=as.numeric(), YMole=as.numeric(),opacity=as.numeric(), time=as.numeric(), id=as.numeric()) 
MOLETEST <- MOLETEST %>%  
  add_row(xIndex = 6, yIndex = 5, XMole = 0, YMole =1.8500,opacity= 0, time =0, id= 1.85)




TESTGazeStat = fixationSessionRange %>%
  mutate(time = as.character(difftime( Timestamp, GameStartTimestamp$Timestamp))) %>%
  separate(col=time,sep="[.]",into=c("i1","i2"), remove=F) %>%
  separate(col=i2,sep="",into=c("i3","i4","i5","i6"), remove=F) %>%
  summarise(x= as.numeric(WorldGazeHitPositionX),
            y= as.numeric(WorldGazeHitPositionY),
            time = as.numeric(paste(i1,".",i4,sep = ""))
  )

TESTGazeStatWithoutNa <- delete.na(TESTGazeStat)


TESTGazeStatReduced <- TESTGazeStatWithoutNa[!duplicated(TESTGazeStatWithoutNa[,c('time')]),]

TESTGazeStatReduced <- TESTGazeStatReduced %>%
  slice(16:40)


TESTMOLE <- D%>%
  select(contains("CurrentMoleToHitIndex"), Timestamp, SessionID, MolePositionWorldX,MolePositionWorldY )%>%
  filter(SessionID =="55aa012d4ac2315806e052fa911c9343")%>%
  mutate(time = as.character(difftime( Timestamp, GameStartTimestamp$Timestamp))) %>%
  separate(col=time,sep="[.]",into=c("i1","i2"), remove=F) %>%
  separate(col=i2,sep="",into=c("i3","i4","i5","i6"), remove=F) %>%
  summarise(xIndex= as.numeric(CurrentMoleToHitIndexX),
            yIndex= as.numeric(CurrentMoleToHitIndexY),
            XMole=as.numeric( MolePositionWorldX),
            YMole=as.numeric(MolePositionWorldY),
            time = as.numeric(paste(i1,".",i4,sep = ""))
  )%>%
  filter(time >=0)


TESTMOLE <- unique(TESTMOLE)

TESTMOLE <- arrange(TESTMOLE,time) 

TESTMOLE[is.na(TESTMOLE)] <- 0


TESTMOLE$opacity <- ifelse(TESTMOLE$xIndex == 0, 0,32)


TESTMOLE <- TESTMOLE %>%
  mutate(id = row_number()) 

TESTMOLE <- TESTMOLE %>%
  slice(16:40)


TESTGazeStatActiveMole <- bind_rows(TESTGazeStatReduced, TESTMOLE) 

TESTGazeStatActiveMole <- arrange(TESTGazeStatActiveMole,time) 


TESTGazeStatActiveMole$opacity[is.na(TESTGazeStatActiveMole$opacity)] <- 0


TESTGazeStatActiveMole <- TESTGazeStatActiveMole %>%
  mutate(id = paste(XMole,YMole,sep = ""))%>%
  mutate(id = as.numeric(id),)
  

TESTDF <- TESTGazeStatActiveMole %>%
  group_by(time)%>%
  group_modify(~add_row(xIndex = MOLETEST$xIndex, yIndex = MOLETEST$yIndex, XMole = MOLETEST$XMole, YMole =MOLETEST$YMole, .x))
  
  
TESTDF$opacity[is.na(TESTDF$opacity)] <- 0


TESTDF <- TESTDF %>% fill(x) %>% fill(y)

TESTDF$xIndex[is.na(TESTDF$xIndex)] <- 0
TESTDF$yIndex[is.na(TESTDF$yIndex)] <- 0
TESTDF$XMole[is.na(TESTDF$XMole)] <- 0
TESTDF$YMole[is.na(TESTDF$YMole)] <- 0
TESTDF$id[is.na(TESTDF$id)] <- 0.00

TESTDF <- arrange(TESTDF,time,opacity) 

TESTDF[7,2] <- -1.6922

TESTDF[7,3] <- 3.4478

TESTDF[8,2] <- 1.5479

TESTDF[8,3] <- 3.2121

TESTDF <- unique(TESTDF)

fig <- plot_ly() %>% add_trace(name="Patient Gaze",data = TESTDF, x=~x, y=~y, frame=~time)%>%
  add_trace(name="Spawn Points", data=MoleWallXY,
            x=~X, y=~Y, type='scatter',mode='markers',symbol=I('o'),marker=list(size=32),hoverinfo='none')%>%
  add_trace(name="Active Mole", data=TESTDF,
            x=~XMole, y=~YMole, type='scatter',mode='markers',frame=~time, marker=list(size=~opacity))


fig



###############
# Below it's a test that i'm using to work only with the mole



MOLETEST <- data.frame(xIndex=as.numeric() , yIndex=as.numeric(), XMole=as.numeric(), YMole=as.numeric(),opacity=as.numeric(), time=as.numeric(), id=as.numeric()) 
MOLETEST <- MoleData %>%  
  slice(1:3)



TESTMOLE <- D%>%
  select(contains("CurrentMoleToHitIndex"), Timestamp, SessionID, MolePositionWorldX,MolePositionWorldY )%>%
  filter(SessionID =="55aa012d4ac2315806e052fa911c9343")%>%
  mutate(time = as.character(difftime( Timestamp, GameStartTimestamp$Timestamp))) %>%
  separate(col=time,sep="[.]",into=c("i1","i2"), remove=F) %>%
  separate(col=i2,sep="",into=c("i3","i4","i5","i6"), remove=F) %>%
  summarise(xIndex= as.numeric(CurrentMoleToHitIndexX),
            yIndex= as.numeric(CurrentMoleToHitIndexY),
            XMole=as.numeric( MolePositionWorldX),
            YMole=as.numeric(MolePositionWorldY),
            time = as.numeric(paste(i1,".",i4,sep = ""))
  )%>%
  filter(time >=0)

TESTMOLE <- unique(TESTMOLE)

TESTMOLE <- arrange(TESTMOLE,time) 

TESTMOLE$size <- ifelse(TESTMOLE$xIndex == 0, 0,32)

TESTMOLE <- TESTMOLE %>%
  slice(0:30)


TESTMOLE <- TESTMOLE %>%
  group_by(time)%>%
  group_modify(~add_row(xIndex = MOLETEST$xIndex, yIndex = MOLETEST$yIndex, XMole = MOLETEST$XMole, YMole =MOLETEST$YMole, .x))

TESTMOLE <- delete.na(TESTMOLE, 3)

TESTMOLE$size[is.na(TESTMOLE$size)] <- 0
TESTMOLE$xIndex[is.na(TESTMOLE$xIndex)] <- 0
TESTMOLE$yIndex[is.na(TESTMOLE$yIndex)] <- 0




TESTMOLE <- TESTMOLE %>%
  mutate(id = paste(XMole,YMole,sep = ""))

TESTMOLE[51,6] <- 0


TESTMOLE[65,6] <- 32


TESTMOLE[57,6] <- 32

TESTMOLE <- unique(TESTMOLE)
TESTMOLE <- arrange(TESTMOLE,time, size) 


fig <- plot_ly() %>% 
  add_trace(name="Spawn Points", data=MoleWallXY,
            x=~X, y=~Y, type='scatter',mode='markers',symbol=I('o'),marker=list(size=32),hoverinfo='none')%>%
  add_trace(name="Active Mole", data=TESTMOLE,
            x=~XMole, y=~YMole, type='scatter',mode='markers',frame=~time, marker=list(size=~size))

fig



