filename :chr = filename 
Event :chr = Event of the game ( gameStart, stop, wallSizeupdate ....)
Timestamp :POSIXct[1:1034494], format = Time of the event
SessionId=chr = Id of the session
Email:chr
EventType:char = type of the event ( MotorSpaceEvent, GameEvent ...)
TimeSinceLastEvent: chr = Time since the last event in : ?
MirrorEffect: chr = is the option mirror effect on or of ? 
EyePatch: chr = is the EyePatch effect on or of ?       
GameState: chr = state of the game ( playing ....)
GameDuration: chr : = game duration 
GameSpeed: chr: = game speed ( normal ...)
GameTimeSpent: chr = gameTimeSpent
GameTimeLeft: chr = Game time left on the timer

/////////////////// Gaze
WorldGazeOriginX: chr  = The origin of the gaze in the 3d Space on  XYZ axis
WorldGazeOriginY: chr 
WorldGazeOriginZ: chr 

WorldGazeDirectionX: chr = Direction of the gaze in the 3d Space on XYZ axis
WorldGazeDirectionY: chr 
WorldGazeDirectionZ: chr 

WorldGazeHitPositionX: chr = the position were do the player gaze hit in the 3d Space 
WorldGazeHitPositionY: chr 
WorldGazeHitPositionZ: chr 


WorldGazeHitObjectName: chr = The object were look the player 
WorldGazeHitObjectIsMole: chr= Is the object a Mole 
WorldGazeHitObjectMoleID: chr = the ID
WorldGazeHitObjectIsWall: chr = Is  the wall ? 
Participant: num = ID of the participant
Condition: chr = The condition of the session ( Only left controller only right small space ...)
