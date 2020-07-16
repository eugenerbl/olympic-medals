# Preprocessing of Olympic Data
# Minimizes original dataset to essential variables
# Eugene Lucino

# original dataset; change file path as needed
athletes <- read.csv("Data Mining II/Olympics/athlete_events.csv")

# consider only athletes who won medals and 
# competed at the summer games
athletes <- athletes[!(is.na(athletes$Medal)),]
athletes <- athletes[!(athletes$Season=="Winter"),]
athletes <- athletes[(athletes$Year > 1990),]

# remove unimportant variables
# remove athlete info (ID, Name, Sex, Age, Height, Weight)
# Games, Season, Team will be covered by Year,City, NOC
athletes <- athletes[,c("NOC", "Year", "City",
                        "Sport", "Event", "Medal")]

# remove duplicates to account for teams
athletes <- athletes[!duplicated(athletes), ]

# remove medals from dissolved countries
athletes <- athletes[!(athletes$NOC=='EUN' |
                       athletes$NOC=='SCG' |
                       athletes$NOC=='IOA'),]

# create a Host_NOC column to show which country hosted
# a specific Olympics
athletes$Host_NOC <- apply(athletes, 1, FUN = function(x)
       if(x[3]=="Barcelona") 'ESP'
  else if(x[3]=="Atlanta") 'USA'
  else if(x[3]=="Sydney") 'AUS'
  else if(x[3]=="Athina") 'GRE'
  else if(x[3]=="Beijing") 'CHN'
  else if(x[3]=="London") 'GBR'
  else if(x[3]=="Rio de Janeiro") 'BRA'
  else 'XXX')

# convert to factor
athletes$Host_NOC <- as.factor(athletes$Host_NOC)

# reorder columns
athletes <- athletes[, colnames(athletes)[c(2,7,3,4,5,1,6)]]
names(athletes)[3] <- "Host_City"

# remove row names
rownames(athletes) <- c()

# create the final dataset
# 6272 medal winners, 7 variables
write.csv(athletes, "Data Mining II/medals.csv",
          row.names = FALSE)
