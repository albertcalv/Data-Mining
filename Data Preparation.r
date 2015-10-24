## SCRIPT: DATA PREPARATION AND PREPROCESSING ##
setwd("/home/evelyn/MD")
dd<-read.csv("NBA-Pbp-Sample-Game-Data.csv",header=T, sep=",")
attach(dd)

## Exclusion criteri columns##
dd <- dd[, c(4:39,42,43)] 
summary(dd$event_type) 
dd <- subset(dd, event_type != "unknown") 

## Data Preparation 
dd$converted_x <- as.numeric(substr(dd$converted_x, 1, 2))
dd$converted_y <- as.numeric(substr(dd$converted_y, 1, 2))

## DATA PREPROCESSING ##
#Split data in qualitative and numerical
qd<-data.frame(dd$a1, dd$a2, dd$a3, dd$a4, dd$a5, dd$h1, dd$h2, dd$h3, dd$h4, dd$h5, dd$period, dd$remaining_time, 
               dd$elapsed, dd$play_length, dd$play_id, dd$team, dd$event_type, dd$assist, dd$away, dd$home, 
               dd$block, dd$entered, dd$left, dd$opponent, dd$player, dd$possession, dd$reason, dd$result, dd$steal, dd$type)
nd<-data.frame(dd$away_score, dd$home_score, dd$num, dd$outof, dd$points, dd$shot_distance,
               dd$converted_x, dd$converted_y)

#Prepare QUALITATIVE data 
qd$dd.period<-factor(qd$dd.period)
qd$dd.play_id<-factor(qd$dd.play_id)


#Sobstitute "" (undefifined values) with "NR" (Not Relevant)
for(i in 1:length(qd)){
  if(levels(qd[,i])[1] == "" ){
    levels(qd[,i])[1] = "NR"
  }
}

#Prepare NUMERICAL data
nd$dd.num[is.na(nd$dd.num)]                     <- 0
nd$dd.outof[is.na(nd$dd.outof)]                 <- 0
nd$dd.points[is.na(nd$dd.points)]               <- 0
nd$dd.shot_distance[is.na(nd$dd.shot_distance)] <- 0

#shots <- nd$dd.shot_distance
#shots[is.na(shots)] <- 0

nNA <- length(nd$dd.converted_x[is.na(nd$dd.converted_x) & qd$dd.event_type !="free throw"]) 
#nNAS <- length(nd$dd.shot_distance[is.na(nd$dd.shot_distance) & qd$dd.event_type !="free throw"]) 

randomX <- sample(0:50, nNA, replace = TRUE)
randomY <- sample(0:94, nNA, replace = TRUE)
#randomS <- sample(min(shots):max(shots), nNA, replace = TRUE)

nd$dd.converted_x[is.na(nd$dd.converted_x) & qd$dd.event_type !="free throw"] <- randomX
nd$dd.converted_y[is.na(nd$dd.converted_y) & qd$dd.event_type !="free throw"] <- randomY
#nd$dd.shot_distance[is.na(nd$dd.shot_distance) & qd$dd.event_type !="free throw"] <- randomS

nd$dd.shot_distance[qd$dd.event_type =="free throw"]<-19
#shot_distance feet to m 
#nd$dd.shot_distance <- nd$dd.shot_distance * 0.3048 
nd$dd.converted_x[qd$dd.event_type =="free throw"]<-as.numeric(25)
nd$dd.converted_y[qd$dd.event_type =="free throw" & qd$dd.team == "SAS"]<-as.numeric(20)
nd$dd.converted_y[qd$dd.event_type =="free throw" & qd$dd.team == "MIA"]<-as.numeric(71)

cds <- dd

cds$period <- qd$dd.period
cds$remaining_time <- qd$dd$remaining_time
cds$elapsed <- qd$dd.elapsed
cds$play_length <- qd$dd.play_length
cds$play_id <- qd$dd.play_id
cds$team <- qd$dd.team
cds$event_type <- qd$dd.event_type
cds$assist <- qd$dd.assist
cds$away <- qd$dd.away
cds$home <- qd$dd.home
cds$block <- qd$dd.block
cds$entered <- qd$dd.entered
cds$left <- qd$dd.left
cds$opponent <- qd$dd.opponent
cds$player <- qd$dd.player
cds$possession <- qd$dd.possession
cds$reason <- qd$dd.reason
cds$result <- qd$dd.result
cds$steal <- qd$dd.steal
cds$type <- qd$dd.type

cds$num <- nd$dd.num
cds$outof <- nd$dd.outof
cds$points <- nd$dd.points
cds$shot_distance <- nd$dd.shot_distance
cds$converted_x <- nd$dd.converted_x
cds$converted_y <- nd$dd.converted_y

write.csv(cds, file="NBA.csv")
