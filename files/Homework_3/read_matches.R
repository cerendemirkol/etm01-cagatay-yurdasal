# Init files----
# 
require(data.table)
require(lubridate)
# read matches info
matches=readRDS('matches.rds')

# Data corrections----
# delete these column
matches$type=NULL
matches$leagueId=NULL

# fix home teams
matches[home=='manchester-city',home:='manchester city']
matches[home=='crystal-palace',home:='crystal palace']
matches[home=='newcastle utd',home:='newcastle']
matches[home=='stoke',home:='stoke city']
matches[home=='west-ham',home:='west ham']
matches[home %in% c('manchester-united','manchester-utd'), home:='manchester united']

# fix away teams
matches[away=='manchester-city',away:='manchester city']
matches[away=='crystal-palace',away:='crystal palace']
matches[away=='newcastle utd',away:='newcastle']
matches[away=='stoke',away:='stoke city']
matches[away=='west-ham',away:='west ham']
matches[away %in% c('manchester-united','manchester-utd'), away:='manchester united']

# remove unknown scores & postponed matches
matches <- matches[!is.na(score)]
matches <- matches[!score=='POSTP.']

# date operations
matches[,timestamp:=as_datetime(date,tz='Turkey')]
matches[,date:=date(timestamp)]


# Data manipulation----
# adding scores separately
matches[,c('score_home','score_away'):=tstrsplit(score,':')]
matches$score_home <- as.numeric(matches$score_home)
matches$score_away <- as.numeric(matches$score_away)
matches[,totalgoals:=(score_home+score_away)]
matches[, winner:=ifelse(score_home>score_away,'home',
                         ifelse(score_home<score_away,'away','draw'))]

matches[,season:=ifelse(month(matches[,date])<7,
                         year(matches[,date]-1),
                         year(matches[,date]))]

seasons <- sort(unique(matches$season))

teams <- sort(unique(c(matches$home,matches$away)))


calculate <- reshape(matches, direction="wide",idvar="")
summary <- calculate[home==inputs_team, .(base="HOME",scored=mean(score_home),conceded=mean(score_away))]
summary <- rbind(summary,calculate[away==inputs_team, .(base="AWAY",scored=mean(score_away),conceded=mean(score_home))])
summary <- melt(summary, id.vars = "base", measure.vars = c("scored","conceded"))








