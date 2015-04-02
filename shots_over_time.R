fn<-function(year,div){
    # Read in data
      df<-read.csv(paste("http://www.football-data.co.uk/mmz4281/",year,"/",div, sep=""))#[,c(3:7, 12:15)]
    # Create new columns for TS, conversion rates and TSR for each team
      df$TS<-df$HS+df$AS
      df$id<-1:nrow(df) # Keep track of the ORDER of games - v. useful later!
      df$conv_H<-df$FTHG/df$HS
      df$conv_A<-df$FTAG/df$AS
      df$TSR_H<-df$HS/df$TS
      df$TSR_A<-df$AS/df$TS
    
    ## Set other useful objects ##
    # Make vector unique teams
      teams<-as.vector(unique(df[["HomeTeam"]]))
    # Get unique number of home games played by each team
      home_games<-as.data.frame(table(df$HomeTeam)) 
      names(home_games)<-c("team","games")
      away_games<-as.data.frame(table(df$AwayTeam))
      names(away_games)<-c("team","games")
      home_games$games<-home_games$games + away_games$games
      GP<-home_games
    # Make df in which to store all shot data
      shots_teams<-as.data.frame(matrix(nrow=max(GP$games), ncol=length(teams)))
      names(shots_teams)<-teams
    # Get total goals, shots and conversion rates
      sum_goals<-sum(df$FTHG)+sum(df$FTAG)
      sum_shots<-sum(df$TS)
      conv_rate<-sum_goals/sum_shots
    
    # Get table of shots by each team over each matchday
    # install.packages("plyr")
      home_shots<-ddply(df, .(HomeTeam,AwayTeam,HS,AS,id), function(df)sum(df$HS+df$AS))
    # Subset to keep relevant columns
      home_shots<-home_shots[,c(1,3,5)]
    # Rename columns
      names(home_shots)<-c("team", "shots", "id")
    # Repeat for away shots
      away_shots<-ddply(df, .(AwayTeam,HomeTeam,AS,HS,id), function(df)sum(df$HS+df$AS))
      away_shots<-away_shots[,c(1,3,5)]
      names(away_shots)<-c("team", "shots", "id")
    # Append two tables
      total_shots<-rbind(home_shots, away_shots)
    # Re-order by team and by id (i.e. matchday)
      total_shots<-total_shots[order(total_shots$team, total_shots$id),]
    # Replace id with matchday
      total_shots$matchday<-0
      total_shots<-total_shots[,c(1,2,4)]
      shots_by_matchday<-data.frame(matrix(nrow=0,ncol=ncol(total_shots)))
      for(i in 1:length(teams)){
        dfs<-(subset(total_shots, total_shots[["team"]]==teams[i]))
        # Make shots a five-game moving average
        for (i in 5:nrow(dfs)){
          dfs[i,"shots"] = mean(dfs[(i-5):i, "shots"])
        }
        dfs$matchday<-c(1:nrow(dfs))
        shots_by_matchday<-rbind(shots_by_matchday, dfs)
      }
      
    # Subset total_shots by team and place shot data in shots_teams
    # The shot data for each team is turned into a vector for easier manipulation
    # This is necessary in order to deal with the issue of teams playing too few games
    # In this case, the vector is too small for the shots_teams df so the code fails
      'for(i in 1:length(teams)){
        dfs<-(subset(total_shots, total_shots[["team"]]==teams[i]))
              v<-vector(mode="list", length=max(GP$games))
              v<-dfs[["shots"]]
          if (length(v)<max(GP$games)){
            # diff<-max(GP$games)-length(v)
              for (j in length(v):max(GP$games)-1){
                v[j+1]<-NA
              }
          }
        shots_teams[,teams[i]]<-v
      }'
        
    #install ggplot2
    #install.packages("ggplot2")
    library("ggplot2")
    
    # Plot the data
    x<-(ggplot(shots_by_matchday, aes(matchday, shots))
      + geom_line(aes(colour=team))
      + geom_text(data = shots_by_matchday[shots_by_matchday$matchday == max(GP$games),], aes(label = team, colour=team))
      + theme(legend.position="none"))
    print(x)
  
}

fn(1415,"F1")