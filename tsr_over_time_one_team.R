tsr<-function(team){
  
  #read in the dataset - teams, goals, shots, shots on target
  df<-read.csv('http://www.football-data.co.uk/mmz4281/1415/E0.csv')[,c(3:7, 12:15)]
  
  #subset to team passed through function
  df<-subset(df, df[["HomeTeam"]]==team | df[["AwayTeam"]]==team)  
  
  #create new column TS (total shots) of zeros
  df$TS<-0
  
  #fill TS column with HS + AS
  for (i in 1:nrow(df)){
    df[[i,"TS"]]<-df[[i,"HS"]]+df[[i,"AS"]]
  }
  
  #make empty vectors to store number of shots for and vs team 
  shots_for<-as.vector(nrow(df))
  shots_vs<-as.vector(nrow(df))
  
  #for each row, add the number of shots for to the shots for vector (depending if home or away)
  for (i in 1:nrow(df)){
    if (df[[i,"HomeTeam"]]==team) {
      shots_for[i]<-df[[i,"HS"]]
    }
    else {
      shots_for[i]<-df[[i,"AS"]]
    }
  }
  
  #for each row, add the number of shots vs to the shots vs vector (depending if home or away)
  for (i in 1:nrow(df)){
    if (df[[i,"HomeTeam"]]==team) {
      shots_vs[i]<-df[[i,"AS"]]
    }
    else {
      shots_vs[i]<-df[[i,"HS"]]
    }
  }
  
  #make TSR
  tsr<-as.vector(shots_for/df[["TS"]])
  
  #find average TSR
  av_tsr<-mean(tsr)
  
  #make tsr a 5 game moving average
  for (i in 5:length(tsr)){
    tsr[i] = mean(tsr[(i-5):i])
  }
  
  #create matchday vector
  matchday<-as.vector(1:nrow(df))
  
  #plot tsr over time
  plot(matchday,tsr, main = paste(team,"Premier League TSR 2014/15"))
  lines(matchday,tsr, type="o", col="navy")
  abline(h=av_tsr, col="red")
  
  #To do: make this comparative - can choose 2(+?) teams to compare
}
