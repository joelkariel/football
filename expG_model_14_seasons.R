# VERY basic expected goals model, based on shots taken for H/A teams over last 14 seasons
  
exp_goals<-function(home_shots,away_shots,div){
  # Read in data on goals and shots
  df1<-read.csv(paste("http://www.football-data.co.uk/mmz4281/0102/",div,sep=""))[,c("FTHG","FTAG","HS","AS")]
  df2<-read.csv(paste("http://www.football-data.co.uk/mmz4281/0203/",div,sep=""))[,c("FTHG","FTAG","HS","AS")]
  df3<-read.csv(paste("http://www.football-data.co.uk/mmz4281/0304/",div,sep=""))[,c("FTHG","FTAG","HS","AS")]
  df4<-read.csv(paste("http://www.football-data.co.uk/mmz4281/0405/",div,sep=""))[,c("FTHG","FTAG","HS","AS")]
  df5<-read.csv(paste("http://www.football-data.co.uk/mmz4281/0506/",div,sep=""))[,c("FTHG","FTAG","HS","AS")]
  df6<-read.csv(paste("http://www.football-data.co.uk/mmz4281/0607/",div,sep=""))[,c("FTHG","FTAG","HS","AS")]
  df7<-read.csv(paste("http://www.football-data.co.uk/mmz4281/0708/",div,sep=""))[,c("FTHG","FTAG","HS","AS")]
  df8<-read.csv(paste("http://www.football-data.co.uk/mmz4281/0809/",div,sep=""))[,c("FTHG","FTAG","HS","AS")]
  df9<-read.csv(paste("http://www.football-data.co.uk/mmz4281/0910/",div,sep=""))[,c("FTHG","FTAG","HS","AS")]
  df10<-read.csv(paste("http://www.football-data.co.uk/mmz4281/1011/",div,sep=""))[,c("FTHG","FTAG","HS","AS")]
  df11<-read.csv(paste("http://www.football-data.co.uk/mmz4281/1112/",div,sep=""))[,c("FTHG","FTAG","HS","AS")]
  df12<-read.csv(paste("http://www.football-data.co.uk/mmz4281/1213/",div,sep=""))[,c("FTHG","FTAG","HS","AS")]
  df13<-read.csv(paste("http://www.football-data.co.uk/mmz4281/1314/",div,sep=""))[,c("FTHG","FTAG","HS","AS")]
  df14<-read.csv(paste("http://www.football-data.co.uk/mmz4281/1415/",div,sep=""))[,c("FTHG","FTAG","HS","AS")]
  # Merge the dfs into one
  df<-rbind(df1,df2,df3,df4,df5,df6,df7,df8,df9,df10,df11,df12,df13,df14)
  # Remove NAs
  df<-df[complete.cases(df),]
  # Summarise data
  home<-ddply(df,.(HS),summarize
          ,freq=length(HS)
          ,av_goals=mean(FTHG) 
          ,total_shots=sum(HS)
          ,total_goals=sum(FTHG)
          )
  home$conv<-home$total_goals/home$total_shots*100
  away<-ddply(df,.(AS),summarize
              ,freq=length(AS)
              ,av_goals=mean(FTAG)
              ,total_shots=sum(AS)
              ,total_goals=sum(FTAG)
  )
  away$conv<-away$total_goals/away$total_shots*100
  # Distribution of total shots ONLY
  #hist(df$HS, 100, freq=FALSE)
  #hist(df$AS, 100, freq=FALSE)
  # Subset df for X shots for H/A team
    dfh<-subset(df,df[["HS"]]==home_shots)
    h_mean<-mean(dfh$FTHG)
    h_sd<-sd(dfh$FTHG)

    dfa<-subset(df,df[["AS"]]==away_shots)
    a_mean<-mean(dfa$FTAG)
    a_sd<-sd(dfa$FTAG)
  # Plot histograms
    hh<-ggplot(dfh, aes(x=FTHG)) + xlab("Home Goals") + xlim(c(min(dfh$FTHG),max(dfh$FTHG))) + geom_histogram(binwidth=1, aes(y=..density..)) + geom_vline(xintercept=h_mean, linetype="dotted", col="red")
    print(hh)
    
    aa<-ggplot(dfa, aes(x=FTAG)) + xlab("Away Goals") + xlim(c(min(dfa$FTAG),max(dfa$FTAG))) + geom_histogram(binwidth=1, aes(y=..density..)) + geom_vline(xintercept=a_mean, linetype="dotted", col="red")
    print(aa)
  # Plot expected score
    exp_score<-matrix(ncol=2,nrow=2)
    exp_score[1,1]<-h_mean
    exp_score[2,1]<-h_sd
    exp_score[1,2]<-a_mean
    exp_score[2,2]<-a_sd
    bp<-barplot(exp_score, names.arg=c("Home","Away")
                ,main = "Expected Goals", beside = TRUE
                ,col=c("darkgrey","lightgrey"), border = "white", axes = FALSE, cex.names=1)
    text(bp, 0, paste(round(exp_score, 3)) ,cex=1, pos=3, col = "white")
}
  exp_goals(8,12,"E0")