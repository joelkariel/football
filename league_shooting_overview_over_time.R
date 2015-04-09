y<-function(div){
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
  
  l<-list(df1,df2,df3,df4,df5,df6,df7,df8,df9,df10,df11,df12,df13,df14)
  
  s<-sapply(l,colMeans, na.rm=TRUE)
  s<-melt(s, c("type","year"))
  g<-(ggplot(s, aes(year,value)) + geom_line(aes(colour=type)) + facet_wrap(~type))
  print(g)
}
y("E0")
  
  