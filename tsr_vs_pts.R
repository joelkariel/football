fn<-function(year,div,title){

#read in the dataset - teams, goals, shots, shots on target
df<-read.csv(paste("http://www.football-data.co.uk/mmz4281/",year,"/",div, sep=""))[,c(3:7, 12:15)]

#set some variables
teams<-as.vector(unique(df[,1]))
shots_for_H<-as.vector(length(teams))
shots_for_A<-as.vector(length(teams))
shots_vs_H<-as.vector(length(teams))
shots_vs_A<-as.vector(length(teams))
wins_h<-as.vector(length(teams))
wins_a<-as.vector(length(teams))
draws_h<-as.vector(length(teams))
draws_a<-as.vector(length(teams))


#get SHOTS FOR for each team
for(i in 1:length(teams)){
  dfh<-subset(df, df[["HomeTeam"]]==teams[i])
  shots_for_H[i]<-sum(dfh$HS)
}
for(i in 1:length(teams)){
  dfa<-subset(df, df[["AwayTeam"]]==teams[i])
  shots_for_A[i]<-sum(dfa$AS)
}
shots_for<-shots_for_H+shots_for_A

#get SHOTS AGAINST for each team
for(i in 1:length(teams)){
  dfh<-subset(df, df[["HomeTeam"]]==teams[i])
  shots_vs_H[i]<-sum(dfh$AS)
}
for(i in 1:length(teams)){
  dfa<-subset(df, df[["AwayTeam"]]==teams[i])
  shots_vs_A[i]<-sum(dfa$HS)
}
shots_vs<-shots_vs_H+shots_vs_A

#get wins and draws for each team
for(i in 1:length(teams)){
  dfwh<-subset(df, df[["FTR"]]=="H" & df[["HomeTeam"]]==teams[i])
  wins_h[i]<-nrow(dfwh)
}
for(i in 1:length(teams)){
  dfwa<-subset(df, df[["FTR"]]=="A" & df[["AwayTeam"]]==teams[i])
  wins_a[i]<-nrow(dfwa)
}
for(i in 1:length(teams)){
  dfdh<-subset(df, df[["FTR"]]=="D" & df[["HomeTeam"]]==teams[i])
  draws_h[i]<-nrow(dfdh)
}
for(i in 1:length(teams)){
  dfda<-subset(df, df[["FTR"]]=="D" & df[["AwayTeam"]]==teams[i])
  draws_a[i]<-nrow(dfda)
}

#get points
pts_h<-wins_h*3 + draws_h
pts_a<-wins_a*3 + draws_a
pts<-pts_h+pts_a

#make data frame for each team's tsr
data<-data.frame(teams, pts, shots_for, shots_vs, tsr=shots_for/(shots_for+shots_vs))

#sort by tsr
data<-data[order(data$tsr, decreasing=TRUE), ]

#regress pts on tsr
fit<-lm(data$pts~data$tsr)
#retrieve r2
r2<-summary(fit)$r.squared
a<-summary(fit)$coefficients[1,1]
b<-summary(fit)$coefficients[2,1]

#install ggplot2
install.packages("ggplot2")
library("ggplot2")

#plot relationship
graph<-qplot(data$tsr, data$pts, data, color=data$shots_for
             ,size=1
             ,xlim=c(min(data$tsr)-0.02,max(data$tsr)+0.02)
             ,ylim=c(min(pts)-5,max(pts)+5)
             ,xlab="TSR", ylab="Points", label=data$teams
             ,main=title)
x<-graph + geom_abline(intercept=a,slope=b,col="darkgrey") + geom_text(aes(label=ifelse(data$pts>(max(data$pts)/1.5),as.character(data$teams),'')), hjust=0.5, vjust=1.5) + theme(legend.position="none") #this removes the legend
print(x)
}

fn(1314,"E0","Premier League TSR vs. points 2013/14")