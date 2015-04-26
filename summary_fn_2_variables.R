fn<-function(year,div,metric1,metric2){
  
  # Packages needed
  require(stringi)
  require(ggplot2)
  
  # Import relevant data
  df<-read.csv(paste("http://www.football-data.co.uk/mmz4281/",year,"/",div, sep=""))[c(3:7, 12:15)]
  
  # Create shot-related metrics
  df$TS<-df$HS+df$AS
  df$TST<-df$HST+df$AST
  df$conv_H<-df$FTHG/df$HS
  df$conv_A<-df$FTAG/df$AS
  df$TSR_H<-df$HS/df$TS
  df$TSR_A<-df$AS/df$TS
  df$pts_H<-ifelse(df$FTR=="H",3,ifelse(df$FTR=="A",0,1))
  df$pts_A<-ifelse(df$FTR=="A",3,ifelse(df$FTR=="H",0,1))
  df$TSTR_H<-df$HST/df$TST
  df$TSTR_A<-df$AST/df$TST
  
  # Create vector of unique teams
  teams<-as.vector(unique(df[["HomeTeam"]]))
  
  # Create empty df for data
  all<-as.data.frame(matrix(nrow=length(teams), ncol=3))
  names(all)<-c("team", metric1, metric2)
  all[,1]<-teams
  
  # Subset for each team and create average for relevant metric
  for(i in 1:length(teams)){
    # Subset for home and away and keep only relevant variables
    dfh<-df[df$HomeTeam == teams[i],c(1,3,6,8,12,14,16,18)]
    dfa<-df[df$AwayTeam == teams[i],c(2,4,7,9,13,15,17,19)]
    # Rename variables for dfh and dfa
    var<-c("Team","Goals","Shots","Shots on Target","Conversion Rate","TSR","PPG","TSoTR")
    names(dfh)<-var
    names(dfa)<-var
    # Keep only relevant metrics
    m1h<-dfh[,c(metric1)]
    m1a<-dfa[,c(metric1)]
    m2h<-dfh[,c(metric2)]
    m2a<-dfa[,c(metric2)]
    # Merge home and away data for each metric
    temp1<-c(m1h,m1a) 
    temp2<-c(m2h,m2a)
    # Place mean of metrics in dataframe
    all[i,2]<-mean(temp1)
    all[i,3]<-mean(temp2)
  }
  # Sort by metric1
  all<-all[order(all[,2], decreasing=TRUE),]
  # Remove row.names variable created by sorting
  rownames(all)<-NULL
  # View output
  View(all)
  # Plot relationship
  m1<-metric1 # Create these for the title
  m2<-metric2
  #stri_trans_general(x, id = "Title") # Formats strings as 'proper'
  #as.numeric(strsplit(as.character(year), "")[[1]])[1:2] # Splits integer up
  graph<-(qplot(all[,2], all[,3], all, color=all[,1]
               ,xlim=c(0.9*(min(all[,2])), 1.1*(max(all[,2]))), ylim=c(0.9*(min(all[,3])), 1.1*(max(all[,3])))
               ,xlab=metric1, ylab=metric2, label=all$team
               ,main=paste(m1," vs. ",m2," (League ",div,") ",year,sep="")
               )
          + geom_text(aes(label=as.character(all$team)), hjust=0.5, vjust=1.5)
          + theme(legend.position="none") # This removes the legend
          )
  print(graph)
}

fn(1415,"E0","PPG","Shots on Target")

