# This code will read in odds data to make an 'expected pts' table
# Then it will be compared to the 'actual' pts accumulated by teams and compare

exp_pts<-function(year,div){
    # Read the data
      df<-read.csv(paste("http://www.football-data.co.uk/mmz4281/",year,"/",div, sep=""))
    # Deal with NAs in odds data - replace with average of ALL odds data
      odds<-c("B365H","B365D","B365A","BWH","BWD","BWA","IWH","IWD","IWA"
            ,"LBH","LBD","LBA","PSH","PSD","PSA","WHH","WHD","WHA"
            ,"VCH","VCD","VCA")
      df[,odds][is.na(df[,odds])]<-mean(unlist(!is.na(df[,odds])))
    # Create H, D, A odds which are average of the 6 companies' odds
    # Divide this equation by 1 (i.e. get reciprocal - hence why 6 divided by summation)
    # We divide by 1 to get odds-based pr. of each result
      df$H<-6/(df$B365H+df$BWH+df$IWH+df$LBH+df$WHH+df$VCH)
      df$D<-6/(df$B365D+df$BWD+df$IWD+df$LBD+df$WHD+df$VCD)
      df$A<-6/(df$B365A+df$BWA+df$IWA+df$LBA+df$WHA+df$VCA)
    # Subset to keep just teams and three variables representing average odds
      df<-df[,c("HomeTeam","AwayTeam","H","D","A")]
    # In order to remove bookmakers' profit, we need to divide these probabilities
    # by the sum of all probabilities (because sum of H+D+A odds > 1)
      df$H<-df$H/(df$H+df$D+df$A)
      df$D<-df$D/(df$H+df$D+df$A)
      df$A<-df$A/(df$H+df$D+df$A)
    # Multiply pr. of result by points awarded to create columns for H & A expected points
      df$exp_ptsH<-(3*df$H+df$D)
      df$exp_ptsA<-(3*df$A+df$D)
    # Subset to keep just teams and expected points
      df<-df[,c("HomeTeam","AwayTeam","exp_ptsH","exp_ptsA")]
    # Sum expected points for each team - H and A, then sum the results
      # Home
        exp_ptsH_sum<-ddply(df,.(HomeTeam),summarize,x=sum(exp_ptsH))
        names(exp_ptsH_sum)<-c("team", "exp_pts")
      # Away
        exp_ptsA_sum<-ddply(df,.(AwayTeam),summarize,x=sum(exp_ptsA))
        names(exp_ptsA_sum)<-c("team", "exp_pts")
      # Sum
        exp_pts_table<-ddply(rbind(exp_ptsH_sum, exp_ptsA_sum), .(team), summarize, exp_pts = sum(exp_pts))
        exp_pts_table<-exp_pts_table[order(-exp_pts_table$exp_pts),]
    # Get sequential numbers in 'row.names'
      row.names(exp_pts_table) <- NULL
     
    # Put exp_pts_table df in Global Environment
      assign("exp_pts_table", exp_pts_table, .GlobalEnv)
    
  }

pts<-function(year,div){
  # Read in the dataset - teams, goals, shots, shots on target
    df<-read.csv(paste("http://www.football-data.co.uk/mmz4281/",year,"/",div, sep=""))[,c(3:7, 12:15)]
  # Set some variables
    teams<-as.vector(unique(df[,1]))
    wins_h<-as.vector(length(teams))
    wins_a<-as.vector(length(teams))
    draws_h<-as.vector(length(teams))
    draws_a<-as.vector(length(teams))
  # Get wins and draws for each team
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
  # Get points
    pts_h<-wins_h*3 + draws_h
    pts_a<-wins_a*3 + draws_a
    pts<-pts_h+pts_a
  # Make a df of teams and their pts
    pts<-data.frame(teams, pts)
    names(pts)<-c("team","pts")
  # Put pts df in Global Environment
    assign("pts", pts, .GlobalEnv)
  
}

fn<-function(year,div,title){
  # Run the two functions for same season and division
    pts(year,div)
    exp_pts(year,div)
  # Merge the two output tables by team
    final_table<-merge(pts, exp_pts_table, by="team")
  # Regress pts on exp_pts
    fit<-lm(final_table$exp_pts~final_table$pts)
  # Retrieve r2, a, b
    r2<-summary(fit)$r.squared
    a<-summary(fit)$coefficients[1,1]
    b<-summary(fit)$coefficients[2,1]
  #install ggplot2
    #install.packages("ggplot2")
    library("ggplot2")
  # Plot the relationship
    graph<-qplot(final_table$pts, final_table$exp_pts, final_table, color=final_table$pts
                 ,size=1
                 ,xlab="Points", ylab="Expected Points (odds)", label=final_table$team
                 ,main=title
                 )
    x<-graph + geom_abline(intercept=a,slope=b,col="darkgrey") + geom_text(aes(label=ifelse(final_table$exp_pts>max(final_table$exp_pts)/1.5,as.character(final_table$team),'')), size=5, hjust=0.5, vjust=1.5) + theme(legend.position="none")
    print(x)
}

fn(1314,"F1", "Bundesliga performance vs. expectation 2013/14")

