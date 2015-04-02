# This code will read in odds data to make an 'expected pts' table
    # Read the data - teams and odds
      df<-read.csv('http://www.football-data.co.uk/mmz4281/1415/E0.csv')[,c(3:4,24:47)]
    # Create H, D, A odds which are average of the 6 companies' odds
    # Divide this equation by 1 (i.e. get reciprocal - hence why 6 divided by summation)
    # We divide by 1 to get odds-based pr. of each result
      df$H<-6/(df$B365H+df$BWH+df$IWH+df$LBH+df$WHH+df$VCH)
      df$D<-6/(df$B365D+df$BWD+df$IWD+df$LBD+df$WHD+df$VCD)
      df$A<-6/(df$B365A+df$BWA+df$IWA+df$LBA+df$WHA+df$VCA)
    # Subset to keep just teams and three variables representing average odds
      df<-df[,c(1:2,27:29)]
    # In order to remove bookmakers' profit, we need to divide these probabilities
    # by the sum of all probabilities (because sum of H+D+A odds > 1)
      df$H<-df$H/(df$H+df$D+df$A)
      df$D<-df$D/(df$H+df$D+df$A)
      df$A<-df$A/(df$H+df$D+df$A)
    # Multiply pr. of result by points awarded to create columns for H & A expected points
      df$exp_ptsH<-(3*df$H+df$D)
      df$exp_ptsA<-(3*df$A+df$D)
    # Subset to keep just teams and expected points
      df<-df[,c(1:2,6:7)]
    # Sum expected points for each team - H and A, then sum the results
      # install.packages("plyr")
      # Home
      exp_ptsH_sum<-ddply(df,.(HomeTeam),summarize,x=sum(exp_ptsH))
      names(exp_ptsH_sum)<-c("team", "exp_pts")
      # Away
      exp_ptsA_sum<-ddply(df,.(AwayTeam),summarize,x=sum(exp_ptsA))
      names(exp_ptsA_sum)<-c("team", "exp_pts")
      # Sum
      exp_pts_table<-ddply(rbind(exp_ptsH_sum, exp_ptsA_sum), .(team), summarize, exp_pts = sum(exp_pts))
      exp_pts_table<-exp_pts_table[order(-exp_pts_table$exp_pts),]
      
      