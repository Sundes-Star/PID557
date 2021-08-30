# Upsets vs Reg for all years:

# upsets:
U <- ATPSeasons %>%
  filter(Upset15 == 1)

# Regular:
R <- ATPSeasons %>%
  filter(Upset15 == 0)

###### 1 Newdf <- U
###### 2 Newdf <- R

Newdf <- R

# Aces

# #Number of aces as a predictor
Yearsdf = c(2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018)
ace_per <- vector()
n <- 1
for (y in Yearsdf){
  df <- Newdf %>% filter(id == y)
  ace_dif<- na.omit(df$w_ace) - na.omit(df$l_ace)
  
  ace_dif <- ifelse(ace_dif > 0 , 1 , 0)
  
  #what percentage of time did winner hit more aces than the loser
  ace_per[n] <- sum(ace_dif, na.rm = TRUE)*100/length(ace_dif)
  n <- n +1
}

# statistics:
summary(Season2014$w_ace)
summary(Season2014$l_ace)

# .......................

# Double faults
#Number of double faults as a predictor
# difference between number of double faults hit by the loser and the winner

df_per <- vector()
n <- 1
for (y in Yearsdf){
  df <- Newdf %>% filter(id == y)
  df_dif<- na.omit(df$l_df) - na.omit(df$w_df)
  
  df_dif <- ifelse(df_dif > 0 , 1 , 0)
  
  #what percentage of time did losers make more double faults than the winners
  df_per[n] <- sum(df_dif, na.rm = TRUE)*100/length(df_dif)
  n <- n +1
}

# statistics:
summary(Season2014$w_ace)
summary(Season2014$l_ace)

# .......................

# The first serve percentage

fsp_per <- vector()
n <- 1
for (y in Yearsdf){
  df <- Newdf %>% filter(id == y)
  
  p1spw <- df$w_1stIn/df$w_svpt    # winner
  p1spl <- df$l_1stIn/df$l_svpt    # loser
  
  p1sd <- ifelse((p1spw - p1spl) > 0 , 1 , 0)
  
  #percentage of time winner had a better 1st serve percentage
  fsp_per[n] <- sum(p1sd, na.rm = TRUE)*100/length(p1sd)
  
  n <- n +1
}

# .......................

#First and second serve winning percentages

# First

fswp_per <- vector()
n <- 1
for (y in Yearsdf){
  df <- Newdf %>% filter(id == y)
  
  w1swp <- df$w_1stWon/ df$w_1stIn    
  w1slp <- df$l_1stWon/ df$l_1stIn
  
  f1swpd <- ifelse((w1swp - w1slp) > 0 , 1 , 0)
  
  #percentage of time the winning player had a higher winning percentage on 1st serve
  fswp_per[n] <- sum(f1swpd, na.rm = TRUE)*100/length(f1swpd)
  
  n <- n +1
}

# Second

S2swp_per <- vector()
n <- 1
for (y in Yearsdf){
  df <- Newdf %>% filter(id == y)
  
  w2swp <- df$w_2ndWon/ (df$w_svpt- df$w_1stIn)     
  w2slp <- df$l_2ndWon/ (df$l_svpt- df$l_1stIn)
  
  s2swpd <- ifelse((w2swp - w2slp) > 0 , 1 , 0)
  
  #percentage of time the winning player had a higher winning percentage on 2nd serve
  S2swp_per[n] <- sum(s2swpd, na.rm = TRUE)*100/length(s2swpd)
  
  n <- n +1
}

# .......................

# Break Points
# Break points faced

bpf_per <- vector()
n <- 1
for (y in Yearsdf){
  df <- Newdf %>% filter(id == y)
  
  bpfd <- ifelse((df$w_bpFaced - df$l_bpFaced) > 0 , 1 , 0)
  
  #percentage 
  bpf_per[n] <- sum(bpfd, na.rm = TRUE)*100/length(bpfd)
  
  n <- n +1
}

# break points saved

bps_per <- vector()
n <- 1
for (y in Yearsdf){
  df <- Newdf %>% filter(id == y)
  
  df$w_bpSavedp <- ifelse(df$w_bpFaced == 0, 0, df$w_bpSaved*100/df$w_bpFaced)
  df$l_bpSavedp <- ifelse(df$l_bpFaced == 0, 0, df$l_bpSaved*100/df$l_bpFaced)
  
  bpsd <- ifelse((df$w_bpSavedp - df$l_bpSavedp) > 0 , 1 , 0)
  
  #percentage 
  bps_per[n] <- sum(bpsd, na.rm = TRUE)*100/length(bpsd)
  
  n <- n +1
}

# Return Games Won %

Ret_per <- vector()
n <- 1
for (y in Yearsdf){
  df <- Newdf %>% filter(id == y)
  
  df$w_Retpwp <- (df$l_svpt-(df$l_1stWon+df$l_2ndWon))*100/df$l_svpt
  df$l_Retpwp <- (df$w_svpt-(df$w_1stWon+df$w_2ndWon))*100/df$w_svpt
  
  Retd <- ifelse((df$w_Retpwp - df$l_Retpwp) > 0 , 1 , 0)
  
  #percentage 
  Ret_per[n] <- sum(Retd, na.rm = TRUE)*100/length(Retd)
  
  n <- n +1
}


# data frame of percentage difference:

per_diff_stat <- data.frame(matrix(ncol = 11, nrow = 8))
x <- c("Stat", "2009", "2010", "2011", "2012", "2013", "2014", "2015", "2016", "2017", "2018")

colnames(per_diff_stat) <- x

# stats of interest:
per_diff_stat$Stat <- c("Aces", "Double Faults", "First Serve Percentage", 
                        "1st Serve Win %", "2nd Serve Win %", 
                        "Break Points Saved", "Break Points Faced", "Return Games Won %")

mStat <- data.frame(ace_per, df_per, fsp_per, fswp_per, S2swp_per, bpf_per, bps_per, Ret_per)
n <- 1
for(n in 1:length(mStat)){
  per_diff_stat[n,2:11] <- mStat[,n]
  n <- n+1
}

mStat2<- round(mStat,2)
write.csv(mStat2,"mStat.csv", row.names = FALSE)
