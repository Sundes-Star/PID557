
# Read in Files:

# Adapted From:
# https://blog.exploratory.io/how-to-read-multiple-excel-or-csv-files-together-42af5d314a10
# Since all file share the same data structure, Combine the dataaset into one data frame for easy manipulation 
datset <-list.files(pattern="*.csv")
ATPSeasons <- sapply(datset, read_csv, simplify=FALSE) %>% 
  bind_rows(.id = "id")

ATPSeasons %>% group_by(tourney_name) %>%
  count(tourney_name)
# 827 tournments


#################################################################################
# Manipulation:

# Cleaning:

ATPSeasons %>% 
  filter(score %in% c("RET", "W/O")) 
# 171

# Delete rows with incomplete matches
ATPSeasons<-ATPSeasons[!(ATPSeasons$score=="RET" | ATPSeasons$score=="W/O"),]

# drop rows that have NA statistics:
ATPSeasons<-ATPSeasons[!(is.na(ATPSeasons$w_ace)),]   # when ace is NA, mostly, the rest of the statistics are NA

# Exclude Davis Cup: 
ATPSeasons <- ATPSeasons %>%
  filter(tourney_level != "D")

dim(ATPSeasons)
# 50 * 26507    

# id as a year for easy analysis by year:
ATPSeasons$id <-
  ifelse(
    ATPSeasons$id == "atp_matches_2009.csv",
    "2009",
    ifelse(
      ATPSeasons$id == "atp_matches_2010.csv",
      "2010",
      ifelse(
        ATPSeasons$id == "atp_matches_2011.csv",
        "2011",
        ifelse(
          ATPSeasons$id == "atp_matches_2012.csv",
          "2012",
          ifelse(
            ATPSeasons$id == "atp_matches_2013.csv",
            "2013",
            ifelse(
              ATPSeasons$id == "atp_matches_2014.csv",
              "2014",
              ifelse(
                ATPSeasons$id == "atp_matches_2015.csv",
                "2015",
                ifelse(
                  ATPSeasons$id == "atp_matches_2016.csv",
                  "2016",
                  ifelse(
                    ATPSeasons$id == "atp_matches_2017.csv",
                    "2017",
                    ifelse(ATPSeasons$id == "atp_matches_2018.csv", "2018", NA)
                  )
                )
              )
            )
          )
        )
      )
    )
  )

####################################################################################################################

# EDA

# number of matches per level:
table(ATPSeasons$tourney_level)

# percentage of each level
# reorder the level according to tier:
ATPSeasons <- arrange(transform(ATPSeasons,
                                tourney_level=factor(tourney_level,levels=c("G", "M", "A","F","D"))),tourney_level)

ATPSeasons <- arrange(transform(ATPSeasons,
                                round=factor(round,levels=c("F", "SF", "QF", "R16", "R32", "R64", "R128", "PR"))),round)

ATPSeasonsLp <- ATPSeasons %>% 
  group_by(id) %>%
  count(tourney_level) %>% 
  mutate(perc = n*100 / sum(n)) 

ATPSeasonsLp %>%
  group_by(id) %>%
  ggplot(aes(x = tourney_level, y = perc)) + 
  geom_bar(stat = "identity", fill='steelblue', color='black') +
  theme(text = element_text(size = 20)) +
  geom_text(aes(label = percent(round(perc)/100), vjust = 0.5),size = 7) +
  facet_wrap(id ~.) +
  xlab("Tourney Level") + ylab("Percentage %")

# ..................

# Surface

# percentage of each surface
ATPSeasonsLp <- ATPSeasons %>% 
  group_by(id) %>%
  count(surface) %>% 
  mutate(perc = n*100 / sum(n)) 

ATPSeasonsLp %>%
  group_by(id) %>%
  ggplot(aes(x = surface, y = perc)) + 
  geom_bar(stat = "identity", fill='steelblue', color='black') +
  theme(text = element_text(size = 20)) +
  geom_text(aes(label = percent(round(perc)/100), vjust = 1),size = 7) +
  facet_wrap(id ~.) +
  xlab("Surface") + ylab("Percentage %")

# ..................

# winner_hand
table(ATPSeasons$winner_hand)

ATPSeasons$winner_hand[is.na(ATPSeasons$winner_hand)] <- "U"

sum(is.na(ATPSeasons$winner_hand))


# loser_hand

table(ATPSeasons$loser_hand)

ATPSeasons$loser_hand[is.na(ATPSeasons$loser_hand)] <- "U"

sum(is.na(ATPSeasons$loser_hand))

# ..................

# Age difference distribution:
ATPSeasons$agedif <- ATPSeasons$winner_age - ATPSeasons$loser_age

# Adapted From:
# https://www.kaggle.com/ambarish/omnibus-womens-and-mens-tennis-matches-analysis 

ATPSeasons %>%
  ggplot(aes(x = agedif)) +
  geom_density(fill = "steelblue") +
  labs(x= 'Age Difference',y = 'Count', title = paste("Distribution of Age Difference")) +
  theme_bw()   

sd(ATPSeasons$agedif, na.rm = TRUE)
# 5.365677

# age mean
plot1 <- ATPSeasons %>%
  group_by(id) %>%
  summarise(Winners = mean(winner_age, na.rm = TRUE), Losers = mean(loser_age, na.rm = TRUE) ) %>%
  pivot_longer(c(Winners, Losers), names_to = "Type", values_to = "Age") %>%
  ggplot(aes(id, Age, group = Type, color = Type)) + geom_line(size = 1) +
  labs(title = paste(" (a) Distribution of Age Difference")) +
  theme(text = element_text(size = 20), legend.position = "none") +
  labs(x= "Year", y="Mean Age")

# ..................

# Hight difference distribution:
ATPSeasons$htdif <- ATPSeasons$winner_ht - ATPSeasons$loser_ht

# Adapted From:
# https://www.kaggle.com/ambarish/omnibus-womens-and-mens-tennis-matches-analysis 

ATPSeasons %>%
  ggplot(aes(x = htdif)) +
  geom_density(fill = "steelblue") +
  labs(x= 'Height Difference',y = 'Count', title = paste("Distribution of Height Difference")) +
  theme_bw() 

sd(ATPSeasons$htdif, na.rm = TRUE)
# 10.11299

# age mean
plot2 <- ATPSeasons %>%
  group_by(id) %>%
  summarise(Winners = mean(winner_ht, na.rm = TRUE), Losers = mean(loser_ht, na.rm = TRUE) ) %>%
  pivot_longer(c(Winners, Losers), names_to = "Type", values_to = "Hight") %>%
  ggplot(aes(id, Hight, group = Type, color = Type)) + geom_line(size = 1) +
  labs(title = paste(" (b) Distribution of Height Difference")) +
  theme(text = element_text(size = 20)) +
  labs(x= "Year", y="Mean Height")


grid.arrange(plot1, plot2, ncol=2)

# ..................

# best of
table(ATPSeasons$best_of)
# 3     5 
# 21445  5062 

# ..................

# who won the most matches each season in the last 10 years before 2019? 
winnersT<- ATPSeasons %>% 
  group_by(id, winner_name)%>%
  mutate(wins = n())%>%
  arrange(id) 

summary(winnersT)

# Investigate correlation between Tournaments wins and Wins number / win ratio each season:

# wins number:
ATPSeasons <- ATPSeasons %>% 
  group_by(id, winner_name)%>%
  mutate(winers_wins = n())

# Win ratio:
# loses of winners:
Lossdf <- ATPSeasons %>% group_by(id, loser_name)%>%
  mutate(winers_loses = n())

# join 2 df:
ATPSeasons <- ATPSeasons %>% 
  inner_join(Lossdf, by = c("winner_name" = "loser_name", "id" = "id")) %>% 
  mutate(winner_wr = winers_wins*100/(winers_wins + winers_loses))

# Tournaments wins:
TwinnersT <- ATPSeasons %>% filter(round == "F") %>%
  group_by(id, winner_name)%>%
  mutate(Twins = n())

# correlation:

Win_Cor_Season <- TwinnersT %>% group_by(id)%>%
  summarise(Tour_Wnum = cor(Twins,winers_wins), Tour_WRatio = cor(Twins,winner_wr))
# plot:

Win_Cor_Season %>%
  pivot_longer(c(Tour_Wnum, Tour_WRatio), names_to = "Type", values_to = "Corr") %>%
  ggplot(aes(x = id, y = Corr, group = Type, color = Type)) +
  geom_line(size =1) + theme(axis.text.x = element_text(hjust = 1)) +
  theme(text = element_text(size = 20)) +
  labs(x= "Year", y="Correlation Ratio")

cor.test(TwinnersT$Twins,TwinnersT$winers_wins)   # 2.2e-16
cor.test(TwinnersT$Twins,TwinnersT$winner_wr)     # 2.2e-16


# inspect 2014 season:
Twinners2014 <- TwinnersT %>% filter(id == "2014") %>%
  group_by(winner_name)%>%
  summarize(Twins = n())%>%
  arrange(desc(Twins))

model1 <- lm(Twins ~ winers_wins, data = TwinnersT)
summary(model1)

model2 <- lm(Twins ~ winner_wr, data = TwinnersT)
summary(model2)

# .................

# Investigate winners performance per surface in the Final rounds:
# the winner of the final round will be the tournament winner.

## Tournament winners by surface: Players win ratio per surface for the tournament winners

Twinners2014 %>% 
  inner_join(ATPSeasons %>% filter(id == "2014"), by = "winner_name", suffix = c("_19", "_TW") ) %>%
  group_by(winner_name) %>%
  count(surface) -> TWW

Twinners2014 %>% 
  inner_join(ATPSeasons %>% filter(id == "2014"), by = c("winner_name" = "loser_name")) %>%
  group_by(winner_name) %>%
  count(surface) -> TWL

TWW %>% 
  inner_join(TWL, by = "winner_name", suffix = c("_W", "_L") ) -> df1

df1$surface <- NA
df1$Sprec <- NA

df1 <- df1 %>% pivot_wider(names_from = surface_W, values_from = n_W, values_fill = 0) %>%
  pivot_longer(c(Clay, Grass, Hard), names_to = "surface_W", values_to = "n_W") %>%
  pivot_wider(names_from = surface_L, values_from = n_L, values_fill = 0) %>%
  pivot_longer(c(Clay, Grass, Hard), names_to = "surface_L", values_to = "n_L") 

df1$surface <- unlist(df1$surface)

for (i in 1:nrow(df1)) {
  if (df1[i,4] == df1[i,6]){
    df1$surface[i] <- df1[i,4]
    df1$Sprec[i] <-  df1$n_W[i]*100/(df1$n_W[i]+ df1$n_L[i]) 
  }
}

df1 <- na.omit(df1)
df1$surface <- unlist(df1$surface)

df1 %>% 
  ggplot(aes(winner_name, y=Sprec, fill = surface)) +
  geom_col() + theme(text = element_text(size = 20), axis.text.x = element_text(angle = 90, hjust = 1)) +
  geom_text(aes(label = round(Sprec,2)), position = position_stack(vjust = 0.5), fontface = "bold")+
  xlab("Winner Name") + ylab("Percentage of Wins %")

df1 %>% group_by(winner_name)
# ..................

# Players' Ranks:

Season2014 <- ATPSeasons %>% filter(id == "2014")

TSeason2014 <- TwinnersT %>% filter(id == "2014")

summary(Season2014$winner_rank)
sum(is.na(Season2014$winner_rank))

g1 <- Season2014 %>% 
  ggplot(aes(x = winner_rank)) + 
  geom_density(aes( y=..scaled..), fill='steelblue', color='black') +
  theme(text = element_text(size = 20), axis.text.x = element_text(hjust = 0.5)) +
  xlim(1,503) + xlab("Winner Rank") + ylab("Scale") +
  ggtitle(" (a) The Density Estimate of Winners Ranks") +
  theme(plot.title = element_text(size = 20, face = "bold"))


g2 <- Season2014 %>% 
  filter(winner_rank <=73)%>%
  count(winner_rank) %>%
  ggplot(aes(x = winner_rank, y = n)) + 
  geom_col(fill='steelblue', color='black') +
  theme(text = element_text(size = 20), axis.text.x = element_text(hjust = 0.5)) +
  xlim(1,76) + xlab("Winner Rank") + ylab("Count") + 
  ggtitle(" (b) The 75th percentile of Winners Ranks") +
  theme(plot.title = element_text(size = 20, face = "bold"))


# The distribution is sort of uniform especially form rank 10 to 76

summary(TSeason2014$winner_rank)

g3 <- ggplot(TSeason2014 %>% filter(winner_rank <=30), aes(x=winner_rank, fill = surface)) + 
  geom_bar() +
  theme(text = element_text(size = 20), axis.text.x = element_text(hjust = 0.5)) +
  facet_wrap(tourney_level~.) +
  xlab("Winner Rank") +  ylab("Count") +
  ggtitle(" (c) Ranks Distribution of Tournament Winners by Level and Surface ") +
  theme(plot.title = element_text(size = 20, face = "bold"))


grid.arrange(arrangeGrob(g1), 
             arrangeGrob(g2,g3, ncol=1), 
             ncol=2, widths=c(1,1))


# ..................
# Player seeds
Season2014 <- ATPSeasons %>% filter(id == "2014")

Season2014 %>%
  arrange(winner_rank) %>%
  group_by(winner_rank) %>%
  count() 

Season2014 %>% 
  count(winner_rank) %>%
  ggplot(aes(x = winner_rank)) + 
  geom_histogram(fill='steelblue') +
  theme(axis.text.x = element_text(angle = 90, hjust = 0.5)) +
  scale_x_continuous("Winner Seed", labels = as.character(Season2014$winner_rank), breaks = Season2014$winner_rank) +
  ylab("Count")

table(Season2014$winner_rank, Season2014$tourney_level)

ATPSeasons %>% 
  filter(winner_rank <=100) %>%
  count(winner_rank) %>%
  ggplot(aes(x = winner_rank)) + 
  geom_histogram(fill='steelblue') +
  theme(axis.text.x = element_text(angle = 90, hjust = 0.5)) +
  scale_x_continuous("Winner Rank", labels = as.character(Season2014$winner_rank), breaks = Season2014$winner_rank) +
  theme(text = element_text(size = 20)) +
  xlab("Winner Rank") + ylab("Count") + xlim(0, 100) + facet_wrap(id~.) 

# * higher seeded players win more matches than lower ranked seed players.(para)

# Are seeds more likely to win tournaments?
# Seeds
TwinnersT %>%
  group_by(winner_seed) %>%
  count() %>%
  arrange(desc(n)) 

ggplot(TwinnersT, aes(x=winner_seed, fill = tourney_level)) + 
  geom_histogram() + 
  theme(text = element_text(size = 20)) +
  xlab("Winner Seed") + ylab("Count") + 
  facet_wrap(id~.) 


# Ranks
TwinnersT %>%
  group_by(winner_rank) %>%
  count() %>%
  arrange(desc(n)) 

ggplot(TwinnersT, aes(x=winner_rank, fill = tourney_level)) + 
  geom_histogram() + 
  theme(text = element_text(size = 20)) +
  xlab("Winner Seed") + ylab("Count") + xlim(0, 300) +
  facet_wrap(id~.) 

#################################################################################################################

# Match Statistics: Winners and Losers

# Aces

# #Number of aces as a predictor
Yearsdf = c(2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018)
ace_per <- vector()
n <- 1
for (y in Yearsdf){
  df <- ATPSeasons %>% filter(id == y)
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

# Adapted From:
# https://medium.com/swlh/predicting-atp-tennis-match-outcomes-using-serving-statistics-cde03d99f410
# Double faults faults as a predictor
# difference between number of double faults hit by the loser and the winner

df_per <- vector()
n <- 1
for (y in Yearsdf){
  df <- ATPSeasons %>% filter(id == y)
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
  df <- ATPSeasons %>% filter(id == y)
  
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
  df <- ATPSeasons %>% filter(id == y)
  
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
  df <- ATPSeasons %>% filter(id == y)
  
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
  df <- ATPSeasons %>% filter(id == y)
  
  bpfd <- ifelse((df$w_bpFaced - df$l_bpFaced) > 0 , 1 , 0)
  
  #percentage 
  bpf_per[n] <- sum(bpfd, na.rm = TRUE)*100/length(bpfd)
  
  n <- n +1
}

# break points saved

bps_per <- vector()
n <- 1
for (y in Yearsdf){
  df <- ATPSeasons %>% filter(id == y)
  
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
  df <- ATPSeasons %>% filter(id == y)
  
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

mStat <- data.frame(ace_per, df_per, fsp_per, fswp_per, S2swp_per, bps_per, bpf_per, Ret_per)
n <- 1
for(n in 1:length(mStat)){
  per_diff_stat[n,2:11] <- mStat[,n]
  n <- n+1
}


################################################################################
################################################################################
################################################################################

## Upset matches: 

# Define: 
# rank difference > 15
# seeds were inspected and they are not very accurate
# will build on ranks

## How many times occur in the last 10 years? 

# Create upset column : (1 means the match was an upset)

ATPSeasons$Upset15 <- ifelse(ATPSeasons$winner_rank - ATPSeasons$loser_rank > 15,  1, 0)

ATPSeasons %>%
  group_by(id) %>%
  summarise(upsets_num = sum(Upset15, na.rm = TRUE)) %>%
  ggplot(aes(id, upsets_num, group = 1)) + geom_line(size = 1) +
  theme(text = element_text(size = 20))+
  labs(x= "Year", y="Number of Upset Matches")


# Investigate upset matches: any patterns?

# remove NA:
ATPSeasons <- ATPSeasons[!(is.na(ATPSeasons$Upset15)),]


# Surface

# percentage of each surface
# upset:
ATPSeasons %>% 
  filter(Upset15 == 1) %>%
  group_by(id) %>%
  count(surface) %>% 
  mutate(perc = n*100 / sum(n)) %>%
  ggplot(aes(x = surface, y = perc)) + 
  geom_bar(stat = "identity",fill='steelblue') +
  geom_text(aes(label = percent(round(perc)/100), vjust = -0.5)) +
  facet_wrap(id ~.)

# regular:
ATPSeasons %>% 
  filter(Upset15 == 0) %>%
  group_by(id) %>%
  count(surface) %>% 
  mutate(perc = n*100 / sum(n)) %>%
  ggplot(aes(x = surface, y = perc)) + 
  geom_bar(stat = "identity",fill='steelblue') +
  geom_text(aes(label = percent(round(perc)/100), vjust = -0.5)) +
  facet_wrap(id ~.)

# .................................

# tourney_level

# upset:
ATPSeasons %>% 
  filter(Upset15 == 1) %>%
  group_by(id) %>%
  count(tourney_level) %>% 
  mutate(perc = n*100 / sum(n)) %>%
  ggplot(aes(x = tourney_level, y = perc)) + 
  geom_bar(stat = "identity",fill='steelblue') +
  geom_text(aes(label = percent(round(perc)/100), vjust = -0.5)) +
  facet_wrap(id ~.)

# regular:
ATPSeasons %>% 
  filter(Upset15 == 0) %>%
  group_by(id) %>%
  count(tourney_level) %>% 
  mutate(perc = n*100 / sum(n)) %>%
  ggplot(aes(x = tourney_level, y = perc)) + 
  geom_bar(stat = "identity",fill='steelblue') +
  geom_text(aes(label = percent(round(perc)/100), vjust = -0.5)) +
  facet_wrap(id ~.)

# .................................

# round:

# upset:
ATPSeasons %>% 
  filter(Upset15 == 1) %>%
  group_by(id) %>%
  count(round) %>% 
  mutate(perc = n*100 / sum(n)) %>%
  ggplot(aes(x = round, y = perc)) + 
  geom_bar(stat = "identity",fill='steelblue') +
  geom_text(aes(label = percent(round(perc)/100), vjust = -0.5)) +
  facet_wrap(id ~.)

# regular:
ATPSeasons %>% 
  filter(Upset15 == 0) %>%
  group_by(id) %>%
  count(round) %>% 
  mutate(perc = n*100 / sum(n)) %>%
  ggplot(aes(x = round, y = perc)) + 
  geom_bar(stat = "identity",fill='steelblue') +
  geom_text(aes(label = percent(round(perc)/100), vjust = -0.5)) +
  facet_wrap(id ~.)

# .................................

# minutes:

ATPSeasons %>% 
  filter(Upset15 == 1) %>%
  group_by(id )-> m1
summary(m1$minutes)

ATPSeasons %>% 
  filter(Upset15 == 0) %>%
  group_by(id )-> m0
summary(m0$minutes)

mperavg <- (mean(m1$minutes, na.rm = TRUE)-mean(m0$minutes, na.rm = TRUE))*200/(mean(m1$minutes, na.rm = TRUE)+mean(m0$minutes, na.rm = TRUE))
# 3.17%

# 2014 
ATPSeasons %>% 
  filter(id == "2014", Upset15 == 1) -> m1
summary(m1$minutes)

ATPSeasons %>% 
  filter(id == "2014", Upset15 == 0) -> m0
summary(m0$minutes)

mperavg <- (mean(m1$minutes, na.rm = TRUE)-mean(m0$minutes, na.rm = TRUE))*200/(mean(m1$minutes, na.rm = TRUE)+mean(m0$minutes, na.rm = TRUE))
# 0.17%
# .................................

# match statistics 2014:

# Aces

# #Number of aces as a predictor

# upsets:
U <- ATPSeasons %>%
  filter(id == "2014", Upset15 == 1)
ace_dif <- na.omit(U$w_ace) - na.omit(U$l_ace)

ace_dif <- ifelse(ace_dif > 0 , 1 , 0)

#what percentage of time did winner hit more aces than the loser
sum(ace_dif, na.rm = TRUE)*100/length(ace_dif)
# 55.33662

# statistics:
summary(U$w_ace)
summary(U$l_ace)

# Regular:
R <- ATPSeasons %>%
  filter(id == "2014", Upset15 == 0)
ace_dif <- na.omit(R$w_ace) - na.omit(R$l_ace)

ace_dif <- ifelse(ace_dif > 0 , 1 , 0)

#what percentage of time did winner hit more aces than the loser
sum(ace_dif, na.rm = TRUE)*100/length(ace_dif)
# 61.97%

# statistics:
summary(R$w_ace)
summary(R$l_ace)

#-------------------

# Double faults
# Number of double faults as a predictor
# difference between number of double faults hit by the loser and the winner

# Upsets:

df_dif <-  U$l_df -  U$w_df
#number of times loser hit more double faults in a match
df_dif <- ifelse(df_dif > 0 , 1 , 0)
#what percentage of time did loser hit more double faults than the winner
sum(df_dif, na.rm = TRUE)*100/length(df_dif)
# 53.69 %
summary(U$w_df)
summary(U$l_df)

# Regular:

df_dif <-  R$l_df -  R$w_df
#number of times loser hit more double faults in a match
df_dif <- ifelse(df_dif > 0 , 1 , 0)
#what percentage of time did loser hit more double faults than the winner
sum(df_dif, na.rm = TRUE)*100/length(df_dif)
# 51.12 %
summary(R$w_df)
summary(R$l_df)

# .................................

# #first serve percentage = number of first serves made / number of serve points

#upsets:

p1spw <- U$w_1stIn/U$w_svpt    # winner
p1spl <- U$l_1stIn/U$l_svpt    # loser

summary(p1spw)
summary(p1spl)

#First serve percentage as a predictor

#percentage of time winner had a better 1st serve percentage
p1sd <- ifelse((p1spw - p1spl) > 0 , 1 , 0)
sum(p1sd, na.rm = TRUE)*100/length(p1sd)
# 51.4 %

# Regular:

p1spw <- R$w_1stIn/R$w_svpt   # winner
p1spl <- R$l_1stIn/R$l_svpt    # loser

summary(p1spw)
summary(p1spl)

#First serve percentage as a predictor

#percentage of time winner had a better 1st serve percentage
p1sd <- ifelse((p1spw - p1spl) > 0 , 1 , 0)
sum(p1sd, na.rm = TRUE)*100/length(p1sd)
# 56.31 %

#First and second serve winning percentages

# #first serve winning percentage = number of first-serve points won / number of first serves made

# Upsets:

w1swp <- U$w_1stWon/ U$w_1stIn    
w1slp <- U$l_1stWon/ U$l_1stIn

#percentage of time the winning player had a higher winning percentage on 1st serve
f1swpd <- ifelse((w1swp - w1slp) > 0 , 1 , 0)
sum(f1swpd, na.rm = TRUE)*100/length(f1swpd)
# 77.18%

summary(w1swp)
summary(w1slp)

# Regular:

w1swp <- R$w_1stWon/R$w_1stIn    
w1slp <- R$l_1stWon/R$l_1stIn

#percentage of time the winning player had a higher winning percentage on 1st serve
f1swpd <- ifelse((w1swp - w1slp) > 0 , 1 , 0)
sum(f1swpd, na.rm = TRUE)*100/length(f1swpd)
# 85.23%

summary(w1swp)
summary(w1slp)

#second serve winning percentage 

# Upsets:

w2swp <- U$w_2ndWon/ (U$w_svpt- U$w_1stIn)     
w2slp <- U$l_2ndWon/ (U$l_svpt- U$l_1stIn)

# #percentage of time the winning player had a higher winning percentage on 2nd serve
s2swpd <- ifelse((w2swp - w2slp) > 0 , 1 , 0)
sum(s2swpd, na.rm = TRUE)*100/length(s2swpd)
# 75.53%

summary(w2swp)
summary(w2slp)


# Regular:

w2swp <- R$w_2ndWon/ (R$w_svpt - R$w_1stIn)     
w2slp <- R$l_2ndWon/ (R$l_svpt - R$l_1stIn)

# #percentage of time the winning player had a higher winning percentage on 2nd serve
s2swpd <- ifelse((w2swp - w2slp) > 0 , 1 , 0)
sum(s2swpd, na.rm = TRUE)*100/length(s2swpd)
# 76.63%

summary(w2swp)
summary(w2slp)


# .................................

# Break points faced

# Upset
bpf <- data.frame(winner = na.omit(U$w_bpFaced), loser = na.omit(U$l_bpFaced) )

bpfd <- ifelse((bpf$loser - bpf$winner) > 0 , 1 , 0)
sum(bpfd, na.rm = TRUE)*100/length(bpfd)
#  71.43%

summary(bpf)

# Regular:
bpf <- data.frame(winner = na.omit(R$w_bpFaced), loser = na.omit(R$l_bpFaced) )
summary(bpf)

bpfd <- ifelse((bpf$loser - bpf$winner) > 0 , 1 , 0)
sum(bpfd, na.rm = TRUE)*100/length(bpfd)
#78.72%

# .................................

# break points saved

# upset:

bps <- data.frame(winner = na.omit(U$w_bpSaved), loser = na.omit(U$l_bpSaved) )

bpsd <- ifelse((bps$winner - bps$loser) > 0 , 1 , 0)
sum(bpsd, na.rm = TRUE)*100/length(bpsd)
# 32.18%

summary(bps)

# Regular:
bps <- data.frame(winner = na.omit(R$w_bpSaved), loser = na.omit(R$l_bpSaved) )

bpsd <- ifelse((bps$winner - bps$loser) > 0 , 1 , 0)
sum(bpsd, na.rm = TRUE)*100/length(bpsd)
# 29.07%

summary(bps)
###############################################################################

# Upsets vs Reg for all years:

# upsets:
U <- ATPSeasons %>%
  filter(Upset15 == 1)

# Regular:
R <- ATPSeasons %>%
  filter(Upset15 == 0)

# another script

##############################################################################

# Adapted From: https://tennismash.com/2016/01/21/302/

# Compare the performance of the upset winner to their average performance on the same surface


upsets <- ATPSeasons %>% 
  filter(id == "2014", Upset15 == 1) # 609

regular <- ATPSeasons %>% 
  filter(id == "2014", Upset15 == 0) # 1964

## Find the statistics of the upsets winners in regular matches: df1 (as winners in R) & df2 (as losers in R)

## 1- upsets winners as winners of regular:

df1 <- upsets %>%
  select(winner_id) %>%
  distinct(winner_id) %>%
  inner_join(regular, by = c("winner_id" = "winner_id"))


## 2- upsets winners as losers of regular:
df2 <- upsets %>%
  select(winner_id) %>%
  distinct(winner_id) %>%
  inner_join(regular, by = c("winner_id" = "loser_id")) 


## Find the statistics of the upsets losers in regular matches: df3 (as winners in R) & df4 (as losers in R)
## 3- upsets losers as winners of regular:
df3 <- upsets %>%
  select(loser_id) %>%
  distinct(loser_id) %>%
  inner_join(regular, by = c("loser_id" = "winner_id")) 


## 4- upsets losers as losers of regular:
df4 <- upsets %>%
  select(loser_id) %>%
  distinct(loser_id) %>%
  inner_join(regular, by = c("loser_id" = "loser_id")) 

# ...................

## statistics of the upsets winners in regular matches (df1 & df2)
## Then compare these statistics in upset matches in the same surface: (upsets)


data_list <- list(df1, df2, df3, df4)                    

Tstat <- data.frame(matrix(ncol = 13, nrow = 8))
x <- c("Stat", "df1C", "df1G","df1H", "df2C", "df2G","df2H","df3C", "df3G","df3H","df4C", "df4G","df4H")
colnames(Tstat) <- x

# stats of interest:
Tstat$Stat <- c("Ace", "DF", "First Serve %", 
                "1st Srv Won %", "2nd Srv Won %", 
                "BP Saved %", "BP Faced", "Return %")

surface <- c("Clay", "Grass", "Hard")
n <- 1
dn <- 0
for (d in data_list){
  dn <- dn + 1
  if(dn == 1 | dn == 3){
    ifelse(n <= 4, n , 7)
    for (s in surface){
      df <- d %>% filter(surface == s)
      n <- n +1
      Tstat[1, n] <- mean(df$w_ace)                            ## Aces
      Tstat[2, n] <- mean(df$w_df)                             ## double faults
      Tstat[3, n] <- mean(df$w_1stIn/df$w_svpt, na.rm = TRUE)                ## First serve percentage
      Tstat[4, n] <- mean(df$w_1stWon/df$w_1stIn, na.rm = TRUE)              ## First serve winning percentages
      Tstat[5, n] <- mean(df$w_2ndWon/(df$w_svpt- df$w_1stIn), na.rm = TRUE) ## Second serve winning percentages
      Tstat[6, n] <- mean(ifelse(df$w_bpFaced == 0, 0, df$w_bpSaved/df$w_bpFaced))  ## break points saved (%)
      Tstat[7, n] <- mean(df$w_bpFaced)                                             ## break points faced (n)
      Tstat[8, n] <- mean((df$l_svpt-(df$l_1stWon+df$l_2ndWon))/df$l_svpt, na.rm = TRUE)   ## Return Games Won Percentage
    }
  } else {
    ifelse(n <= 7, n , 10)
    for (s in surface){
      df <- d %>% filter(surface == s)
      n <- n +1
      Tstat[1, n] <- mean(df$l_ace)                             ## Aces
      Tstat[2, n] <- mean(df$l_df)                              ## double faults
      Tstat[3, n] <- mean(df$l_1stIn/df$l_svpt)                 ## First serve percentage
      Tstat[4, n] <- mean(df$l_1stWon/df$l_1stIn)               ## First serve winning percentages
      Tstat[5, n] <- mean(df$l_2ndWon/(df$l_svpt- df$l_1stIn))  ## Second serve winning percentages
      Tstat[6, n] <- mean(ifelse(df$l_bpFaced == 0, 0, df$l_bpSaved/df$l_bpFaced))        ## break points saved (%)
      Tstat[7, n] <- mean(df$l_bpFaced)                                                   ## break points faced (n)
      Tstat[8, n] <- mean((df$w_svpt-(df$w_1stWon+df$w_2ndWon))/df$w_svpt, na.rm = TRUE)                ## Return Games Won Percentage
    }
  }
}

# mean in regular matches: upset winners df1 & df2 ; upset losers: df3 & df4

Tstat2 <- data.frame(matrix(ncol = 7, nrow = 8))
x <- c("Stat", "dUC", "dUG","dUH", "dRC", "dRG","dRH")   # U: upset winners ; R: upset losers 
colnames(Tstat2) <- x

# stats of interest:
Tstat2$Stat <- c("Ace", "DF", "First Serve %", 
                 "1st Srv Won %", "2nd Srv Won %", 
                 "BP Saved %", "BP Faced", "Return %")

n <- 1
h <- 1
for (c in 1:2){
  for (s in surface){
    ifelse(n == 4, 7, n)
    n <- n +1
    h <- h +1
    for (i in 1:8){
      Tstat2[i, h] <- (Tstat[i, n]+Tstat[i, n+3])/2
    }
  }
}

# ........
# mean of winners in upset matches based on surfaces:

Tstat_up <- data.frame(matrix(ncol = 4, nrow = 8))
x <- c("Stat", "C", "G","H")
colnames(Tstat_up) <- x

# stats of interest:
Tstat_up$Stat <- c("Ace", "DF", "First Serve %", 
                   "1st Srv Won %", "2nd Srv Won %", 
                   "BP Saved %", "BP Faced", "Return %")
n <- 1
for (s in surface){
  df <- upsets %>% filter(surface == s)
  n <- n +1
  Tstat_up[1, n] <- mean(df$w_ace)                   ## Aces
  Tstat_up[2, n] <- mean(df$w_df)                    ## double faults
  Tstat_up[3, n] <- mean(df$w_1stIn/df$w_svpt, na.rm = TRUE)     ## First serve percentage
  Tstat_up[4, n] <- mean(df$w_1stWon/df$w_1stIn, na.rm = TRUE)   ## First serve winning percentages
  Tstat_up[5, n] <- mean(df$w_2ndWon/(df$w_svpt- df$w_1stIn), na.rm = TRUE)  ## Second serve winning percentages
  Tstat_up[6, n] <- mean(ifelse(df$w_bpFaced == 0, 0, df$w_bpSaved/df$w_bpFaced))  ## break points saved (%)
  Tstat_up[7, n] <- mean(df$w_bpFaced)                                             ## break points faced (n)
  Tstat_up[8, n] <- mean((df$l_svpt-(df$l_1stWon+df$l_2ndWon))/df$l_svpt, na.rm = TRUE)          ## Return Games Won Percentage
}

# find the improvement % in upsets winners performance from regular to upset matches :
Tstat_up_per <- data.frame(matrix(ncol = 5, nrow = 8))
x <- c("Stat", "Clay", "Grass","Hard", "Avg%")
colnames(Tstat_up_per) <- x

# stats of interest:
Tstat_up_per$Stat <- c("Ace", "DF", "First Serve %", 
                       "1st Srv Won %", "2nd Srv Won %", 
                       "BP Saved %", "BP Faced", "Return %")

Tstat_up_per[,2:4] <- ( Tstat_up[,2:4] - Tstat2[, 2:4] )*100/Tstat2[,2:4]

Tstat_up_per[,5] <- rowMeans(Tstat_up_per[,2:4])

# ......................

# mean of losers in upset matches based on surfaces:

Tstat_Ra <- data.frame(matrix(ncol = 4, nrow = 8))
x <- c("Stat", "C", "G","H")
colnames(Tstat_Ra) <- x

# stats of interest:
Tstat_Ra$Stat <- c("Ace", "DF", "First Serve %", 
                   "1st Srv Won %", "2nd Srv Won %", 
                   "BP Saved %", "BP Faced", "Return %")
n <- 1
for (s in surface){
  df <- upsets %>% filter(surface == s)
  n <- n +1
  Tstat_Ra[1, n] <- mean(df$l_ace)                             ## Aces
  Tstat_Ra[2, n] <- mean(df$l_df)                              ## double faults
  Tstat_Ra[3, n] <- mean(df$l_1stIn/df$l_svpt,na.rm = TRUE)                 ## First serve percentage
  Tstat_Ra[4, n] <- mean(df$l_1stWon/df$l_1stIn, na.rm = TRUE)               ## First serve winning percentages
  Tstat_Ra[5, n] <- mean(df$l_2ndWon/(df$l_svpt- df$l_1stIn), na.rm = TRUE)  ## Second serve winning percentages
  Tstat_Ra[6, n] <- mean(ifelse(df$l_bpFaced == 0, 0, df$l_bpSaved/df$l_bpFaced))        ## break points saved (%)
  Tstat_Ra[7, n] <- mean(df$l_bpFaced)                                                   ## break points faced (n)
  Tstat_Ra[8, n] <- mean((df$w_svpt-(df$w_1stWon+df$w_2ndWon))/df$w_svpt, na.rm = TRUE)                ## Return Games Won Percentage
}

# find the improvement % in upsets winners performance from regular to upset matches :
Tstat_Ra_per <- data.frame(matrix(ncol = 5, nrow = 8))
x <- c("Stat", "Clay", "Grass","Hard", "Avg%")
colnames(Tstat_Ra_per) <- x

# stats of interest:
Tstat_Ra_per$Stat <- c("Ace", "DF", "First Serve %", 
                       "1st Srv Won %", "2nd Srv Won %", 
                       "BP Saved %", "BP Faced", "Return %")

Tstat_Ra_per[,2:4] <- ( Tstat_Ra[,2:4] - Tstat2[, 5:7] )*100/Tstat2[,5:7]

Tstat_Ra_per[,5] <- rowMeans(Tstat_Ra_per[,2:4])

# .........

# plots of percentage:

# make a joint:

Plot1 <- Tstat_up_per %>%
  full_join(Tstat_Ra_per, by = c("Stat"), suffix = c("_upset", "_regular")) 

Plot1 <- Plot1 %>% 
  rename(
    Avg_upset = "Avg%_upset",
    Avg_regular = "Avg%_regular"
  )

#Plot1 <- Plot1[-8, ]

library(grid)

# from https://stackoverflow.com/questions/18265941/two-horizontal-bar-charts-with-shared-axis-in-ggplot2-similar-to-population-pyr

g.mid<-ggplot(Plot1,aes(x=1,y=Stat))+geom_text(aes(label=Stat), size=7)+
  geom_segment(aes(x=0.94,xend=0.96,yend=Stat))+
  geom_segment(aes(x=1.04,xend=1.065,yend=Stat))+
  ggtitle("   ")+
  ylab(NULL)+
  scale_x_continuous(expand=c(0,0),limits=c(0.94,1.065))+
  theme(axis.title=element_blank(),
        panel.grid=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        panel.background=element_blank(),
        axis.text.x=element_text(color=NA),
        axis.ticks.x=element_line(color=NA),
        plot.margin = unit(c(1,-1,1,-1), "mm"))

g1 <- ggplot(data = Plot1, aes(x = Stat, y = Avg_upset)) +
  geom_bar(stat = "identity", fill = "#3CB371") + ggtitle("Upsets Winners' Average Performance Improvment") +
  geom_text(aes(label = percent(Avg_upset/100), vjust = 0.5, hjust =  0.4), size=7) +
  theme(axis.title.x = element_blank(), 
        axis.title.y = element_blank(), 
        axis.text.y = element_blank(), 
        axis.ticks.y = element_blank(), 
        plot.title = element_text(size = 20, face = "bold"),
        plot.margin = unit(c(1,-1,1,0), "mm")) +
  scale_y_reverse() + coord_flip()

g2 <- ggplot(data = Plot1, aes(x = Stat, y = Avg_regular)) +xlab(NULL)+
  geom_bar(stat = "identity", fill = "#DC143C") + ggtitle("Upsets Losers' Average Performance Drop") +
  geom_text(aes(label = percent(Avg_regular/100), vjust = 0.5, hjust = 0.4), size=7) +
  theme(axis.title.x = element_blank(), axis.title.y = element_blank(), 
        axis.text.y = element_blank(), axis.ticks.y = element_blank(),
        plot.title = element_text(size = 20, face = "bold"),
        plot.margin = unit(c(1,0,1,-1), "mm")) +
  coord_flip()

library(gridExtra)
gg1 <- ggplot_gtable(ggplot_build(g1))
gg2 <- ggplot_gtable(ggplot_build(g2))
gg.mid <- ggplot_gtable(ggplot_build(g.mid))

grid.arrange(gg1,gg.mid,gg2,ncol=3,widths=c(4/9,1/9,4/9))


################################################################################