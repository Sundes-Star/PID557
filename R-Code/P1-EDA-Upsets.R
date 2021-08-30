
# Packages:
library(tidyverse)
library(dplyr)
library(ggplot2)
library(assertive)
library(visdat)
library(gridExtra)
library(scales)

#################################################################################

# Read in the dataset:
atp19 <- read.csv("atp_matches_2019.csv", na.strings=c("","NA"))
names(atp19)

glimpse(atp19)
# 2781 obs. of  49 variables

summary(atp19)

atp19 %>% group_by(tourney_name) %>%
  count(tourney_name)
# 126 tournments

#################################################################################
# Manipulation:

# Cleaning:
# 1- missing ---> NA
# 2- Keep only completed matches, (No injury withdrawals & walkovers)
# RET ,  W/O    
atp19 %>% 
  filter(score %in% c("RET", "W/O")) 
# 21 obs

# Exclude Davis Cup: 
atp19 <- atp19 %>%
  filter(tourney_level != "D")

# Delete rows with incomplete matches
atp19<-atp19[!(atp19$score=="RET" | atp19$score=="W/O"),]
glimpse(atp19)
# 49 * 2,760

vis_miss(atp19)

atp19 %>%
  arrange(w_ace) %>%
  vis_miss()

# missing data: 9.2%

# drop rows that have NA statistics:
atp19<-atp19[!(is.na(atp19$w_ace)),]   # when ace is NA, mostly, the rest of the statistics are NA

vis_miss(atp19)
# 49 * 2,679


# Data Type:

# Convert ages to integer, add in new columns
atp19 <- atp19 %>% mutate(winner_age_int = as.integer(atp19$winner_age))
atp19 <- atp19 %>% mutate(loser_age_int = as.integer(atp19$loser_age))

################################################################################

#EDA 

# Visualization:

### Categorical:

# Surface

# percentage of each surface
atp19 %>% 
  count(surface) %>% 
  mutate(perc = n*100 / nrow(atp19)) -> atp19p

ggplot(atp19p, aes(x = surface, y = perc)) + 
  geom_bar(stat = "identity",fill='steelblue', color='black') +
  geom_text(aes(label = percent(round(perc)/100), vjust = -1))  + 
  xlab("Surface") + ylab("Percentage %")

# number of matches per level:
atp19 %>%
  arrange(desc(tourney_level))  %>%
  group_by(tourney_level) %>%
  summarise(avgl = n()) 

# percentage of each level
# reorder the level according to tier:
atp19 <- arrange(transform(atp19,
                           tourney_level=factor(tourney_level,levels=c("G", "M", "A","F"))),tourney_level)

atp19 %>% 
  count(tourney_level) %>% 
  mutate(perc = n*100 / sum(n)) -> atp19p

ggplot(atp19p, aes(x = tourney_level, y = perc)) + 
  geom_bar(stat = "identity", fill='steelblue', color='black') +
  geom_text(aes(label = percent((perc)/100), vjust = -1)) +
  xlab("Tourney Level") + ylab("Percentage %")


table(atp19$tourney_level)


# winner_hand
atp19 %>% 
  count(winner_hand) %>% 
  mutate(perc = n*100 / nrow(atp19)) -> atp19wh

ggplot(atp19wh, aes(x= winner_hand, y = perc)) + 
  geom_bar(stat = "identity",fill='steelblue', color='black') +
  geom_text(aes(label = percent(round(perc)/100), vjust = -1)) + 
  scale_x_discrete(drop=TRUE) + xlab("Hand") + ylab("Total matches") # mostly Right

# loser_hand
# make missing values as U
atp19$loser_hand[is.na(atp19$loser_hand)] <- "U"

ggplot(atp19, aes(x= loser_hand)) + 
  geom_bar( fill='steelblue', color='black')+ 
  scale_x_discrete(drop=TRUE) + xlab("Hand") + ylab("Total matches") # mostly Right

## age
ggplot(atp19, aes(x= winner_age_int)) + 
  geom_bar( fill='steelblue', color='black')+ 
  scale_x_discrete(drop=TRUE) + xlab("Age") + ylab("Count") # 

ggplot(atp19, aes(x= loser_age_int)) + 
  geom_bar(fill='steelblue', color='black')+ 
  scale_x_discrete(drop=TRUE) + xlab("Age") + ylab("Count") # 

## height
atp19hi$winner_ht <- as.factor(atp19$winner_ht)
atp19hi$loser_ht <- as.factor(atp19$loser_ht)

atp19hi<-atp19hi[!(is.na(atp19hi$winner_ht)),]

ggplot(atp19hi, aes(x= winner_ht)) + 
  geom_bar( fill='steelblue', color='black')+ 
  scale_x_discrete(drop=TRUE) + xlab("Height") + ylab("Count") # 

atp19hi<-atp19[!(is.na(atp19$loser_ht)),]

ggplot(atp19hi, aes(x= loser_ht)) + 
  geom_bar(fill='steelblue', color='black')+ 
  scale_x_discrete(drop=TRUE) + xlab("Height") + ylab("Count") # 

# best of
ggplot(atp19, aes(x = best_of)) +
  geom_bar(fill='steelblue', color='black') # mostly 3

### Numeric
ggplot(atp19, aes(x = w_bpFaced)) +
  geom_histogram()

#################################################################################

# EDA: 2
# How many match per tournament:

atp19 %>% 
  count(tourney_name) %>%
  arrange(desc(n))

# Top winners' countries:
atp19 %>% 
  count(winner_ioc) %>%
  arrange(desc(n))

#################################################################################

# Adapted from ATP Tour 2012: 
# https://medium.com/@tobikasali/exploratory-data-analysis-with-r-f0b0a5163ecd
# who won the most matches that year? 
# What are the factors that influence players winning matches? 

#1# who won the most matches in 2019?   
# Top 10 players:
# create a new df of winners
winners<- atp19 %>%  group_by(winner_name)%>%
  summarize(wins = n())%>%
  arrange(desc(wins)) 
winners[1:10,]

glimpse(winners)  # 197 winners
summary(winners)

# Look at players with wins > the average (13)
winners %>% filter(wins > 13 & wins < 40) %>%
  ggplot(aes(x = winner_name, y = wins)) +
  geom_col() + theme(axis.text.x = element_text(angle = 90, hjust = 1))

# Another visualization 
winners %>%
  arrange(wins) %>%    # First sort by val. This sort the data frame but NOT the factor levels
  mutate(name=factor(winner_name, levels=winner_name)) %>%   # This trick update the factor levels
  ggplot( aes(x=name, y=wins)) +
  geom_segment( aes(xend=name, yend=0)) +
  geom_point( size=4, color="steelblue") +
  coord_flip() +
  theme_bw() +
  xlab("")

winners %>%
  filter(wins > 13.14) %>%
  arrange(wins) %>%    # First sort by val. This sort the dataframe but NOT the factor levels
  mutate(name=factor(winner_name, levels=winner_name)) %>%   # This trick update the factor levels
  ggplot( aes(x=name, y=wins)) +
  geom_segment( aes(xend=name, yend=0)) +
  geom_point( size=4, color="steelblue") +
  coord_flip() +
  theme_bw() +
  theme(text = element_text(size=13)) +
  xlab("")

# ..............................................................................

# Win Ratio 
# who wins most of the matches they played.

# Win ratio for winners of > 11 wins:

# loses of winners:
atp19 %>% group_by(loser_name)%>%
  summarise(loses = n()) %>% arrange(loses) -> atp19L

# join 2 df:
winners %>% filter(wins > 13) %>%
  inner_join(atp19L, by = c("winner_name" = "loser_name")) -> win_r

win_r <- win_r %>% mutate(wr = wins*100/(wins + loses)) %>% arrange(desc(wr))
win_r
boxplot(win_r$wr,cex.axis=2)

#visualize:

win_r %>% ggplot(aes(x = winner_name, y = wr)) +
  geom_col() + theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  geom_text(aes(label = round(wr,2), vjust = -1),size = 3)

win_r %>%
  arrange(wr) %>%    # First sort by val. This sort the dataframe but NOT the factor levels
  mutate(name=factor(winner_name, levels=winner_name)) %>%   # This trick update the factor levels
  ggplot( aes(x=name, y=wr)) +
  geom_segment( aes(xend=name, yend=0)) +
  geom_point( size=4, color="steelblue") +
  coord_flip() +
  theme_bw() +
  xlab("")

summary(win_r)

# 75 % of the top players have > 59% win rate

# ..............................................................................

## the most tournament wins
# investigate rounds:

# reorder the round:
atp19 <- arrange(transform(atp19,
                           round=factor(round,levels=c("F", "SF", "QF", "R16", "R32", "R64", "R128", "PR"))),round)

ggplot(atp19, aes(x= round)) + 
  geom_bar( fill='steelblue', color='black')+ 
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  scale_x_discrete(drop=TRUE) + xlab("Round") + ylab("Total matches")
# most matches are round of 32 : R32

# Investigate Final rounds:
# the winner of the final round will be the tournament winner.

Twinners<- atp19 %>% filter(round == "F") %>%
  group_by(winner_name)%>%
  summarize(Twins = n())%>%
  arrange(desc(Twins))
Twinners[1:10,]

Twinners %>% ggplot(aes(x = winner_name, y = Twins)) +
  geom_col(fill='steelblue', color='black') + theme(text = element_text(size = 20), axis.text.x = element_text(size=15, angle = 90, hjust = 1)) +
  xlab("Winner Name") + ylab("Number of Wins")

# ..............................................................................

atp19winners<- atp19 %>% group_by(winner_name)%>%
  mutate(wins = n())

atp19winners<- atp19winners %>% 
  inner_join(atp19L, by = c("winner_name" = "loser_name"))  %>% 
  mutate(wr = wins*100/(wins + loses)) 

Twinners %>% 
  inner_join(atp19winners, by = "winner_name", suffix = c("_19", "_TW") ) -> atp19winners 

plot(Twins~wins, data=atp19winners)
plot(Twins~wr, data=atp19winners)

cor(atp19winners$Twins, atp19winners$wins)
cor(atp19winners$Twins, atp19winners$wr)

model <- lm(Twins ~ wins, data = atp19winners)
summary(model)

model <- lm(Twins ~ wr, data = atp19winners)
summary(model)

# unseeded tournaments winners and their ranks
atp19winners %>%
  filter(is.na(winner_seed)) %>%
  select(Twins, winner_rank, winner_seed) %>%
  arrange(desc(winner_rank)) -> test

atp19winners %>%
  filter(is.na(winner_seed), winner_rank <= 32) %>%
  select(tourney_name, winner_name, Twins, winner_rank, winner_seed) %>%
  arrange(desc(winner_rank)) -> test

# ..............................................................................

## Join tables: by Tournament winners:
atp19 %>% 
  filter(round == "F") %>%
  group_by(winner_name)%>%
  inner_join(Twinners, by = "winner_name", suffix = c("_19", "_TW") ) -> atp19T

## Tournament winners by Tournament Type

atp19T %>% group_by(winner_name)%>%
  ggplot(aes(x = winner_name, fill = surface)) +
  geom_bar() + theme(text = element_text(size = 20), axis.text.x = element_text(angle = 90, hjust = 1)) +
  facet_wrap(tourney_level~.) +
  xlab("Winner Name") + ylab("Number of Wins")

## Tournament winners by surface: Players win ratio per surface for the tournament winners
atp19T %>% group_by(winner_name)%>%
  ggplot(aes(x = winner_name)) +
  geom_bar() + theme(text = element_text(size = 20), axis.text.x = element_text(angle = 90, hjust = 1)) +
  facet_wrap(surface~.)

## Surface Percentage 
atp19T %>% group_by(winner_name) %>%
  count(surface) %>%
  mutate(per = n*100/sum(n)) -> t


## win_ratio for the number of matches the top winners played per surface
# top winners win > 2 tournaments: 

Twinners %>% 
  inner_join(atp19, by = "winner_name", suffix = c("_19", "_TW") ) %>%
  group_by(winner_name) %>%
  count(surface) -> TWW

Twinners %>% 
  inner_join(atp19, by = c("winner_name" = "loser_name")) %>%
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

# ..............................................................................

#Draw size
table(atp19$draw_size, atp19$tourney_level)

# Player seeds
atp19 %>% 
  count(winner_seed) %>%
  ggplot(aes(x = winner_seed, y = n)) + 
  geom_col(fill='steelblue', color='black') +
  theme(text = element_text(size = 20), axis.text.x = element_text(hjust = 0.5)) +
  scale_x_continuous("Winner Seed", labels = as.character(atp19$winner_seed), breaks = atp19$winner_seed) +
  ylab("Count")

# * higher seeded players win more matches than lower ranked seed in Grand Slams and Masters

## with NA as unseeded: 
winerseedt <- atp19$winner_seed

winerseedt[is.na(winerseedt)]<-"Unseeded"
table(winerseedt)

barplot(table(winerseedt))

# Are seeds more likely to win tournaments?

# reorder the seeds:
atp19T <- arrange(transform(atp19T,
                            winner_seed=factor(winner_seed,levels=c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12", "13", "Unseeded"))),winner_seed)
levels(atp19$winner_seed)

atp19T$winner_seed[is.na(atp19T$winner_seed)]<-"Unseeded"
table(atp19T$winner_seed, atp19T$tourney_level)
length(atp19T$winner_name)

atp19T %>%
  arrange(winner_seed) %>%
  group_by(winner_seed) %>%
  count() 

ggplot(atp19T, aes(x=winner_seed, fill = surface)) + 
  geom_bar() +
  theme(text = element_text(size = 20), axis.text.x = element_text(hjust = 0.5)) +
  facet_wrap(tourney_level~.) +
  xlab("Winner Seed") +  ylab("Count")

# ..............................................................................

# Player Ranks
atp19 %>% 
  count(winner_rank) %>%
  ggplot(aes(x = winner_rank, y = n)) + 
  geom_col(fill='steelblue', color='black') +
  theme(text = element_text(size = 20), axis.text.x = element_text(hjust = 0.5)) +
  scale_x_continuous("Winner Seed", labels = as.character(atp19$winner_rank), breaks = atp19$winner_rank) +
  ylab("Count")

table(atp19$winner_rank)
# 1 - 503
summary(atp19$winner_rank)
sum(is.na(atp19$winner_rank))

atp19<-atp19[!(is.na(atp19$winner_rank)),]

g1 <- atp19 %>% 
  ggplot(aes(x = winner_rank)) + 
  geom_density(aes( y=..scaled..), fill='steelblue', color='black') +
  theme(text = element_text(size = 20), axis.text.x = element_text(hjust = 0.5)) +
  xlim(1,503) + xlab("Winner Rank") + ylab("Scale") +
  ggtitle(" (a) The Density Estimate of Winners Ranks") +
  theme(plot.title = element_text(size = 20, face = "bold"))

g2 <- atp19 %>% 
  filter(winner_rank <=76)%>%
  count(winner_rank) %>%
  ggplot(aes(x = winner_rank, y = n)) + 
  geom_col(fill='steelblue', color='black') +
  theme(text = element_text(size = 20), axis.text.x = element_text(hjust = 0.5)) +
  xlim(1,76) + xlab("Winner Rank") + ylab("Count") + 
  ggtitle(" (b) The 75th percentile of Winners Ranks") +
  theme(plot.title = element_text(size = 20, face = "bold"))

# The distribution is sort of uniform especially form rank 10 to 76

g3 <- ggplot(atp19T %>% filter(winner_rank <=54), aes(x=winner_rank, fill = surface)) + 
  geom_bar() +
  theme(text = element_text(size = 20), axis.text.x = element_text(hjust = 0.5)) +
  facet_wrap(tourney_level~.) +
  xlab("Winner Rank") +  ylab("Count") +
  ggtitle(" (c) Ranks Distribution of Tournament Winners by Level and Surface ") +
  theme(plot.title = element_text(size = 20, face = "bold"))

summary(atp19T$winner_rank)

grid.arrange(arrangeGrob(g1), 
             arrangeGrob(g2,g3, ncol=1), 
             ncol=2, widths=c(1,1))

################################################################################

# Further EDA:

# Age difference distribution:
require(gridExtra)

atp19$agedif <- atp19$winner_age - atp19$loser_age

# Adapted from
# https://www.kaggle.com/ambarish/omnibus-womens-and-mens-tennis-matches-analysis 

plot1 <- atp19 %>%
  ggplot(aes(x = agedif)) +
  geom_density(fill = "steelblue") +
  labs(x= 'Age Difference',y = '', title = paste("Distribution of Age Difference")) +
  theme_bw() +
  theme(text = element_text(size = 20)) +
  annotate("text", y= 0.06, x =-14, label="sigma = 6.7", color = 'black', size = 10,hjust=0.5) 

sd(atp19$agedif, na.rm = TRUE)
# 6.7

# mean age
mean(atp19$winner_age, na.rm = TRUE)  #  27.77
mean(atp19$loser_age, na.rm = TRUE)   #  27.64

# ..............................................................................

# Hight difference distribution:
atp19$htdif <- atp19$winner_ht - atp19$loser_ht

# Adapted from
# https://www.kaggle.com/ambarish/omnibus-womens-and-mens-tennis-matches-analysis 

plot2 <- atp19 %>%
  ggplot(aes(x = htdif)) +
  geom_density(fill = "steelblue") +
  labs(x= 'Height Difference',y = '', title = paste("Distribution of Height Difference")) +
  theme_bw() +
  theme(text = element_text(size = 20)) +
  annotate("text", y= 0.044, x =-27, label="sigma = 11.2", color = 'black', size = 10,hjust=0.5) 


sd(atp19$htdif, na.rm = TRUE)
# 11.2

# mean height 

mean(atp19$winner_ht, na.rm = TRUE)  #  186.18
mean(atp19$loser_ht, na.rm = TRUE)   #  185.7

# ..................

grid.arrange(plot1, plot2, ncol=2)
################################################################################

## Adapted From -> Predicting ATP Tennis Match Outcomes Using Serving Statistics
# https://medium.com/swlh/predicting-atp-tennis-match-outcomes-using-serving-statistics-cde03d99f410

# Ace as a predictor
ace_dif <- na.omit(atp19$w_ace) - na.omit(atp19$l_ace)

ace_dif <- ifelse(ace_dif > 0 , 1 , 0)

# percentage of time winner hit more Ace than the loser
sum(ace_dif, na.rm = TRUE)*100/length(ace_dif)

# 57.19

# statistics:
summary(atp19$w_ace)
summary(atp19$l_ace)

#  Double faults as a predictor

df_dif <-  atp19$l_df -  atp19$w_df

#number of times loser hit more double faults in a match
df_dif <- ifelse(df_dif > 0 , 1 , 0)

#what percentage of time did loser hit more double faults than the winner
sum(df_dif, na.rm = TRUE)*100/length(df_dif)
# 50.54

summary(atp19$w_df)
summary(atp19$l_df)


# ..............................................................................

# first serve percentage = number of first serves made / number of serve points

p1spw <- atp19$w_1stIn/ atp19$w_svpt    # winner
p1spl <- atp19$l_1stIn/atp19$l_svpt    # loser

summary(p1spw)
summary(p1spl)

#First serve percentage as a predictor

#percentage of time winner had a better 1st serve percentage
p1sd <- ifelse((p1spw - p1spl) > 0 , 1 , 0)
sum(p1sd, na.rm = TRUE)*100/length(p1sd)
# 56.91

# ...........
# plots: 
# compare first serve percentage of loser and winner:

fsp <- data.frame(winner = p1spw, loser = p1spl )
fsp %>%
  pivot_longer(everything(), names_to = "Type", values_to = "FSP") %>%
  ggplot(aes(x=FSP, fill = Type))+
  geom_histogram()+
  labs(x = "First Server Percentage", y = "Counts")

fsp %>%
  pivot_longer(everything(), names_to = "Type", values_to = "FSP") %>%
  ggplot(aes(x = Type, y=FSP, color = Type))+
  geom_boxplot()+
  labs(x = "Player", y = "First Server Percentage")

summary(fsp)

# ..............................................................................

#Second serve percentage as a predictor

#percentage of time winner had a better 2nd serve percentage
#  number of first serves made  = (number of Serve Points -  number of first serves made / number of Serve Points )

p2spw <- 1 - p1spw
p2spl <- 1 - p1spl

p1sd <- ifelse((p1spw - p1spl) > 0 , 1 , 0)
sum(p1sd, na.rm = TRUE)*100/length(p1sd)
# 56.92

summary(p2spw)
summary(p2spl)

# ..............................................................................

#First and second serve winning percentages

# #first serve winning percentage = number of first-serve points won / number of first serves made

w1swp <- atp19$w_1stWon/ atp19$w_1stIn    
w1slp <- atp19$l_1stWon/ atp19$l_1stIn

#percentage of time the winning player had a higher winning percentage on 1st serve
f1swpd <- ifelse((w1swp - w1slp) > 0 , 1 , 0)
sum(f1swpd, na.rm = TRUE)*100/length(f1swpd)
# 83.11 %

summary(w1swp)
summary(w1slp)

#second serve winning percentage = number of second-serve points won / (number of serve points - number of first serves made)

w2swp <- atp19$w_2ndWon/ (atp19$w_svpt- atp19$w_1stIn)     
w2slp <- atp19$l_2ndWon/ (atp19$l_svpt- atp19$l_1stIn)

# #percentage of time the winning player had a higher winning percentage on 2nd serve
s2swpd <- ifelse((w2swp - w2slp) > 0 , 1 , 0)
sum(s2swpd, na.rm = TRUE)*100/length(s2swpd)
# 76.74 %

summary(w2swp)
summary(w2slp)

#-------
# plots: 
# compare first serve winning percentage of loser and winner:

fswp <- data.frame(winner = w1swp, loser = w1slp )
fswp %>%
  pivot_longer(everything(), names_to = "Type", values_to = "FSWP") %>%
  ggplot(aes(x=FSWP, fill = Type))+
  geom_histogram(alpha=0.5)+
  labs(x = "First Server Winning Percentage", y = "Counts")

fswp %>%
  pivot_longer(everything(), names_to = "Type", values_to = "FSWP") %>%
  ggplot(aes(x = Type, y=FSWP, color = Type))+
  geom_boxplot()+
  labs(x = "Player", y = "First Server Winning Percentage")

summary(fswp)

# ..............................................................................

# plots: 
# compare second serve winning percentage of loser and winner:

sswp <- data.frame(winner = w2swp, loser = w2slp )
sswp %>%
  pivot_longer(everything(), names_to = "Type", values_to = "SSWP") %>%
  ggplot(aes(x=SSWP, fill = Type))+
  geom_histogram(alpha=0.5)+
  labs(x = "Second Server Winning Percentage", y = "Counts")

sswp %>%
  pivot_longer(everything(), names_to = "Type", values_to = "SSWP") %>%
  ggplot(aes(x = Type, y=SSWP, color = Type))+
  geom_boxplot()+
  labs(x = "Player", y = "Second Server Winning Percentage")

summary(sswp)

################################################################################

# Adapted from https://medium.com/@tobikasali/exploratory-data-analysis-with-r-f0b0a5163ecd

# Break Points
# Break points faced

bpfd <- ifelse((atp19$w_bpFaced - atp19$l_bpFaced) > 0 , 1 , 0)
sum(bpfd, na.rm = TRUE)*100/length(bpfd)
# 17.04%
summary(atp19$w_bpFaced)
summary(atp19$l_bpFaced)


bpf <- data.frame(winner = na.omit(atp19$w_bpFaced), loser = na.omit(atp19$l_bpFaced) )

bpf %>%
  pivot_longer(everything(), names_to = "Type", values_to = "BPF") %>%
  ggplot(aes(x=BPF, fill = Type))+
  geom_histogram(alpha=0.5)+
  labs(x = "Second Server Winning Percentage", y = "Counts")

bpf %>%
  pivot_longer(everything(), names_to = "Type", values_to = "BPF") %>%
  ggplot(aes(x = Type, y=BPF, color = Type))+
  geom_boxplot()+
  labs(x = "Type", y = "Second Server Percentage")

#.................

# Percentage break points saved
w_bpSavedp <- atp19$w_bpSaved/atp19$w_bpFaced
l_bpSavedp <- atp19$l_bpSaved/atp19$l_bpFaced

bpsd <- ifelse((w_bpSavedp - l_bpSavedp) > 0 , 1 , 0)
sum(bpsd, na.rm = TRUE)*100/length(bpsd)
# 60.32

summary(w_bpSavedp)
summary(l_bpSavedp)

bps <- data.frame(winner = na.omit(atp19$w_bpSaved), loser = na.omit(atp19$l_bpSaved) )

bps %>%
  pivot_longer(everything(), names_to = "Type", values_to = "BPS") %>%
  ggplot(aes(x=BPS, fill = Type))+
  geom_histogram(alpha=0.5)+
  labs(x = "Second Server Winning Percentage", y = "Counts")

bps %>%
  pivot_longer(everything(), names_to = "Type", values_to = "BPS") %>%
  ggplot(aes(x = Type, y=BPS, color = Type))+
  geom_boxplot()+
  labs(x = "Type", y = "Second Server Percentage")

#----------------------------
# Return Games Won Percentage

w_Retpwp = (atp19$l_svpt-(atp19$l_1stWon+atp19$l_2ndWon))/atp19$l_svpt # w Return Games Won Percentage 
l_Retpwp = (atp19$w_svpt-(atp19$w_1stWon+atp19$w_2ndWon))/atp19$w_svpt # l Return Games Won Percentage 

Retd <- ifelse((w_Retpwp - l_Retpwp) > 0 , 1 , 0)
sum(Retd, na.rm = TRUE)*100/length(Retd)
# 91.42

summary(w_Retpwp)
summary(l_Retpwp)

###############################################################################

# Adapted From 
# https://towardsdatascience.com/understanding-the-importance-of-first-serve-in-tennis-with-data-analysis-4829ab088d36
# Understanding the Importance of First Serve in Tennis with Data Analysis

# Top 10 players:
# create a new df of winners

# wins ratio:
atp19 <- atp19 %>% 
  group_by(winner_name)%>%
  mutate(winers_wins = n())

# Win ratio:
# loses of winners:
Lossdf <- atp19 %>% group_by(loser_name)%>%
  summarise(winers_loses = n())

# join 2 df:
atp19 <- atp19 %>% 
  inner_join(Lossdf, by = c("winner_name" = "loser_name")) %>% 
  mutate(winner_wr = winers_wins*100/(winers_wins + winers_loses))

#..............................

atp19p <- atp19 %>% 
  group_by(winner_name) %>%
  filter(winers_wins > 13) %>%
  select(winner_name, w_1stIn, w_svpt, w_1stWon, w_2ndWon, w_bpSaved, w_bpFaced,l_svpt,l_1stWon,l_2ndWon, winner_wr)

atp19p <- atp19p %>% 
  group_by(winner_name) %>%
  mutate(w1sp  = mean(w_1stIn*100/w_svpt),                                     # w 1st serve %
         w1swp = mean(w_1stWon*100/w_1stIn),                                   # w 1st serve wining %
         w2sp  = mean(1 - w1sp),                                               # w 2nd serve %   
         w2swp = mean(w_2ndWon*100/(w_svpt-w_1stIn)),                          # w 2nd serve wining %
         wbpSp = mean(ifelse(w_bpFaced == 0, 0, w_bpSaved*100/w_bpFaced)),     # Break points saved (%) 
         wbpF  = mean(w_bpFaced),                                              # Break points Faced 
         w_Retpwp = mean((l_svpt-(l_1stWon+l_2ndWon))*100/l_svpt),             # w Return Games Won Percentage 
         
  )   

atp19p <- atp19p %>%
  group_by(winner_name) %>% 
  select(winner_name, w1sp, w1swp, w2sp, w2swp, wbpSp, wbpF, w_Retpwp, winner_wr)  

atp19p <- atp19p[!duplicated(atp19p$winner_name),]  

stat_cor <- cor(atp19p$winner_wr, atp19p[-1])
stat_cor

cor(atp19p[,-1])

# ------------------------------------------------------------------------------
# Investigate the Correlation Matrix
# http://www.sthda.com/english/wiki/visualize-correlation-matrix-using-correlogram

library(corrplot)
M <- cor(atp19p[,-1])
col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))
corrplot(M, method="color", col=col(200),  
         type="upper", order="hclust", 
         addCoef.col = "black",     # Add coefficient of correlation
         tl.col="black", tl.srt=45, #Text label color and rotation
         insig = "blank", 
         diag=FALSE,        # hide correlation coefficient on the principal diagonal
         tl.cex = 2,
         number.cex=2
)

# Computing the p-value of correlations

cor.test(atp19p$winner_wr,atp19p$w1sp)     # 0.1783
cor.test(atp19p$winner_wr,atp19p$w1swp)    # 0.007774
cor.test(atp19p$winner_wr,atp19p$w2sp)     # 0.1783
cor.test(atp19p$winner_wr,atp19p$w2swp)    # 0.004757 
cor.test(atp19p$winner_wr,atp19p$wbpSp)    # 0.07681
cor.test(atp19p$winner_wr,atp19p$wbpF)     # 0.002519
cor.test(atp19p$winner_wr,atp19p$w_Retpwp) # 2.919e-08

cor.test(atp19p$w1swp,atp19p$w_Retpwp) # 2.919e-08
cor.test(atp19p$w1swp,atp19p$wbpF) #  4.712e-14


# Plots
#1#
ggplot(atp19p, aes(x = w1sp,
                   y = winner_wr, label=winner_name)) +
  geom_point() +
  geom_smooth(method = "lm",
              fullrange = TRUE) +
  geom_text(aes(label=ifelse(winner_wr>80 | w1sp>67,as.character(winner_name),'')),hjust=0,vjust=0)

#2#
ggplot(atp19p, aes(x = w1swp,
                   y = winner_wr, label=winner_name)) +
  geom_point() +
  geom_smooth(method = "lm",
              fullrange = TRUE) +
  geom_text(aes(label=ifelse(winner_wr>70 | w1swp>86 |(w1swp<83.5 & w1swp>82 & winner_wr >60) ,as.character(winner_name),'')), angle = 25, size = 7, hjust=0,vjust=0)+
  theme(text = element_text(size = 20), axis.text.x = element_text(hjust = 0.5)) +
  xlab("First Serve Winning Percentage") + ylab("Winning Ratio") + xlim(68, 90)+ ylim(40, 91)+ 
  annotate("text", y= max(atp19p$winner_wr), x =max(atp19p$w1swp),label="Pearson's r = 0.31, P-value < 0.05", color = 'blue', size = 10,hjust=0.5) 

#3#
ggplot(atp19p, aes(x = w2sp,
                   y = winner_wr, label=winner_name)) +
  geom_point() +
  geom_smooth(method = "lm",
              fullrange = TRUE) +
  geom_text(aes(label=ifelse(winner_wr>80,as.character(winner_name),'')),hjust=0,vjust=0)

#4#
ggplot(atp19p, aes(x = w2swp,
                   y = winner_wr, label=winner_name)) +
  geom_point() +
  geom_smooth(method = "lm",
              fullrange = TRUE) +
  geom_text(aes(label=ifelse(winner_wr>80,as.character(winner_name),'')),hjust=0,vjust=0)

#5#
ggplot(atp19p, aes(x = wbpSp,
                   y = winner_wr, label=winner_name)) +
  geom_point() +
  geom_smooth(method = "lm",
              fullrange = TRUE) +
  geom_text(aes(label=ifelse(winner_wr>80,as.character(winner_name),'')),hjust=0,vjust=0)

#6#
ggplot(atp19p, aes(x = wbpFp,
                   y = winner_wr, label=winner_name)) +
  geom_point() +
  geom_smooth(method = "lm",
              fullrange = TRUE) +
  geom_text(aes(label=ifelse(winner_wr>80 | wbpFp>64 ,as.character(winner_name),'')),hjust=0,vjust=0)

# ..............................................................................

## Probability of winning a point when serving: p1*q1 + (1-p1) * p2*q2
# w1sp  : p1
# w1swp : q1
# w2sp  : p2
# w2swp : q2

atp19p %>%
  mutate(winps_pro = (w1sp * w1swp/100) + (1 - w1sp/100 ) * (w2sp * w2swp/100) ) -> atp19p

cor(atp19p$winps_pro, atp19p$winner_wr)
# r = 0.2854948 : weak relationship 
ggplot(atp19p, aes(winps_pro, winner_wr)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE)

# ..............................................................................


#  first serves percentage:

cor(atp19p$w1sp, atp19p$winner_wr)
# r =0.1679202 : weak or no relationship 
ggplot(atp19p, aes(w1sp, winner_wr)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE)

# first serve winning percentage

cor(atp19p$w1swp, atp19p$winner_wr)
# r =0.1752419 : weak relationship 
ggplot(atp19p, aes(w1swp, winner_wr)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE)

# correlation between the percentage of successful first serves and 
# the percentage of points won on successful first serves 

cor(atp19p$w1swp, atp19p$w1sp)
# r = -0.2595253 : weak relationship 
ggplot(atp19p, aes(w1swp, w1sp)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE)

## The perfect equilibrium

atp19p %>%
  mutate(win_pro_s1 = w1sp * w1swp/100,
         win_pro_s2 = w2sp * w2swp/100) -> atp19p


cor(atp19p$win_pro_s1, atp19p$winner_wr)
# r = 0.2806555 : weak relationship 
ggplot(atp19p, aes(win_pro_s1, winner_wr)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE)

# The second serve
atp19p %>%
  pivot_longer(c(win_pro_s1, win_pro_s2), names_to = "Type", values_to = "winp") %>%
  ggplot(aes(x = Type , y= winp)) + 
  geom_violin() 


## Probability of winning a point when serving: p1*q1 + (1-p1) * p2*q2
# w1sp  : p1
# w1swp : q1
# w2sp  : p2
# w2swp : q2

atp19p %>%
  mutate(winps_pro = (w1sp * w1swp/100) + (1 - w1sp/100 ) * (w2sp * w2swp/100) ) -> atp19p

cor(atp19p$winps_pro, atp19p$winner_wr)
# r = 0.2854948 : weak relationship 
ggplot(atp19p, aes(winps_pro, winner_wr)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE)

################################################################################
################################################################################
################################################################################

## Upset matches: 

# Define: 
# 1- rank difference > 15
# 2- Seeds categories: 8, 16, 32, 64, 128, > 128

## 1- rank difference > 15 :

## How many times occur? 
# Adapted From
# https://www.kaggle.com/wpncrh/upsets : Exploring Tennis Upsets


atp19$Upsetrd <- ifelse(atp19$winner_rank - atp19$loser_rank > 15,  1, 0)
sum(atp19$Upsetrd, na.rm = TRUE)
# 688

# Investigate upset matches patterns

# remove NA:

atp19<-atp19[!(is.na(atp19$Upsetrd)),]

# Surface

# percentage of each surface
# upset:
atp19up <- atp19 %>% 
  filter(Upsetrd == 1) %>%
  count(surface) %>% 
  mutate(perc = n*100 / sum(n)) 

ggplot(atp19up, aes(x = surface, y = perc)) + 
  geom_bar(stat = "identity",fill='steelblue') +    #, color='black'
  geom_text(aes(label = percent((perc)/100), vjust = -1))

# regular:
atp19Rp <- atp19 %>% 
  filter(Upsetrd == 0) %>%
  count(surface) %>% 
  mutate(perc = n*100 / sum(n)) 

ggplot(atp19Rp, aes(x = surface, y = perc)) + 
  geom_bar(stat = "identity",fill='steelblue', color='black') +
  geom_text(aes(label = percent(round(perc)/100), vjust = -1))

# ..............................................................................

# number of matches per level:

# upset:

atp19 %>%
  filter(Upsetrd == 1) %>%
  arrange(desc(tourney_level))  %>%
  group_by(tourney_level) %>%
  summarise(avgl = n()) 

# percentage of each level
atp19 %>% 
  filter(Upsetrd == 1) %>%
  count(tourney_level) %>% 
  mutate(perc = n*100 / sum(n)) %>%
  ggplot(aes(x = tourney_level, y = perc)) + 
  geom_bar(stat = "identity", fill='steelblue', color='black') +
  geom_text(aes(label = percent(round(perc)/100), vjust = -1))

### not in : F : Tour finals

# regular:
atp19 %>% 
  filter(Upsetrd == 0) %>%
  count(tourney_level) %>% 
  mutate(perc = n*100 / nrow(atp19)) %>%
  ggplot(aes(x = tourney_level, y = perc)) + 
  geom_bar(stat = "identity", fill='steelblue', color='black') +
  geom_text(aes(label = percent(round(perc)/100), vjust = -1))


# ..............................................................................

# winner_hand
# upset:
atp19 %>% 
  filter(Upsetrd == 1) %>%
  count(winner_hand) %>% 
  mutate(perc = n*100 / sum(n)) -> atp19wh

ggplot(atp19wh, aes(x= winner_hand, y = perc)) + 
  geom_bar(stat = "identity",fill='steelblue', color='black') +
  geom_text(aes(label = percent(round(perc)/100), vjust = -1)) + 
  scale_x_discrete(drop=TRUE) + xlab("Hand") + ylab("Total matches") 

# regular:
atp19 %>% 
  filter(Upsetrd == 1) %>%
  count(loser_hand) %>% 
  mutate(perc = n*100 / sum(n)) -> atp19wh

ggplot(atp19wh, aes(x= loser_hand, y = perc)) + 
  geom_bar(stat = "identity",fill='steelblue', color='black') +
  geom_text(aes(label = percent(round(perc)/100), vjust = -1)) + 
  scale_x_discrete(drop=TRUE) + xlab("Hand") + ylab("Total matches") 
# ..............................................................................

## age
# upset:
atp19 %>% 
  filter(Upsetrd == 1) %>%
  ggplot(aes(x= winner_age)) + 
  geom_bar( fill='steelblue', color='black')+ 
  xlab("Age") + ylab("Count") # 

atp19 %>% 
  filter(Upsetrd == 1) %>%
  summarise(meanau = mean(winner_age))
# 27.0

atp19 %>% 
  filter(Upsetrd == 1) %>%
  ggplot(aes(x= loser_age)) + 
  geom_bar(fill='steelblue', color='black')+ 
  xlab("Age") + ylab("Count") # 

atp19 %>% 
  filter(Upsetrd == 1) %>%
  summarise(meanar = mean(loser_age))
# 27.0

# regular:

atp19 %>% 
  filter(Upsetrd == 0) %>%
  ggplot(aes(x= winner_age_int)) + 
  geom_bar(fill='steelblue', color='black')+ 
  xlab("Age") + ylab("Count") # 

atp19 %>% 
  filter(Upsetrd == 0) %>%
  summarise(meanar = mean(winner_age_int))
# 27.3

atp19 %>% 
  filter(Upsetrd == 0) %>%
  ggplot(aes(x= loser_age_int)) + 
  geom_bar(fill='steelblue', color='black')+ 
  xlab("Age") + ylab("Count") # 

atp19 %>% 
  filter(Upsetrd == 0) %>%
  summarise(meanar = mean(loser_age_int))

# 27.15%

# ..............................................................................

# Height:

# upset:

atp19hi$winner_ht <- as.factor(atp19$winner_ht)
atp19hi$loser_ht <- as.factor(atp19$loser_ht)

atp19hi<-atp19hi[!(is.na(atp19hi$winner_ht)),]

atp19hi %>% 
  filter(Upsetrd == 1) %>%
  ggplot(aes(x= winner_ht)) + 
  geom_bar( fill='steelblue', color='black')+ 
  scale_x_discrete(drop=TRUE) + xlab("Height") + ylab("Count") # 

atp19 %>% 
  filter(Upsetrd == 1) %>%
  summarise(meanah = mean(winner_ht, na.rm = TRUE))

atp19 %>% 
  filter(Upsetrd == 1) %>%
  summarise(meanah = mean(loser_ht, na.rm = TRUE))



atp19hi<-atp19[!(is.na(atp19$loser_ht)),]
atp19hi %>% 
  filter(Upsetrd == 1) %>%
  ggplot(aes(x= loser_ht)) + 
  geom_bar(fill='steelblue', color='black')+ 
  scale_x_discrete(drop=TRUE) + xlab("Height") + ylab("Count") # 


# regular:

atp19$winner_ht <- as.factor(atp19$winner_ht)
atp19$loser_ht <- as.factor(atp19$loser_ht)

atp19hi<-atp19[!(is.na(atp19$winner_ht)),]

atp19hi %>% 
  filter(Upsetrd == 0) %>%
  ggplot(aes(x= winner_ht)) + 
  geom_bar( fill='steelblue', color='black')+ 
  scale_x_discrete(drop=TRUE) + xlab("Height") + ylab("Count") # 


atp19hi<-atp19[!(is.na(atp19$loser_ht)),]
atp19hi %>% 
  filter(Upsetrd == 0) %>%
  ggplot(aes(x= loser_ht)) + 
  geom_bar(fill='steelblue', color='black')+ 
  scale_x_discrete(drop=TRUE) + xlab("Height") + ylab("Count") # 

atp19 %>% 
  filter(Upsetrd == 0) %>%
  summarise(meanah = mean(winner_ht, na.rm = TRUE))

atp19 %>% 
  filter(Upsetrd == 0) %>%
  summarise(meanah = mean(loser_ht, na.rm = TRUE))

# ..............................................................................
# best of

# upset:

atp19 %>% 
  filter(Upsetrd == 1) %>%
  ggplot(aes(x = best_of)) +
  geom_bar(fill='steelblue', color='black') # mostly 3

# regular:
atp19 %>% 
  filter(Upsetrd == 0) %>%
  ggplot(aes(x = best_of)) +
  geom_bar(fill='steelblue', color='black') # mostly 3


# ..............................................................................

# Countries:  ## !!!!! more investigation !!!!!!
# upset:
atp19 %>% 
  filter(Upsetrd == 1) %>%
  select(tourney_name, surface, winner_name, winner_ioc, loser_name, loser_ioc)

atp19 %>% 
  count(winner_ioc) %>%
  arrange(desc(n))

# regular:
atp19 %>% 
  filter(Upsetrd == 0) %>%
  select(tourney_name, surface, winner_name, winner_ioc, loser_name, loser_ioc)

atp19 %>% 
  count(winner_ioc) %>%
  arrange(desc(n))

# ..............................................................................

#1# 
# Top 10 players:

# upset:
Upset_winners<- atp19 %>%  
  filter(Upsetrd == 1) %>%
  group_by(winner_name) %>%
  summarize(wins = n()) %>%   #mutate
  arrange(desc(wins))
Upset_winners[1:10,]

# regular:
Rwinners<- atp19 %>%  
  filter(Upsetrd == 0) %>%
  group_by(winner_name) %>%
  summarize(wins = n()) %>%
  arrange(desc(wins))
Rwinners[1:10,]

# ..............................................................................

# Tournament Winners:
# upset:
atp19 %>% filter(round == "F") %>%
  filter(Upsetrd == 1) %>%
  group_by(winner_name)%>%
  mutate(Twins = n())%>%
  arrange(desc(Twins)) -> upset_Tour

# regular:
atp19 %>% filter(round == "F") %>%
  filter(Upsetrd == 0) %>%
  group_by(winner_name)%>%
  mutate(Twins = n())%>%
  arrange(desc(Twins)) -> Reg_Tour

# ..............................................................................

# minutes:

atp19 %>% 
  filter(Upsetrd == 1) -> m1
summary(m1$minutes)

atp19 %>% 
  filter(Upsetrd == 0) -> m0
summary(m0$minutes)

mperavg <- (mean(m1$minutes, na.rm = TRUE)-mean(m0$minutes))*200/(mean(m1$minutes, na.rm = TRUE)+mean(m0$minutes))

# 5.42% 

# ..............................................................................
# Winners seeds
# upset:

atp19 %>% 
  filter(Upsetrd == 1) %>%
  count(winner_seed) %>%
  ggplot(aes(x = winner_seed, y = n)) + 
  geom_col(fill='steelblue', color='black') +
  theme(axis.text.x = element_text(hjust = 0.5)) +
  scale_x_continuous("winner_seed", labels = as.character(atp19$winner_seed), breaks = atp19$winner_seed)

atp19 %>% 
  filter(Upsetrd == 0) %>%
  count(winner_seed) %>%
  ggplot(aes(x = winner_seed, y = n)) + 
  geom_col(fill='steelblue', color='black') +
  theme(axis.text.x = element_text(hjust = 0.5)) +
  scale_x_continuous("winner_seed", labels = as.character(atp19$winner_seed), breaks = atp19$winner_seed)

# ..............................................................................

# Winners ranks
# upsets:

atp19 %>% 
  filter(Upsetrd == 1, wins > 4) %>%
  count(winner_rank) %>%
  arrange(n) %>%
  filter(n > 2) %>%
  mutate(rank=factor(winner_rank, levels=winner_rank)) %>% 
  ggplot(aes(x = rank, y = n)) + 
  geom_segment( aes(xend=rank, yend=0)) +
  geom_point( size=4, color="steelblue") +
  coord_flip() +
  theme_bw() +
  xlab("")


# regular:
atp19 %>% 
  filter(Upsetrd == 0, wins > 4) %>%
  count(winner_rank) %>%
  arrange(n) %>%
  filter(n > 2) %>%
  mutate(rank=factor(winner_rank, levels=winner_rank)) %>% 
  ggplot(aes(x = rank, y = n)) + 
  geom_segment( aes(xend=rank, yend=0)) +
  geom_point( size=4, color="steelblue") +
  coord_flip() +
  theme_bw() +
  xlab("")

# ..............................................................................

# Age
# Adapted from Kaggle:
# upsets:
df <- atp19 %>% 
  filter(Upsetrd == 1) %>%
  select(winner_age_int, loser_age_int)

df %>% 
  rename(
    winner = winner_age_int,
    loser = loser_age_int
  ) -> df

df %>%
  pivot_longer(everything(), names_to = "Type", values_to = "Age") ->df

ggplot(df, aes(factor(Type), Age, fill = Type)) + geom_boxplot(alpha = 0.8) + theme_light()

# Regular:
df <- atp19 %>% 
  filter(Upsetrd == 0) %>%
  select(winner_age_int, loser_age_int)

df %>% 
  rename(
    winner = winner_age_int,
    loser = loser_age_int
  ) -> df

df %>%
  pivot_longer(everything(), names_to = "Type", values_to = "Age") ->df

ggplot(df, aes(factor(Type), Age, fill = Type)) + geom_boxplot(alpha = 0.8) + theme_light()

# ..............................................................................

############# Statistics #######################################################

## Adapted From -> Predicting ATP Tennis Match Outcomes Using Serving Statistics
# https://medium.com/swlh/predicting-atp-tennis-match-outcomes-using-serving-statistics-cde03d99f410

# Ace as a predictor

# upsets:
atp19u <- atp19 %>%
  filter(Upsetrd == 1)
ace_dif <- na.omit(atp19u$w_ace) - na.omit(atp19u$l_ace)

ace_dif <- ifelse(ace_dif > 0 , 1 , 0)

#what percentage of time did winner hit more Ace than the loser
sum(ace_dif, na.rm = TRUE)*100/length(ace_dif)
# 51.88953

# statistics:
summary(atp19u$w_ace)
summary(atp19u$l_ace)

# Regular:
atp19R <- atp19 %>%
  filter(Upsetrd == 0)
ace_dif <- na.omit(atp19R$w_ace) - na.omit(atp19R$l_ace)

ace_dif <- ifelse(ace_dif > 0 , 1 , 0)

#what percentage of time did winner hit more Ace than the loser
sum(ace_dif, na.rm = TRUE)*100/length(ace_dif)
# 59.05%

# statistics:
summary(atp19R$w_ace)
summary(atp19R$l_ace)

# ..............................................................................

# Double faults as a predictor

# Upsets:

df_dif <-  atp19u$w_df -  atp19u$l_df
#number of times loser hit more double faults in a match
df_dif <- ifelse(df_dif > 0 , 1 , 0)
#what percentage of time did loser hit more double faults than the winner
sum(df_dif, na.rm = TRUE)*100/length(df_dif)
# 31.1 %
summary(atp19u$w_df)
summary(atp19u$l_df)

# Regular:

df_dif <-  atp19R$w_df -  atp19R$l_df
#number of times loser hit more double faults in a match
df_dif <- ifelse(df_dif > 0 , 1 , 0)
#what percentage of time did loser hit more double faults than the winner
sum(df_dif, na.rm = TRUE)*100/length(df_dif)
# 35.54 %
summary(atp19R$w_df)
summary(atp19R$l_df)

# ..............................................................................

# #first serve percentage = number of first serves made / number of serve points

#upsets:

p1spw <- atp19u$w_1stIn/ atp19u$w_svpt    # winner
p1spl <- atp19u$l_1stIn/atp19u$l_svpt    # loser

summary(p1spw)
summary(p1spl)

#First serve percentage as a predictor

#percentage of time winner had a better 1st serve percentage
p1sd <- ifelse((p1spw - p1spl) > 0 , 1 , 0)
sum(p1sd, na.rm = TRUE)*100/length(p1sd)
# 53.92 %

# Regular:

p1spw <- atp19R$w_1stIn/ atp19R$w_svpt   # winner
p1spl <- atp19R$l_1stIn/atp19R$l_svpt    # loser

summary(p1spw)
summary(p1spl)

#First serve percentage as a predictor

#percentage of time winner had a better 1st serve percentage
p1sd <- ifelse((p1spw - p1spl) > 0 , 1 , 0)
sum(p1sd, na.rm = TRUE)*100/length(p1sd)
# 57.77 %

# ..............................................................................

#First and second serve winning percentages

# #first serve winning percentage = number of first-serve points won / number of first serves made

# Upsets:

w1swp <- atp19u$w_1stWon/ atp19u$w_1stIn    
w1slp <- atp19u$l_1stWon/ atp19u$l_1stIn

#percentage of time the winning player had a higher winning percentage on 1st serve
f1swpd <- ifelse((w1swp - w1slp) > 0 , 1 , 0)
sum(f1swpd, na.rm = TRUE)*100/length(f1swpd)
# 76.02%

summary(w1swp)
summary(w1slp)

# Regular:

w1swp <- atp19R$w_1stWon/ atp19R$w_1stIn    
w1slp <- atp19R$l_1stWon/ atp19R$l_1stIn

#percentage of time the winning player had a higher winning percentage on 1st serve
f1swpd <- ifelse((w1swp - w1slp) > 0 , 1 , 0)
sum(f1swpd, na.rm = TRUE)*100/length(f1swpd)
# 85.73%

summary(w1swp)
summary(w1slp)

#second serve winning percentage 

# Upsets:

w2swp <- atp19u$w_2ndWon/ (atp19u$w_svpt- atp19u$w_1stIn)     
w2slp <- atp19u$l_2ndWon/ (atp19u$l_svpt- atp19u$l_1stIn)

# #percentage of time the winning player had a higher winning percentage on 2nd serve
s2swpd <- ifelse((w2swp - w2slp) > 0 , 1 , 0)
sum(s2swpd, na.rm = TRUE)*100/length(s2swpd)
# 74.71%

summary(w2swp)
summary(w2slp)


# Regular:

w2swp <- atp19R$w_2ndWon/ (atp19R$w_svpt - atp19R$w_1stIn)     
w2slp <- atp19R$l_2ndWon/ (atp19R$l_svpt - atp19R$l_1stIn)

# #percentage of time the winning player had a higher winning percentage on 2nd serve
s2swpd <- ifelse((w2swp - w2slp) > 0 , 1 , 0)
sum(s2swpd, na.rm = TRUE)*100/length(s2swpd)
# 77.40%

summary(w2swp)
summary(w2slp)

#----------------

# plots: 
# compare first serve winning percentage of loser and winner:

fswp <- data.frame(winner = w1swp, loser = w1slp )
fswp %>%
  pivot_longer(everything(), names_to = "Type", values_to = "FSWP") %>%
  ggplot(aes(x=FSWP, fill = Type))+
  geom_histogram(alpha=0.5)+
  labs(x = "First Server Winning Percentage", y = "Counts")

fswp %>%
  pivot_longer(everything(), names_to = "Type", values_to = "FSWP") %>%
  ggplot(aes(x = Type, y=FSWP, color = Type))+
  geom_boxplot()+
  labs(x = "Type", y = "First Server Percentage")

summary(fswp)

#----------------

# plots: 
# compare second serve winning percentage of loser and winner:

#----------------

# Adapted from https://medium.com/@tobikasali/exploratory-data-analysis-with-r-f0b0a5163ecd

# Break Points
# Break points faced

# Upset

bpf <- data.frame(winner = na.omit(atp19u$w_bpFaced), loser = na.omit(atp19u$l_bpFaced) )

bpf %>%
  pivot_longer(everything(), names_to = "Type", values_to = "BPF") %>%
  ggplot(aes(x=BPF, fill = Type))+
  geom_histogram(alpha=0.5)+
  labs(x = "Second Server Winning Percentage", y = "Counts")

bpf %>%
  pivot_longer(everything(), names_to = "Type", values_to = "BPF") %>%
  ggplot(aes(x = Type, y=BPF, color = Type))+
  geom_boxplot()+
  labs(x = "Type", y = "Second Server Percentage")


bpfd <- ifelse((bpf$winner - bpf$loser) > 0 , 1 , 0)
sum(bpfd, na.rm = TRUE)*100/length(bpfd)
#  22.09%

summary(bpf)


# Regular:

bpf <- data.frame(winner = na.omit(atp19R$w_bpFaced), loser = na.omit(atp19R$l_bpFaced) )
summary(bpf)

bpfd <- ifelse((bpf$winner - bpf$loser) > 0 , 1 , 0)
sum(bpfd, na.rm = TRUE)*100/length(bpfd)
#15.17%


bpf %>%
  pivot_longer(everything(), names_to = "Type", values_to = "BPF") %>%
  ggplot(aes(x=BPF, fill = Type))+
  geom_histogram(alpha=0.5)+
  labs(x = "Second Server Winning Percentage", y = "Counts")

bpf %>%
  pivot_longer(everything(), names_to = "Type", values_to = "BPF") %>%
  ggplot(aes(x = Type, y=BPF, color = Type))+
  geom_boxplot()+
  labs(x = "Type", y = "Second Server Percentage")

# -------------------

# Percentage break points saved

# upset:
#w_bpSaved = ifelse(w_bpFaced == 0, 0, w_bpSaved/w_bpFaced)
#l_bpSaved = ifelse(l_bpFaced == 0, 0, l_bpSaved/l_bpFaced)

bps <- data.frame(winner = na.omit(ifelse(atp19u$w_bpFaced == 0, 0, atp19u$w_bpSaved/atp19u$w_bpFaced)), 
                  loser = na.omit(ifelse(atp19u$l_bpFaced == 0, 0, atp19u$l_bpSaved/atp19u$l_bpFaced) ))

bpsd <- ifelse((bps$winner - bps$loser) > 0 , 1 , 0)
sum(bpsd, na.rm = TRUE)*100/length(bpsd)
# 62.65%

summary(bps)

bps %>%
  pivot_longer(everything(), names_to = "Type", values_to = "BPS") %>%
  ggplot(aes(x=BPS, fill = Type))+
  geom_histogram(alpha=0.5)+
  labs(x = "Second Server Winning Percentage", y = "Counts")

bps %>%
  pivot_longer(everything(), names_to = "Type", values_to = "BPS") %>%
  ggplot(aes(x = Type, y=BPS, color = Type))+
  geom_boxplot()+
  labs(x = "Type", y = "Second Server Percentage")


# Regular:
bps <- data.frame(winner = na.omit(ifelse(atp19R$w_bpFaced == 0, 0, atp19R$w_bpSaved/atp19R$w_bpFaced)), 
                  loser = na.omit(ifelse(atp19R$l_bpFaced == 0, 0, atp19R$l_bpSaved/atp19R$l_bpFaced) ))

summary(bps)

bpsd <- ifelse((bps$winner - bps$loser) > 0 , 1 , 0)
sum(bpsd, na.rm = TRUE)*100/length(bpsd)
# 59.68%

# ......................................

# break points saved

bps <- data.frame(winner = na.omit(atp19R$w_bpSaved), loser = na.omit(atp19R$l_bpSaved) )

bps %>%
  pivot_longer(everything(), names_to = "Type", values_to = "BPS") %>%
  ggplot(aes(x=BPS, fill = Type))+
  geom_histogram(alpha=0.5)+
  labs(x = "Second Server Winning Percentage", y = "Counts")

bps %>%
  pivot_longer(everything(), names_to = "Type", values_to = "BPS") %>%
  ggplot(aes(x = Type, y=BPS, color = Type))+
  geom_boxplot()+
  labs(x = "Type", y = "Second Server Percentage")

summary(bps)

#---------------------------
# Return Games Won Percentage

# Upsets:
w_Retpwp = (atp19u$l_svpt-(atp19u$l_1stWon+atp19u$l_2ndWon))/atp19u$l_svpt # w Return Games Won Percentage 
l_Retpwp = (atp19u$w_svpt-(atp19u$w_1stWon+atp19u$w_2ndWon))/atp19u$w_svpt # l Return Games Won Percentage 

# #percentage of time the winning player had a higher winning percentage on 2nd serve
Retd <- ifelse((w_Retpwp - l_Retpwp) > 0 , 1 , 0)
sum(Retd, na.rm = TRUE)*100/length(Retd)
# 85.47%

summary(w_Retpwp)
summary(l_Retpwp)


# Regular:

w_Retpwp = (atp19R$l_svpt-(atp19R$l_1stWon+atp19R$l_2ndWon))/atp19R$l_svpt # w Return Games Won Percentage 
l_Retpwp = (atp19R$w_svpt-(atp19R$w_1stWon+atp19R$w_2ndWon))/atp19R$w_svpt # l Return Games Won Percentage 

# #percentage of time the winning player had a higher winning percentage on 2nd serve
Retd <- ifelse((w_Retpwp - l_Retpwp) > 0 , 1 , 0)
sum(Retd, na.rm = TRUE)*100/length(Retd)
# 93.58%

summary(w_Retpwp)
summary(l_Retpwp)


# #############################################################################

# Compare the performance of the upset winner to their average performance on the same surface
# Adapted From: https://tennismash.com/2016/01/21/302/


### ........................................ ###

# 1. Break points won : Break points Saved
# 2. serve won on second serve 
# 3. Return Won on 1st Serve !!!
# 4. Return Won on 2nd Serve !!!
# 6. Return Games Won Percentage
# 7. Total Points Won
# 8. Return Points Won
# 9. Serve Won on 1st serve

### ........................................ ###


upsets <- atp19 %>% 
  filter(Upsetrd == 1) # 688

regular <- atp19 %>% 
  filter(Upsetrd == 0) # 1885

## Find the statistics of the upsets winners in regular matches: df1 (as winners in R) & df2 (as losers in R)
## 1- upsets winners as winners of regular:

upsets %>%
  select(winner_id) %>%
  distinct(winner_id) %>%
  inner_join(regular, by = c("winner_id" = "winner_id")) -> df1

#df[!duplicated(df),] -> df1


## 2- upsets winners as losers of regular:
upsets %>%
  select(winner_id) %>%
  distinct(winner_id) %>%
  inner_join(regular, by = c("winner_id" = "loser_id")) -> df2

#df[!duplicated(df),] -> df2

## Find the statistics of the upsets losers in regular matches: df3 (as winners in R) & df4 (as losers in R)
## 3- upsets losers as winners of regular:
upsets %>%
  select(loser_id) %>%
  distinct(loser_id) %>%
  inner_join(regular, by = c("loser_id" = "winner_id")) -> df3

#df[!duplicated(df),] -> df3


## 4- upsets losers as losers of regular:
upsets %>%
  select(loser_id) %>%
  distinct(loser_id) %>%
  inner_join(regular, by = c("loser_id" = "loser_id")) -> df4

#df[!duplicated(df),] -> df4

# ..........................

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
      Tstat[1, n] <- mean(df$w_ace)                            ## Ace
      Tstat[2, n] <- mean(df$w_df)                             ## double faults
      Tstat[3, n] <- mean(df$w_1stIn/df$w_svpt)                ## First serve percentage
      Tstat[4, n] <- mean(df$w_1stWon/df$w_1stIn)              ## First serve winning percentages
      Tstat[5, n] <- mean(df$w_2ndWon/(df$w_svpt- df$w_1stIn)) ## Second serve winning percentages
      Tstat[6, n] <- mean(ifelse(df$w_bpFaced == 0, 0, df$w_bpSaved/df$w_bpFaced))  ## break points saved (%)
      Tstat[7, n] <- mean(df$w_bpFaced)                                             ## break points faced (n)
      Tstat[8, n] <- mean((df$l_svpt-(df$l_1stWon+df$l_2ndWon))/df$l_svpt)          ## Return Games Won Percentage
    }
  } else {
    ifelse(n <= 7, n , 10)
    for (s in surface){
      df <- d %>% filter(surface == s)
      n <- n +1
      Tstat[1, n] <- mean(df$l_ace)                             ## Ace
      Tstat[2, n] <- mean(df$l_df)                              ## double faults
      Tstat[3, n] <- mean(df$l_1stIn/df$l_svpt)                 ## First serve percentage
      Tstat[4, n] <- mean(df$l_1stWon/df$l_1stIn)               ## First serve winning percentages
      Tstat[5, n] <- mean(df$l_2ndWon/(df$l_svpt- df$l_1stIn))  ## Second serve winning percentages
      Tstat[6, n] <- mean(ifelse(df$l_bpFaced == 0, 0, df$l_bpSaved/df$l_bpFaced))        ## break points saved (%)
      Tstat[7, n] <- mean(df$l_bpFaced)                                                   ## break points faced (n)
      Tstat[8, n] <- mean((df$w_svpt-(df$w_1stWon+df$w_2ndWon))/df$w_svpt)                ## Return Games Won Percentage
    }
  }
}

# ..........................

# mean in regular matches: upset winners df1 & df2 ; upset losers: df3 & df4

Tstat2 <- data.frame(matrix(ncol = 7, nrow = 8))
x <- c("Stat", "dUC", "dUG","dUH", "dRC", "dRG","dRH")   # U: upset winners ; R: upset losers 
colnames(Tstat2) <- x

# stats of interest:
Tstat2$Stat <-c("Ace", "DF", "First Serve %", 
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

# ..........................

# mean of winners in upset matches based on surface:
upsets <- atp19 %>% 
  filter(Upsetrd == 1) # 707

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
  Tstat_up[1, n] <- mean(df$w_ace)                   ## Ace
  Tstat_up[2, n] <- mean(df$w_df)                    ## double faults
  Tstat_up[3, n] <- mean(df$w_1stIn/df$w_svpt)     ## First serve percentage
  Tstat_up[4, n] <- mean(df$w_1stWon/df$w_1stIn)   ## First serve winning percentages
  Tstat_up[5, n] <- mean(df$w_2ndWon/(df$w_svpt- df$w_1stIn))  ## Second serve winning percentages
  Tstat_up[6, n] <- mean(ifelse(df$w_bpFaced == 0, 0, df$w_bpSaved/df$w_bpFaced))  ## break points saved (%)
  Tstat_up[7, n] <- mean(df$w_bpFaced)                                             ## break points faced (n)
  Tstat_up[8, n] <- mean((df$l_svpt-(df$l_1stWon+df$l_2ndWon))/df$l_svpt)          ## Return Games Won Percentage
  
}

# ..........................

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

# ..........................

# mean of losers in upset matches based on surface:
upsets <- atp19 %>% 
  filter(Upsetrd == 1) # 707

Tstat_Ra <- data.frame(matrix(ncol = 4, nrow = 8))
x <- c("Stat", "C", "G","H")
colnames(Tstat_Ra) <- x

# stats of interest:
Tstat_Ra$Stat <-c("Ace", "DF", "First Serve %", 
                  "1st Srv Won %", "2nd Srv Won %", 
                  "BP Saved %", "BP Faced", "Return %")
n <- 1
for (s in surface){
  df <- upsets %>% filter(surface == s)
  n <- n +1
  Tstat_Ra[1, n] <- mean(df$l_ace)                             ## Ace
  Tstat_Ra[2, n] <- mean(df$l_df)                              ## double faults
  Tstat_Ra[3, n] <- mean(df$l_1stIn/df$l_svpt)                 ## First serve percentage
  Tstat_Ra[4, n] <- mean(df$l_1stWon/df$l_1stIn)               ## First serve winning percentages
  Tstat_Ra[5, n] <- mean(df$l_2ndWon/(df$l_svpt- df$l_1stIn))  ## Second serve winning percentages
  Tstat_Ra[6, n] <- mean(ifelse(df$l_bpFaced == 0, 0, df$l_bpSaved/df$l_bpFaced))        ## break points saved (%)
  Tstat_Ra[7, n] <- mean(df$l_bpFaced)                                                   ## break points faced (n)
  Tstat_Ra[8, n] <- mean((df$w_svpt-(df$w_1stWon+df$w_2ndWon))/df$w_svpt)                ## Return Games Won Percentage
}

# ..........................

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

# ..........................

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

# Adapted from https://stackoverflow.com/questions/18265941/two-horizontal-bar-charts-with-shared-axis-in-ggplot2-similar-to-population-pyr

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
        text = element_text(size=20),
        plot.title = element_text(size = 20, face = "bold"),
        plot.margin = unit(c(1,-1,1,0), "mm")) +
  scale_y_reverse() + coord_flip()

g2 <- ggplot(data = Plot1, aes(x = Stat, y = Avg_regular)) +xlab(NULL)+
  geom_bar(stat = "identity", fill = "#DC143C") + ggtitle("Upsets Losers' Average Performance Drop") +
  geom_text(aes(label = percent(Avg_regular/100), vjust = 0.5, hjust = 0.4), size=7) +
  theme(axis.title.x = element_blank(), axis.title.y = element_blank(), 
        axis.text.y = element_blank(), axis.ticks.y = element_blank(),
        text = element_text(size=20),
        plot.title = element_text(size = 20, face = "bold"),
        plot.margin = unit(c(1,0,1,-1), "mm")) +
  coord_flip()

library(gridExtra)
gg1 <- ggplot_gtable(ggplot_build(g1))
gg2 <- ggplot_gtable(ggplot_build(g2))
gg.mid <- ggplot_gtable(ggplot_build(g.mid))

grid.arrange(gg1,gg.mid,gg2,ncol=3,widths=c(4/9,1/9,4/9))

################################################################################

# 25/7/2021

# Compare the performance of the seeds to their average performance on the same surface
# (Para) Seeds % Below Personal Average in Upsets (Ranked instead of Seeds)
# seeds Vs unseeded 


atp19$winner_seed <- ifelse(is.na(atp19$winner_seed), "Unseeded", atp19$winner_seed )
table(atp19$winner_seed)

atp19$loser_seed <- ifelse(is.na(atp19$loser_seed), "Unseeded", atp19$loser_seed )
table(atp19$loser_seed)

atp19$UpsetS <- ifelse(atp19$winner_seed == "Unseeded" & atp19$loser_seed != "Unseeded", 1, 0)
sum(atp19$UpsetS) # 456 upset matches:  seeds Vs unseeded 


S_upsets <- atp19 %>% 
  filter(UpsetS == 1) # 456

S_regular <- atp19 %>% 
  filter(UpsetS == 0) # 2132

# ..............................................................................

# EDA: upsets: seeds Vs unseeded 

# Surface

# percentage of each surface
# upset:
atp19 %>% 
  filter(UpsetS == 1) %>%
  count(surface) %>% 
  mutate(perc = n*100 / sum(n)) -> atp19p

ggplot(atp19p, aes(x = surface, y = perc)) + 
  geom_bar(stat = "identity",fill='steelblue', color='black') +
  geom_text(aes(label = percent(round(perc)/100), vjust = -1))

#..............................................................................

# winner_hand
# upset:
atp19 %>% 
  filter(UpsetS == 1) %>%
  count(winner_hand) %>% 
  mutate(perc = n*100 / nrow(atp19)) -> atp19wh

ggplot(atp19wh, aes(x= winner_hand, y = perc)) + 
  geom_bar(stat = "identity",fill='steelblue', color='black') +
  geom_text(aes(label = percent(round(perc)/100), vjust = -1)) + 
  scale_x_discrete(drop=TRUE) + xlab("Hand") + ylab("Total matches") 

# regular:
atp19 %>% 
  filter(UpsetS == 1) %>%
  count(loser_hand) %>% 
  mutate(perc = n*100 / nrow(atp19)) -> atp19wh

ggplot(atp19wh, aes(x= loser_hand, y = perc)) + 
  geom_bar(stat = "identity",fill='steelblue', color='black') +
  geom_text(aes(label = percent(round(perc)/100), vjust = -1)) + 
  scale_x_discrete(drop=TRUE) + xlab("Hand") + ylab("Total matches") 
#..............................................................................

## age
# upset:
atp19 %>% 
  filter(UpsetS == 1) %>%
  summarise(meanau = mean(winner_age_int))
# 27.0

atp19 %>% 
  filter(UpsetS == 1) %>%
  summarise(meanar = mean(loser_age_int))
# 27.0

# regular:

atp19 %>% 
  filter(UpsetS == 0) %>%
  summarise(meanar = mean(winner_age_int))
# 27.3

atp19 %>% 
  filter(UpsetS == 0) %>%
  summarise(meanar = mean(loser_age_int))

# 27.15%

#..............................................................................

# Height:

# upset:

atp19 %>% 
  filter(UpsetS == 1) %>%
  summarise(meanah = mean(winner_ht, na.rm = TRUE))

atp19 %>% 
  filter(UpsetS == 1) %>%
  summarise(meanah = mean(loser_ht, na.rm = TRUE))

# regular:

atp19 %>% 
  filter(UpsetS == 0) %>%
  summarise(meanah = mean(winner_ht, na.rm = TRUE))

atp19 %>% 
  filter(UpsetS == 0) %>%
  summarise(meanah = mean(loser_ht, na.rm = TRUE))

#..............................................................................
# best of

# upset:

atp19 %>% 
  filter(UpsetS == 1) %>%
  ggplot(aes(x = best_of)) +
  geom_bar(fill='steelblue', color='black') # mostly 3

# regular:
atp19 %>% 
  filter(UpsetS == 0) %>%
  ggplot(aes(x = best_of)) +
  geom_bar(fill='steelblue', color='black') # mostly 3

#..............................................................................

# number of matches per level:

# upset:

atp19 %>%
  filter(UpsetS == 1) %>%
  arrange(desc(tourney_level))  %>%
  group_by(tourney_level) %>%
  summarise(avgl = n()) 

# percentage of each level
atp19 %>% 
  filter(UpsetS == 1) %>%
  count(tourney_level) %>% 
  mutate(perc = n*100 / sum(n)) %>%
  ggplot(aes(x = tourney_level, y = perc)) + 
  geom_bar(stat = "identity", fill='steelblue', color='black') +
  geom_text(aes(label = percent(round(perc)/100), vjust = -1))

### not in : F & D : Tour finals

# regular:
atp19 %>% 
  filter(UpsetS == 0) %>%
  count(tourney_level) %>% 
  mutate(perc = n*100 / nrow(atp19)) %>%
  ggplot(aes(x = tourney_level, y = perc)) + 
  geom_bar(stat = "identity", fill='steelblue', color='black') +
  geom_text(aes(label = percent(round(perc)/100), vjust = -1))

#..............................................................................

# minutes:

atp19 %>% 
  filter(UpsetS == 1) -> m1
summary(m1$minutes)

atp19 %>% 
  filter(UpsetS == 0) -> m0
summary(m0$minutes)

mperavg <- (mean(m1$minutes, na.rm = TRUE)-mean(m0$minutes, na.rm = TRUE))*200/(mean(m1$minutes, na.rm = TRUE)+mean(m0$minutes, na.rm = TRUE))

# 5.42% 
#..............................................................................

# Winners ranks
# upsets:

atp19 %>% 
  filter(UpsetS == 1, wins > 4) %>%
  count(winner_rank) %>%
  arrange(n) %>%
  filter(n > 2) %>%
  mutate(rank=factor(winner_rank, levels=winner_rank)) %>% 
  ggplot(aes(x = rank, y = n)) + 
  geom_segment( aes(xend=rank, yend=0)) +
  geom_point( size=4, color="steelblue") +
  coord_flip() +
  theme_bw() +
  xlab("")


# regular:
atp19 %>% 
  filter(UpsetS == 0, wins > 4) %>%
  count(winner_rank) %>%
  arrange(n) %>%
  filter(n > 2) %>%
  mutate(rank=factor(winner_rank, levels=winner_rank)) %>% 
  ggplot(aes(x = rank, y = n)) + 
  geom_segment( aes(xend=rank, yend=0)) +
  geom_point( size=4, color="steelblue") +
  coord_flip() +
  theme_bw() +
  xlab("")

# ..............................................................................

# mean statistics

# upsets: seeds Vs unseeded 

# Ace

# #Number of Ace as a predictor

# upsets:

ace_dif <- na.omit(S_upsets$w_ace) - na.omit(S_upsets$l_ace)

ace_dif <- ifelse(ace_dif > 0 , 1 , 0)

#what percentage of time did winner hit more Ace than the loser
sum(ace_dif, na.rm = TRUE)*100/length(ace_dif)
# 54.60526

# statistics:
summary(S_upsets$w_ace)
summary(S_upsets$l_ace)

# Regular:
ace_dif <- na.omit(S_regular$w_ace) - na.omit(S_regular$l_ace)

ace_dif <- ifelse(ace_dif > 0 , 1 , 0)

#what percentage of time did winner hit more Ace than the loser
sum(ace_dif, na.rm = TRUE)*100/length(ace_dif)
#57.74%

# statistics:
summary(S_regular$w_ace)
summary(S_regular$l_ace)

#-------------------

# Double faults faults as a predictor


# Upsets:

df_dif <-  S_upsets$w_df -  S_upsets$l_df
#number of times loser hit more double faults in a match
df_dif <- ifelse(df_dif > 0 , 1 , 0)
#what percentage of time did loser hit more double faults than the winner
sum(df_dif, na.rm = TRUE)*100/length(df_dif)
# 31.59 %
summary(S_upsets$w_df)
summary(S_upsets$l_df)

# Regular:

df_dif <-  S_regular$w_df -  S_regular$l_df

#number of times loser hit more double faults in a match
df_dif <- ifelse(df_dif > 0 , 1 , 0)

#what percentage of time did loser hit more double faults than the winner
sum(df_dif, na.rm = TRUE)*100/length(df_dif)
# 34.94 %

summary(S_regular$w_df)
summary(S_regular$l_df)



# ----------------- Referenced---------------------------------------------------------------------

# The first serve percentage = the number of times a player started a point with a first serve /
#  the total number of points a player played on his serve.

# #first serve percentage = number of first serves made / number of serve points

#upsets:

p1spw <- S_upsets$w_1stIn/ S_upsets$w_svpt    # winner
p1spl <- S_upsets$l_1stIn/S_upsets$l_svpt    # loser

summary(p1spw)
summary(p1spl)

#First serve percentage as a predictor

#percentage of time winner had a better 1st serve percentage
p1sd <- ifelse((p1spw - p1spl) > 0 , 1 , 0)
sum(p1sd, na.rm = TRUE)*100/length(p1sd)
# 54.82 %

# Regular:

p1spw <- S_regular$w_1stIn/ S_regular$w_svpt   # winner
p1spl <- S_regular$l_1stIn/S_regular$l_svpt    # loser

summary(p1spw)
summary(p1spl)

#First serve percentage as a predictor

#percentage of time winner had a better 1st serve percentage
p1sd <- ifelse((p1spw - p1spl) > 0 , 1 , 0)
sum(p1sd, na.rm = TRUE)*100/length(p1sd)
# 57.36 %

#..............................................................................

#First and second serve winning percentages

# #first serve winning percentage = number of first-serve points won / number of first serves made

# Upsets:

w1swp <- S_upsets$w_1stWon/ S_upsets$w_1stIn    
w1slp <- S_upsets$l_1stWon/ S_upsets$l_1stIn

#percentage of time the winning player had a higher winning percentage on 1st serve
f1swpd <- ifelse((w1swp - w1slp) > 0 , 1 , 0)
sum(f1swpd, na.rm = TRUE)*100/length(f1swpd)
# 73.25%

summary(w1swp)
summary(w1slp)

# Regular:

w1swp <- S_regular$w_1stWon/ S_regular$w_1stIn    
w1slp <- S_regular$l_1stWon/ S_regular$l_1stIn

#percentage of time the winning player had a higher winning percentage on 1st serve
f1swpd <- ifelse((w1swp - w1slp) > 0 , 1 , 0)
sum(f1swpd, na.rm = TRUE)*100/length(f1swpd)
# 85.23%

summary(w1swp)
summary(w1slp)

#second serve winning percentage 

# Upsets:

w2swp <- S_upsets$w_2ndWon/ (S_upsets$w_svpt- S_upsets$w_1stIn)     
w2slp <- S_upsets$l_2ndWon/ (S_upsets$l_svpt- S_upsets$l_1stIn)

# #percentage of time the winning player had a higher winning percentage on 2nd serve
s2swpd <- ifelse((w2swp - w2slp) > 0 , 1 , 0)
sum(s2swpd, na.rm = TRUE)*100/length(s2swpd)
# 74.78%

summary(w2swp)
summary(w2slp)


# Regular:

w2swp <- S_regular$w_2ndWon/ (S_regular$w_svpt - S_regular$w_1stIn)     
w2slp <- S_regular$l_2ndWon/ (S_regular$l_svpt - S_regular$l_1stIn)

# #percentage of time the winning player had a higher winning percentage on 2nd serve
s2swpd <- ifelse((w2swp - w2slp) > 0 , 1 , 0)
sum(s2swpd, na.rm = TRUE)*100/length(s2swpd)
# 77.16%

summary(w2swp)
summary(w2slp)

#..............................................................................

# from https://medium.com/@tobikasali/exploratory-data-analysis-with-r-f0b0a5163ecd

# Break Points
# Break points faced

# Upset

bpf <- data.frame(winner = na.omit(S_upsets$w_bpFaced), loser = na.omit(S_upsets$l_bpFaced) )


bpfd <- ifelse((bpf$winner - bpf$loser) > 0 , 1 , 0)
sum(bpfd, na.rm = TRUE)*100/length(bpfd)
#  25%

summary(bpf)

# Regular:

bpf <- data.frame(winner = na.omit(S_regular$w_bpFaced), loser = na.omit(S_regular$l_bpFaced) )
summary(bpf)

bpfd <- ifelse((bpf$winner - bpf$loser) > 0 , 1 , 0)
sum(bpfd, na.rm = TRUE)*100/length(bpfd)
#15.34%

# Percentage break points saved

# upset:

bps <- data.frame(winner = na.omit(ifelse(S_upsets$w_bpFaced == 0, 0, S_upsets$w_bpSaved/S_upsets$w_bpFaced)), 
                  loser = na.omit(ifelse(S_upsets$l_bpFaced == 0, 0, S_upsets$l_bpSaved/S_upsets$l_bpFaced) ))

bpsd <- ifelse((bps$winner - bps$loser) > 0 , 1 , 0)
sum(bpsd, na.rm = TRUE)*100/length(bpsd)
# 64.69%

summary(bps)

# Regular:
bps <- data.frame(winner = na.omit(ifelse(S_regular$w_bpFaced == 0, 0, S_regular$w_bpSaved/S_regular$w_bpFaced)), 
                  loser = na.omit(ifelse(S_regular$l_bpFaced == 0, 0, S_regular$l_bpSaved/S_regular$l_bpFaced) ))

summary(bps)

bpsd <- ifelse((bps$winner - bps$loser) > 0 , 1 , 0)
sum(bpsd, na.rm = TRUE)*100/length(bpsd)
# 59.68%

#..............................................................................

# Return Games Won Percentage

# Upsets:
w_Retpwp = (S_upsets$l_svpt-(S_upsets$l_1stWon+S_upsets$l_2ndWon))/S_upsets$l_svpt # w Return Games Won Percentage 
l_Retpwp = (S_upsets$w_svpt-(S_upsets$w_1stWon+S_upsets$w_2ndWon))/S_upsets$w_svpt # l Return Games Won Percentage 

# #percentage of time the winning player had a higher winning percentage on 2nd serve
Retd <- ifelse((w_Retpwp - l_Retpwp) > 0 , 1 , 0)
sum(Retd, na.rm = TRUE)*100/length(Retd)
# 84.87%

summary(w_Retpwp)
summary(l_Retpwp)

# Regular:

w_Retpwp = (S_regular$l_svpt-(S_regular$l_1stWon+S_regular$l_2ndWon))/S_regular$l_svpt # w Return Games Won Percentage 
l_Retpwp = (S_regular$w_svpt-(S_regular$w_1stWon+S_regular$w_2ndWon))/S_regular$w_svpt # l Return Games Won Percentage 

# #percentage of time the winning player had a higher winning percentage on 2nd serve
Retd <- ifelse((w_Retpwp - l_Retpwp) > 0 , 1 , 0)
sum(Retd, na.rm = TRUE)*100/length(Retd)
# 92.82%

summary(w_Retpwp)
summary(l_Retpwp)


# ..............................................................................

## Find the statistics of the upsets winners in regular matches: ds1 (as winners in R) & ds2 (as losers in R)
## 1- upsets winners as winners of regular:

S_upsets %>%
  select(winner_id) %>%
  distinct(winner_id) %>%
  inner_join(S_regular, by = c("winner_id" = "winner_id")) -> ds1

## 2- upsets winners as losers of regular:
S_upsets %>%
  select(winner_id) %>%
  distinct(winner_id) %>%
  inner_join(S_regular, by = c("winner_id" = "loser_id")) -> ds2

## Find the statistics of the upsets losers in regular matches: df3 (as winners in R) & df4 (as losers in R)
## 3- upsets losers as winners of regular:
S_upsets %>%
  select(loser_id) %>%
  distinct(loser_id) %>%
  inner_join(S_regular, by = c("loser_id" = "winner_id")) -> ds3


## 4- upsets losers as losers of regular:
S_upsets %>%
  select(loser_id) %>%
  distinct(loser_id) %>%
  inner_join(S_regular, by = c("loser_id" = "loser_id")) -> ds4

# ............

## statistics of the upsets winners in regular matches (df1 & df2)
## Then compare these statistics in upset matches in the same surface: (S_upsets)

data_list <- list(ds1, ds2, ds3, ds4)                    

TstatS <- data.frame(matrix(ncol = 13, nrow = 8))
x <- c("Stat", "ds1C", "ds1G","ds1H", "ds2C", "ds2G","ds2H","ds3C", "ds3G","ds3H","ds4C", "ds4G","ds4H")
colnames(TstatS) <- x

# stats of interest:
TstatS$Stat <-  c("Ace", "DF", "First Serve %", 
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
      TstatS[1, n] <- mean(df$w_ace)                            ## Ace
      TstatS[2, n] <- mean(df$w_df)                             ## double faults
      TstatS[3, n] <- mean(df$w_1stIn/df$w_svpt)                ## First serve percentage
      TstatS[4, n] <- mean(df$w_1stWon/df$w_1stIn)              ## First serve winning percentages
      TstatS[5, n] <- mean(df$w_2ndWon/(df$w_svpt- df$w_1stIn)) ## Second serve winning percentages
      TstatS[6, n] <- mean(ifelse(df$w_bpFaced == 0, 0, df$w_bpSaved/df$w_bpFaced))  ## break points saved (%)
      TstatS[7, n] <- mean(df$w_bpFaced)                                             ## break points faced (n)
      TstatS[8, n] <- mean((df$l_svpt-(df$l_1stWon+df$l_2ndWon))/df$l_svpt)          ## Return Games Won Percentage
    }
  } else {
    ifelse(n <= 7, n , 10)
    for (s in surface){
      df <- d %>% filter(surface == s)
      n <- n +1
      TstatS[1, n] <- mean(df$l_ace)                             ## Ace
      TstatS[2, n] <- mean(df$l_df)                              ## double faults
      TstatS[3, n] <- mean(df$l_1stIn/df$l_svpt)                 ## First serve percentage
      TstatS[4, n] <- mean(df$l_1stWon/df$l_1stIn)               ## First serve winning percentages
      TstatS[5, n] <- mean(df$l_2ndWon/(df$l_svpt- df$l_1stIn))  ## Second serve winning percentages
      TstatS[6, n] <- mean(ifelse(df$l_bpFaced == 0, 0, df$l_bpSaved/df$l_bpFaced))        ## break points saved (%)
      TstatS[7, n] <- mean(df$l_bpFaced)                                                   ## break points faced (n)
      TstatS[8, n] <- mean((df$w_svpt-(df$w_1stWon+df$w_2ndWon))/df$w_svpt)                ## Return Games Won Percentage
    }
  }
}

# ............

# mean in regular matches: upset winners df1 & df2 ; upset losers: df3 & df4

Tstat2S <- data.frame(matrix(ncol = 7, nrow = 8))
x <- c("Stat", "dUC", "dUG","dUH", "dRC", "dRG","dRH")   # U: upset winners ; R: upset losers 
colnames(Tstat2S) <- x

# stats of interest:
Tstat2S$Stat <-  c("Ace", "DF", "First Serve %", 
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
      Tstat2S[i, h] <- (TstatS[i, n]+TstatS[i, n+3])/2
    }
  }
}

# ........

# mean of winners in upset matches based on surfAce:

S_upsets <- atp19 %>% 
  filter(UpsetS == 1) # 454

Tstat_upS <- data.frame(matrix(ncol = 4, nrow = 8))
x <- c("Stat", "C", "G","H")
colnames(Tstat_upS) <- x

# stats of interest:
Tstat_upS$Stat <-  c("Ace", "DF", "First Serve %", 
                     "1st Srv Won %", "2nd Srv Won %", 
                     "BP Saved %", "BP Faced", "Return %")
n <- 1
for (s in surface){
  df <- S_upsets %>% filter(surface == s)
  n <- n +1
  Tstat_upS[1, n] <- mean(df$w_ace)                   ## Ace
  Tstat_upS[2, n] <- mean(df$w_df)                    ## double faults
  Tstat_upS[3, n] <- mean(df$w_1stIn/df$w_svpt)     ## First serve percentage
  Tstat_upS[4, n] <- mean(df$w_1stWon/df$w_1stIn)   ## First serve winning percentages
  Tstat_upS[5, n] <- mean(df$w_2ndWon/(df$w_svpt- df$w_1stIn))  ## Second serve winning percentages
  Tstat_upS[6, n] <- mean(ifelse(df$w_bpFaced == 0, 0, df$w_bpSaved/df$w_bpFaced))  ## break points saved (%)
  Tstat_upS[7, n] <- mean(df$w_bpFaced)                                             ## break points faced (n)
  Tstat_upS[8, n] <- mean((df$l_svpt-(df$l_1stWon+df$l_2ndWon))/df$l_svpt)          ## Return Games Won Percentage
}

# ............
# find the improvement % in upsets winners performance from regular to upset matches :
Tstat_up_perS <- data.frame(matrix(ncol = 5, nrow = 8))
x <- c("Stat", "Clay", "Grass","Hard", "Avg%")
colnames(Tstat_up_perS) <- x

# stats of interest:
Tstat_up_perS$Stat <-  c("Ace", "DF", "First Serve %", 
                         "1st Srv Won %", "2nd Srv Won %", 
                         "BP Saved %", "BP Faced", "Return %")

Tstat_up_perS[,2:4] <- ( Tstat_upS[,2:4] - Tstat2S[, 2:4] )*100/Tstat2S[,2:4]

Tstat_up_perS[,5] <- rowMeans(Tstat_up_perS[,2:4])

# ......................

# mean of losers in upset matches based on surfAce:
S_upsets <- atp19 %>% 
  filter(UpsetS == 1) # 707

Tstat_Se <- data.frame(matrix(ncol = 4, nrow = 8))
x <- c("Stat", "C", "G","H")
colnames(Tstat_Se) <- x

# stats of interest:
Tstat_Se$Stat <-  c("Ace", "DF", "First Serve %", 
                    "1st Srv Won %", "2nd Srv Won %", 
                    "BP Saved %", "BP Faced", "Return %")
n <- 1
for (s in surface){
  df <- S_upsets %>% filter(surface == s)
  n <- n +1
  Tstat_Se[1, n] <- mean(df$l_ace)                             ## Ace
  Tstat_Se[2, n] <- mean(df$l_df)                              ## double faults
  Tstat_Se[3, n] <- mean(df$l_1stIn/df$l_svpt)                 ## First serve percentage
  Tstat_Se[4, n] <- mean(df$l_1stWon/df$l_1stIn)               ## First serve winning percentages
  Tstat_Se[5, n] <- mean(df$l_2ndWon/(df$l_svpt- df$l_1stIn))  ## Second serve winning percentages
  Tstat_Se[6, n] <- mean(ifelse(df$l_bpFaced == 0, 0, df$l_bpSaved/df$l_bpFaced))        ## break points saved (%)
  Tstat_Se[7, n] <- mean(df$l_bpFaced)                                                   ## break points faced (n)
  Tstat_Se[8, n] <- mean((df$w_svpt-(df$w_1stWon+df$w_2ndWon))/df$w_svpt)                ## Return Games Won Percentage
}

# ............
# find the improvement % in upsets winners performance from regular to upset matches :
Tstat_Se_per <- data.frame(matrix(ncol = 5, nrow = 8))
x <- c("Stat", "Clay", "Grass","Hard", "Avg%")
colnames(Tstat_Se_per) <- x

# stats of interest:
Tstat_Se_per$Stat <-  c("Ace", "DF", "First Serve %", 
                        "1st Srv Won %", "2nd Srv Won %", 
                        "BP Saved %", "BP Faced", "Return %")

Tstat_Se_per[,2:4] <- ( Tstat_Se[,2:4] - Tstat2S[, 5:7] )*100/Tstat2S[,5:7]

Tstat_Se_per[,5] <- rowMeans(Tstat_Se_per[,2:4])

# .........

# plots of percentage:

# make a joint:

Plot2 <- Tstat_up_perS %>%
  full_join(Tstat_Se_per, by = c("Stat"), suffix = c("_upset", "_regular")) 

Plot2 <- Plot2 %>% 
  rename(
    Avg_upset = "Avg%_upset",
    Avg_regular = "Avg%_regular"
  )

#Plot2 <- Plot2[-8, ]

library(grid)

# from https://stackoverflow.com/questions/18265941/two-horizontal-bar-charts-with-shared-axis-in-ggplot2-similar-to-population-pyr


g.mid<-ggplot(Plot2,aes(x=1,y=Stat))+geom_text(aes(label=Stat), size=7)+
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

g1 <- ggplot(data = Plot2, aes(x = Stat, y = Avg_upset)) +
  geom_bar(stat = "identity", fill = "#3CB371") + ggtitle("Upsets Winners' Average Performance Improvment") +
  geom_text(aes(label = percent(Avg_upset/100), vjust = 0.5, hjust =  0.4), size=7) +
  theme(axis.title.x = element_blank(), 
        axis.title.y = element_blank(), 
        axis.text.y = element_blank(), 
        axis.ticks.y = element_blank(), 
        text = element_text(size=20),
        plot.title = element_text(size = 20, face = "bold"),
        plot.margin = unit(c(1,-1,1,0), "mm")) +
  scale_y_reverse() + coord_flip()

g2 <- ggplot(data = Plot2, aes(x = Stat, y = Avg_regular)) +xlab(NULL)+
  geom_bar(stat = "identity", fill = "#DC143C") + ggtitle("Upsets Losers' Average Performance Drop") +
  geom_text(aes(label = percent(Avg_regular/100), vjust = 0.5, hjust = 0.4), size=7) +
  theme(axis.title.x = element_blank(), axis.title.y = element_blank(), 
        axis.text.y = element_blank(), axis.ticks.y = element_blank(),
        text = element_text(size=20),
        plot.title = element_text(size = 20, face = "bold"),
        plot.margin = unit(c(1,0,1,-1), "mm")) +
  coord_flip()

library(gridExtra)
gg1 <- ggplot_gtable(ggplot_build(g1))
gg2 <- ggplot_gtable(ggplot_build(g2))
gg.mid <- ggplot_gtable(ggplot_build(g.mid))

grid.arrange(gg1,gg.mid,gg2,ncol=3,widths=c(4/9,1/9,4/9))


################################################################################

#################################################################################

###############################################################################

###############################################################################

