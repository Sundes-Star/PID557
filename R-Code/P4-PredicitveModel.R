# the distribution of M is not normal, the relationships are non-linear, 
# then make the output categorical (lose/win), encode the two outcomes as 0/1 or FALSE and TRUE, 
# use logistic regression for modeling,  the model returns the probability of winning a match.

# Read in Files: 2017, 2018 seasons

# Reading the firles adapted from
# https://blog.exploratory.io/how-to-read-multiple-excel-or-csv-files-together-42af5d314a10
# Since all file share the same data structure, Combine the dataaset into one data frame for easy manipulation 
datset <-list.files(pattern="*.csv")
ATP2Seasons <- sapply(datset, read_csv, simplify=FALSE) %>% 
  bind_rows(.id = "id")

ATP2Seasons %>% group_by(tourney_name) %>%
  count(match_num) 
# 203 tournaments and 5775 matches 
# 827 ; 8,943

# Cleaning:
#1- Keep only completed matches, Remove withdrawals & walkovers
# RET ,  W/O    
# Delete rows with incomplete matches
ATP2Seasons<-ATP2Seasons[!(ATP2Seasons$score=="RET" | ATP2Seasons$score=="W/O"),]

# 2- Exclude Davis Cup: 
ATP2Seasons <- ATP2Seasons %>%
  filter(tourney_level != "D")

# 3- drop rows that have NA statistics:
ATP2Seasons<-ATP2Seasons[!(is.na(ATP2Seasons$w_ace)),]   # when ace is NA, mostly, the rest of the statistics are NA

glimpse(ATP2Seasons)
# resulted in 5250 matches
# 26,507
################################################################################

# Features Engineering:
# make a new data frame containing the historical stats of each player in the clean data set (From wining and losing matches)

# before joining the winning and losing stats for each player, calculate the win_ration:

# 1- wins ratio:
ATP2Seasons <- ATP2Seasons %>% 
  group_by(winner_name)%>%
  mutate(w_wins = n())

# Win ratio:
# loses of winners:
Lossdf <- ATP2Seasons %>% group_by(loser_name)%>%
  summarise(w_loses = n())

# join 2 df:
ATP2Seasons <- ATP2Seasons %>% 
  inner_join(Lossdf, by = c("winner_name" = "loser_name")) %>% 
  mutate(w_wr = w_wins/(w_wins + w_loses)) %>%ungroup()

# make the win_ratio for losers = 0, it will be averaged after taking the mean of historical data for players whose win ratio is not 0
ATP2Seasons$l_wr <- 0


# 2- calculate statistics:
ATP2Seasons <- ATP2Seasons %>% 
  mutate(w_bpSaved = ifelse(w_bpFaced == 0, 0, w_bpSaved/w_bpFaced),
         l_bpSaved = ifelse(l_bpFaced == 0, 0, l_bpSaved/l_bpFaced),
         w_F1sp  = w_1stIn/w_svpt,                      # w 1st serve %
         w_S2sp  = 1-w_F1sp,                            # w 2nd serve % 
         w_F1swp = w_1stWon/w_1stIn,                    # w 1st serve wining %
         w_S2swp = w_2ndWon/(w_svpt-w_1stIn),           # w 2nd serve wining %
         l_F1sp  = l_1stIn/l_svpt,                      # l 1st serve %
         l_S2sp  = 1-l_F1sp,                            # l 2nd serve % 
         l_F1swp = l_1stWon/l_1stIn,                    # l 1st serve wining %
         l_S2swp = l_2ndWon/(l_svpt-l_1stIn),           # l 2nd serve wining %
         w_F1sRpw = 1 - l_F1swp,                        # w First serve return points won (%) 
         w_S2sRpw = 1 - l_S2swp,                        # w Second serve return points won (%)
         l_F1sRpw = 1 - w_F1swp,                        # l First serve return points won (%) 
         l_S2sRpw = 1 - w_S2swp,                        # l Second serve return points won (%)
         w_Retpwp = (l_svpt-(l_1stWon+l_2ndWon))/l_svpt, # w Return Games Won Percentage 
         l_Retpwp = (w_svpt-(w_1stWon+w_2ndWon))/w_svpt, # l Return Games Won Percentage 
         # the probability of winning a match: for winners:
         w_P = w_F1sp*w_F1swp + (1-w_F1sp)*w_S2swp,                 # Probability of winning a point when serving.
         w_W = (w_P^4)*(15-4*w_P-(10*w_P^2)/(1-2*w_P*(1-w_P))),              # The Probability of winning a game: W
         w_D = (w_P^2)*(1-2*w_P*(1-w_P)),                                # The Probability of winning from deuce: D
         w_TB = (w_P^7)*(1+7*(1-w_P)+28*(1-w_P)^2+84*(1-w_P)^3+210*(1-w_P)^4+462*(1-w_P)^5)+924*(w_P^6)*(1-w_P)^6*w_D,
         w_S = (w_W^6)*(1+6*(1-w_W)+21*(1-w_W)^2+56*(1-w_W)^3+126*(1-w_W)^4+252*w_W*(1-w_W)^5+504*(1-w_W)^6*w_TB), 
         w_M = ifelse(best_of == 3, w_S^2 *(1+2*(1-w_S)), w_S^3 * (1+3*(1-w_S)+ 6*(1-w_S)^2) ),
         # the probability of winning a match: for losers
         l_P = l_F1sp*l_F1swp + (1-l_F1sp)*l_S2swp,                 # Probability of winning a point when serving.
         l_W = (l_P^4)*(15-4*l_P-(10*l_P^2)/(1-2*l_P*(1-l_P))),              # The Probability of winning a game: W
         l_D = (l_P^2)*(1-2*l_P*(1-l_P)),                                # The Probability of winning from deuce: D
         l_TB = (l_P^7)*(1+7*(1-l_P)+28*(1-l_P)^2+84*(1-l_P)^3+210*(1-l_P)^4+462*(1-l_P)^5)+924*(l_P^6)*(1-l_P)^6*l_D,
         l_S = (l_W^6)*(1+6*(1-l_W)+21*(1-l_W)^2+56*(1-l_W)^3+126*(1-l_W)^4+252*l_W*(1-l_W)^5+504*(1-l_W)^6*l_TB), 
         l_M = ifelse(best_of == 3, l_S^2 *(1+2*(1-l_S)), l_S^3 * (1+3*(1-l_S)+ 6*(1-l_S)^2) )
  )

# 3- Select necessary stats for analysis, 
# then use pivot longer to make a new data frame of historical stats of each player, 
# i.e Combine winners and losers under same columns, denoted by another column results (w/L)

ATPm <- ATP2Seasons[,-c(5, 6, 9:11, 13:19, 21:28, 31:35, 40:44, 47:52, 69:73, 75:79)]

sum(is.na(ATPm))

ATPm <- na.omit(ATPm)
sum(is.na(ATPm))

# create Player1 data frame that contains winners data
Players1 <- ATPm[,c(1:7,9:12,17,19:22,27,28,31,33)]

# create Player2 data frame that contains winners data
Players0 <- ATPm[,c(1:6,8,13:16,18,23:26,29,30,32,34)]


Players1 <- Players1 %>%
  pivot_longer(-c(1:7), names_to = c("Status", "Stat"),
               names_sep = "_",
               values_to = "score") %>%
  pivot_wider(names_from=Stat, values_from=score)

Players0 <- Players0 %>%
  pivot_longer(-c(1:7), names_to = c("Status", "Stat"),
               names_sep = "_",
               values_to = "score") %>%
  pivot_wider(names_from=Stat, values_from=score)

# add wr for losers whose wr is not 0
Players0 <- Players0 %>%
  left_join( Players1 %>% select(winner_name, wr) ,by = c("loser_name" = "winner_name"))

Players0 <- unique(Players0)
# drop wr.x column, make NA value in wr.y = 0
Players0 <- subset(Players0, select = -c(wr.x))
Players0 <- Players0 %>% rename(wr = wr.y) 
Players0$wr <- ifelse(is.na(Players0$wr), 0, Players0$wr)

# drop wr in Player1, then add it at the end, to make the two data frames consistent 
winner_wr <- Players1$wr
Players1 <- subset(Players1, select = -c(wr))
Players1$wr <- winner_wr


# Take the mean values of Players1 and Players0 data frame

Players1 <- Players1 %>% 
  group_by(winner_name, surface, Status) %>%
  summarise(ace = mean(ace), df= mean(df), bpSaved= mean(bpSaved), bpFaced= mean(bpFaced), F1sp= mean(F1sp), S2sp= mean(S2sp), F1swp= mean(F1swp), 
            S2swp= mean(S2swp), F1sRpw= mean(F1sRpw), S2sRpw= mean(S2sRpw), Retpwp= mean(Retpwp), M= mean(M), wr= mean(wr))

Players1 <- Players1 %>%
  rename(Player_name = winner_name)

Players0 <- Players0 %>% 
  group_by(loser_name, surface, Status) %>%
  summarise(ace = mean(ace), df= mean(df), bpSaved= mean(bpSaved), bpFaced= mean(bpFaced), F1sp= mean(F1sp), S2sp= mean(S2sp), F1swp= mean(F1swp), 
            S2swp= mean(S2swp), F1sRpw= mean(F1sRpw), S2sRpw= mean(S2sRpw), Retpwp= mean(Retpwp), M= mean(M), wr= mean(wr))

Players0 <- Players0 %>%
  rename(Player_name = loser_name)


# Make a new data frame ATPmP, which combine Player1 and player0 using rbind() 
ATPmP <- rbind(Players1, Players0)
ATPmP %>% group_by(Player_name) 

# Make a history file for mean values of players from 2009 - 2019
#History_df1 <- rbind(Players1, Players0)
#History_df2 <- rbind(Players1, Players0)
#History_df3 <- rbind(Players1, Players0)
#write.csv(History_df,"C:\\Users\\sunde\\OneDrive\\Documents\\Strathclyde\\MS983 Dissertation\\My_project\\R\\History_df.csv", row.names = FALSE)

# 412 players, 1500 observations 
# 793 , 52,994

table(ATPmP$Status)
# there are 545 winning matches and 781 losing matches

################################################################################

# Check correlation:
library(corrplot)
CV <- cor(ATPmP[,4:ncol(ATPmP)])
col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))
corrplot(CV, method="color", col=col(200),  
         type="upper", order="hclust", 
         addCoef.col = "black",     # Add coefficient of correlation
         tl.col="black", tl.srt=45, #Text label color and rotation
         insig = "blank", 
         diag=FALSE,        # hide correlation coefficient on the principal diagonal
         tl.cex = 1.5
)

# In varaible selection for model building, remove 2nd S% as it has -1 correlation with the 1st S % (MultiCollinearity)
# Also remove  F1sRpw & S2sRpw as it has high correlation with Retpwp (0.87, 0.8)
# might cause misleading results

################################################################################
# Model Building:

# Adapted From Data Camp
# https://learn.datacamp.com/courses/supervised-learning-in-r-regression

# split ATPmP into a training set ATPmP_train (70% of the data) and a test set ATPmP_test (30% of the data). 
# 1 # get the number of rows in ATPmP
N <- nrow(ATPmP) #1050

# Calculate number of rows in 70% of N 
Nrows <- round(0.70*N) # 1326

# Create the vector of N uniform random variables: RV
RV <- runif(N)

# we will predict on the outcomes TRUE and FALSE. Create a supervised column winning in the ATPmP data frame that is TRUE when status == "w".

ATPmP$winning <- ATPmP$Status == "w"

# create the training set: ATPmP_train (70% of data) and ATPmP_test (30% of data)
ATPmP_train <- ATPmP[RV < 0.70,]  # l:545  , w:388 
ATPmP_test <- ATPmP[RV >= 0.70,]  # l:236  , w:157  

table(ATPmP_train$surface)

# ..............................................................................

# Variable Selection: Models based on features then make a comparison:

# 1 # mod1: All variables: surface, Ace per match, Double Fault Ace per match, First Serve %, First Serve Winning %, Second Serve Winning %,
#   Return Points Won, Break Point Faced per match, Break Points Saved Percentage, win ratio

# player_name, surface, Status, ace, df, bpSaved, bpFaced, F1sp, F1swp, S2swp, Retpwp, M, wr

# # Make surface as an indicator: Clay = ; Grass = ; Hard = 

model.matrix(winning ~ surface + ace + df + bpSaved+ bpFaced + F1sp + F1swp + S2swp + Retpwp + wr , data = ATPmP_train)

# # Fit the logistic regression model
mod1 <- glm(winning ~ surface + ace + df + bpSaved+ bpFaced + F1sp + F1swp + S2swp + Retpwp + wr , data = ATPmP_train, family = binomial)

# Evaluation:
#
ATPmP_test$pred <- predict(mod1, newdata = ATPmP_test, type = "response")
ggplot(ATPmP_test, aes(pred)) +
  geom_density()

summary(mod1)

# AIC: 122.88
# A lower AIC score is better.

# Pseudo-R on Training data

# Call glance
# install.packages("broom")
# install.packages("tidymodels")

library(broom)
(mod1_perf <- glance(mod1))

# pseudo-R2 on Training data
glance(mod1) %>%
  summarize(pR2 = 1 - deviance/null.deviance)
# 0.923

# The Gain Curve Plot

# Make predictions on test data
ATPmP_test$pred  <- predict(mod1,newdata=ATPmP_test, type="response")

# Look at gain curve
# Load the package WVPlots
library(WVPlots)
GainCurvePlot(ATPmP_test, "pred", "winning", "Winning Tennis Match Model 1")


## .............................................................................
# Adapted From 
# https://www.youtube.com/watch?v=C4N3_XJJ-jU

## calculate the overall "Pseudo R-squared" and its p-value
ll.null <- mod1$null.deviance/-2
ll.proposed <- mod1$deviance/-2

## McFadden's Pseudo R^2 = [ LL(Null) - LL(Proposed) ] / LL(Null)
(ll.null - ll.proposed) / ll.null

## chi-square value = 2*(LL(Proposed) - LL(Null))
## p-value = 1 - pchisq(chi-square value, df = 2-1)
1 - pchisq(2*(ll.proposed - ll.null), df=1)
1 - pchisq((mod1$null.deviance - mod1$deviance), df=1)

# ------------------------------------------------------------------------------

mod2 <- glm(winning ~  bpSaved + F1sp + F1swp + S2swp + Retpwp, data = ATPmP_train, family = binomial)
summary(mod2)
# remove non sig at mod1
# AIC: 120.6
(mod2_perf <- glance(mod2))

# pseudo-R2 on Training data
glance(mod2) %>%
  summarize(pR2 = 1 - deviance/null.deviance)
# 0.911

# Look at gain curve
# Load the package WVPlots
ATPmP_test$pred  <- predict(mod2,newdata=ATPmP_test, type="response")

GainCurvePlot(ATPmP_test, "pred", "winning", "Winning Tennis Match Model 2")

## .............................................................................
# Adapted From 
# https://www.youtube.com/watch?v=C4N3_XJJ-jU

## calculate the overall "Pseudo R-squared" and its p-value
ll.null <- mod2$null.deviance/-2
ll.proposed <- mod2$deviance/-2

## McFadden's Pseudo R^2 = [ LL(Null) - LL(Proposed) ] / LL(Null)
(ll.null - ll.proposed) / ll.null

## chi-square value = 2*(LL(Proposed) - LL(Null))
## p-value = 1 - pchisq(chi-square value, df = 2-1)
1 - pchisq(2*(ll.proposed - ll.null), df=1)
1 - pchisq((mod2$null.deviance - mod2$deviance), df=1)

## .............................................................................


# ------------------------------------------------------------------------------

# # mod3: Variables based on EDA: but exclude surface

# # Fit the logistic regression model

mod3 <- glm(winning ~ bpSaved + bpFaced+ F1swp + S2swp + Retpwp +wr, data = ATPmP_train, family = binomial) # (From EDA)

# Evaluation:
summary(mod3)
# AIC: 151.1

## 
ATPmP_test$pred <- predict(mod3, newdata = ATPmP_test, type = "response")
ggplot(ATPmP_test, aes(pred)) +
  geom_density()

# Pseudo-R on Training data

# Call glance
# install.packages("broom")
# install.packages("tidymodels")

(mod3_perf <- glance(mod3))

# pseudo-R2 on Training data
glance(mod3) %>%
  summarize(pR2 = 1 - deviance/null.deviance)
# 0.909

# The Gain Curve Plot

# Make predictions on test data
ATPmP_test$pred  <- predict(mod3,newdata=ATPmP_test, type="response")

# Look at gain curve

GainCurvePlot(ATPmP_test, "pred", "winning", "Winning Tennis Match Model 3")

par(mfrow=c(1,3))

## .............................................................................
# Adapted From 
# https://www.youtube.com/watch?v=C4N3_XJJ-jU

## NOTE: Since we are doing logistic regression...
## Null devaince = 2*(0 - LogLikelihood(null model))
##               = -2*LogLikihood(null model)
## Residual deviacne = 2*(0 - LogLikelihood(proposed model))
##                   = -2*LogLikelihood(proposed model)
ll.null <- mod3$null.deviance/-2
ll.proposed <- mod3$deviance/-2

## McFadden's Pseudo R^2 = [ LL(Null) - LL(Proposed) ] / LL(Null)
(ll.null - ll.proposed) / ll.null
# 0.903

## chi-square value = 2*(LL(Proposed) - LL(Null))
## p-value = 1 - pchisq(chi-square value, df = 2-1)
1 - pchisq(2*(ll.proposed - ll.null), df=1)
1 - pchisq((mod3$null.deviance - mod3$deviance), df=1)

# 0 , tinny

#...............................................................................

# 4# Mod4: Stepwise logistic regression:

library(MASS)
full_mod <- glm(winning ~., data = ATPmP_train, family = binomial)
mod4 <- full_mod %>% stepAIC(trace = FALSE)
# glm.fit: algorithm did not converge 
coef(mod4)
summary(mod4)

#.........................................................

# plot the regression model: Model1 is the best
# Adapted From 
# https://www.youtube.com/watch?v=C4N3_XJJ-jU

pred <- data.frame(probW=mod1$fitted.values,
                   Winning=ATPmP_train$winning)

pred <- pred[order(pred$probW, decreasing = FALSE),]
pred$rank <- 1:nrow(pred)

ggplot(data=pred, aes( x=rank, y=probW)) +
  geom_point(aes(color=Winning), alpha = 1, shape =4, stroke =2) +
  theme(text = element_text(size = 20)) +
  xlab("Index") +
  ylab("Predicted Probability of Winning a Match")


################################################################################

# Prediction Experment:
# Predicting on New Data
# bpSaved+ bpFaced + F1sp + F1swp + S2swp + Retpwp
# From 2019:
# 
History_df <- read.csv("History_df.csv")


as.data.frame(History_df1 %>% filter(Player_name  == "Andy Murray", tourney_date < 20191014) %>% lapply(mean, rm.na = TRUE)) -> d1
as.data.frame(History_df1 %>% filter(Player_name  == "Stan Wawrinka", tourney_date < 20191014) %>% lapply(mean, rm.na = TRUE)) ->d2

d1$surface <- "Clay"
d2$surface <- "Clay"

(d1$prediction <- predict(mod1, newdata = d1, type = "response"))
(d2$prediction <- predict(mod1, newdata = d2, type = "response"))


# Andy Murray: 0.98
# Stan Wawrinka: 0.82

# Andy:S M1: 0.9766579 - 0.9989612 - 0.9994018,  M2: 0.9697695 - 0.9983176 - 0.9990457, M3: 0.9854557 - 0.9989992 - 0.9995519   :  d: S - M - L
#     M  M1: 0.9966379 - 0.9998869 - 0.9999353,  M2: 0.9879388 - 0.9992754 - 0.9995921, M3: 0.9945536 - 0.9996335 - 0.9998319   :  d: S - M - L
#     L  M1: 0.999821  - 0.9999979 - 0.9999992,  M2:0.9919992 - 0.9996374 - 0.9998151 , M3: 0.9968653 - 0.999854 - 0.9999398 :    d: S - M - L

# Stan:S M1: 0.6564485 - 0.9209482 - 0.9533562,  M2:0.6726734 - 0.910624 - 0.9418675 , M3: 0.8523392 - 0.9735641 - 0.9826265   :  d: S - M - L
#     M  M1: 0.9321345 - 0.9907446 - 0.9939321,  M2:0.8603479 - 0.9694533 - 0.9790419, M3: 0.9301303 - 0.9888419 - 0.9924951  ::  d: S - M - L
#     L  M1: 0.9926242 - 0.9994728 - 0.9996572,  M2:0.8477037 - 0.9737025 - 0.9828901, M3: 0.9301963 - 0.9915262 - 0.9947458    ::  d: S - M - L

# ------------------------------------------------------------------------------

as.data.frame(History_df1 %>% filter(Player_name  == "Alexander Zverev", tourney_date < (20190527)) %>% lapply(mean, rm.na = TRUE)) -> d1
as.data.frame(History_df1 %>% filter(Player_name  == "Fabio Fognini", tourney_date < (20190527)) %>% lapply(mean, rm.na = TRUE)) ->d2

d1$surface <- "Clay"
d2$surface <- "Clay"

(d1$prediction <- predict(mod1, newdata = d1, type = "response"))
(d2$prediction <- predict(mod1, newdata = d2, type = "response"))
# 1 : 0.9967665 (0.9971499)
# 2 : 0.9666211 (0.9070816)

# ------------------------------------------------------------------------------

as.data.frame(History_df1 %>% filter(Player_name  == "Alexander Zverev", tourney_date < (20191007)) %>% lapply(mean, rm.na = TRUE)) -> d1
as.data.frame(History_df1 %>% filter(Player_name  == "Roger Federer", tourney_date < (20191007)) %>% lapply(mean, rm.na = TRUE)) ->d2

d1$surface <- "Clay"
d2$surface <- "Clay"

(d1$prediction <- predict(mod1, newdata = d1, type = "response"))
(d2$prediction <- predict(mod1, newdata = d2, type = "response"))
# 1 : 0.9953813 (0.9970239)
# 2 : 0.999962  (0.999952)

# ------------------------------------------------------------------------------

as.data.frame(History_df1 %>% filter(Player_name  == "Roberto Bautista Agut", tourney_date < (20190114)) %>% lapply(mean, rm.na = TRUE)) -> d1
as.data.frame(History_df1 %>% filter(Player_name  == "Andy Murray", tourney_date < (20190114)) %>% lapply(mean, rm.na = TRUE)) ->d2

d1$surface <- "Clay"
d2$surface <- "Clay"

(d1$prediction <- predict(mod1, newdata = d1, type = "response"))
(d2$prediction <- predict(mod1, newdata = d2, type = "response"))
# 1 : 0.9988287   (0.9971418)
# 2 : 0.9982765   (0.9928886)

# ------------------------------------------------------------------------------

as.data.frame(History_df1 %>% filter(Player_name  == "Roberto Bautista Agut", tourney_date < (20190114)) %>% lapply(mean, rm.na = TRUE)) -> d1
as.data.frame(History_df1 %>% filter(Player_name  == "Marin Cilic", tourney_date < (20190114)) %>% lapply(mean, rm.na = TRUE)) ->d2

d1$surface <- "Clay"
d2$surface <- "Clay"

(d1$prediction <- predict(mod1, newdata = d1, type = "response"))
(d2$prediction <- predict(mod1, newdata = d2, type = "response"))
# 1 : 0.9941804 # upset matches  0.9988287   (0.9971418)
# 2 : 0.9968603 0.9993043                    (0.9982355)

# ------------------------------------------------------------------------------

as.data.frame(History_df1 %>% filter(Player_name  == "Reilly Opelka", tourney_date < (20190114)) %>% lapply(mean, rm.na = TRUE)) -> d1
as.data.frame(History_df1 %>% filter(Player_name  == "John Isner", tourney_date < (20190114)) %>% lapply(mean, rm.na = TRUE)) ->d2

d1$surface <- "Clay"
d2$surface <- "Clay"

(d1$prediction <- predict(mod1, newdata = d1, type = "response"))
(d2$prediction <- predict(mod1, newdata = d2, type = "response"))
# 1 : 0.2383914  # upset matches 0.1280127       (0.1990491 )
# 2 : 0.6988267  # 0.6370258                     (0.9029819)

# ------------------------------------------------------------------------------
