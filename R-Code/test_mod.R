ATPm <- ATPm %>% filter(tourney_date < 20191014)

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
  mutate(ace = mean(ace), df= mean(df), bpSaved= mean(bpSaved), bpFaced= mean(bpFaced), F1sp= mean(F1sp), S2sp= mean(S2sp), F1swp= mean(F1swp), 
            S2swp= mean(S2swp), F1sRpw= mean(F1sRpw), S2sRpw= mean(S2sRpw), Retpwp= mean(Retpwp), M= mean(M), wr= mean(wr))

Players1 <- Players1 %>%
  rename(Player_name = winner_name)

Players0 <- Players0 %>% 
  group_by(loser_name, surface, Status) %>%
  mutate(ace = mean(ace), df= mean(df), bpSaved= mean(bpSaved), bpFaced= mean(bpFaced), F1sp= mean(F1sp), S2sp= mean(S2sp), F1swp= mean(F1swp), 
            S2swp= mean(S2swp), F1sRpw= mean(F1sRpw), S2sRpw= mean(S2sRpw), Retpwp= mean(Retpwp), M= mean(M), wr= mean(wr))

Players0 <- Players0 %>%
  rename(Player_name = loser_name)

# Create the survived column: make the Status = 1 for Player1 , Status = 0 for Player0
#Players1$Outcome <- 1   # Winning TRUE
#Players0$Outcome <- 0   # Winning FALSE

# Make a new data frame ATPmP, which combine Player1 and player0 using rbind() 
ATPmP <- rbind(Players1, Players0)
