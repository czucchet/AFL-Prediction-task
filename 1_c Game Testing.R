library(h2o);h2o.init()## Initial ML apporach that splits into training and test set ##
library(xgboost)

## 1) Import data
teams = unique(dbGetQuery(con, "SELECT Team, GL FROM Player_Detail"));teams = sort(unique(teams[,"Team"]));state = c("SA","QLD","VIC","VIC","VIC","WA","VIC","QLD","NSW","VIC","VIC","VIC","SA","VIC","VIC","NSW","WA","VIC");Teams_States = data.frame(Team = teams,state) %>% mutate_if(is.factor, as.character)
records = dbGetQuery(con, "SELECT * FROM Player_Detail");records[records == "Â"] <- 0
records_t = records %>% filter(Year >= 2010) %>%  mutate_if(names(records) %in% num_cols, as.numeric) %>% mutate(dt_score = (KI*3) + (HB*2) + (MK *3) + (GL*6) + (HB *2) + (HO*1) + (TK*4) + (FF*1) - (FA*-3) ) %>% 
            mutate(Game_ID = ifelse(Round <= 9, substr(ID,8,100),substr(ID,9,100)))  

#Games
game_results = records_t %>% mutate(game_key = paste0(ID,"_",Team), team_year = paste0(Team,"_", Year)) %>% select(game_key, Round, Diff, Home_Away,ID,Year,team_year, Team,Game_ID) %>% distinct()
#Checks
players = records_t %>% mutate(game_key = paste0(ID,"_",Team)) %>% group_by(game_key) %>% summarise(n_players = n(), avg_diff = mean(Diff)) %>% data.frame()
games_check = records_t  %>% group_by(ID) %>% summarise(diff_sum = sum(Diff)) %>% data.frame()
games_check %>% filter(diff_sum != 0)
games = left_join(game_results,players) %>% anti_join(games_check %>% filter(diff_sum != 0)) %>% filter(Diff == avg_diff) %>%  select(-n_players, -avg_diff) %>% arrange(Year, Round) 

games_t2 = games %>% split(.$team_year)
for(i in 1:length(games_t2)){
  games_t2[[i]]$points           = ifelse(games_t2[[i]]$Diff < 0,0,  ifelse(games_t2[[i]]$Diff > 0, 4,2))
  games_t2[[i]]$cum_points       = cumsum(games_t2[[i]]$points);games_t2[[i]]$last_rd_points   = lag(games_t2[[i]]$cum_points , 1)
  games_t2[[i]]$last_3_games     = ifelse(games_t2[[i]]$Round >= 4, lag(games_t2[[i]]$points,1)+lag(games_t2[[i]]$points,2)+lag(games_t2[[i]]$points,3), 0)
}
games_t2a = bind_rows(games_t2)
games_t2b = games_t2a %>% mutate(year_rd = paste0(Year, "_", Round)) %>% split(.$year_rd)
for(i in 1:length(games_t2b)){
  games_t2b[[i]]$ladder_rk = rank(games_t2b[[i]]$cum_points,ties.method= "min")
    }
games_t2c = games_t2b %>% bind_rows()
games_t2d = games_t2c %>% split(.$team_year)
for(i in 1:length(games_t2d)){
  games_t2d[[i]]$pr_ladder_rank           = lag(games_t2d[[i]]$ladder_rk,1)
  }

games_t3 = games_t2d %>% bind_rows() %>% select(-points, -cum_points) %>%  left_join(Teams_States) ;games_t3[is.na(games_t3)] <- 0 ;head(games_t3)
games_home = games_t3 %>% filter(Home_Away == "Home") %>% select(ID, last_rd_points, Diff, Round, Year,state,Game_ID,Team,last_3_games,pr_ladder_rank) %>% 
  rename(Home_Points = last_rd_points,Home_State = state,Home_Team = Team,Home_last_3_games = last_3_games, home_ladder_rk = pr_ladder_rank)
games_away = games_t3 %>% filter(Home_Away == "Away") %>% select(ID, last_rd_points,state,Team,last_3_games,pr_ladder_rank) %>% 
  rename(Away_Points = last_rd_points,Away_State = state,Away_Team = Team,Away_last_3_games = last_3_games, away_ladder_rk = pr_ladder_rank)

games_t4 = left_join(games_home,games_away)  %>% select(-ID) %>% 
  mutate(Home_finals_race = ifelse(Home_Points > (Round*2), 1,0),Away_finals_race = ifelse(Away_Points > (Round*2), 1,0),
          Is_Interstate = ifelse(Home_State != Away_State,1,0), Is_WA = ifelse(Home_State == "WA", 1,0),stadium = substr(Game_ID,15,60))

games_t4$stadium[games_t4$stadium == "Etihad Stadium"] <- "Marvel Stadium";games_t4$stadium[games_t4$stadium == "Subiaco"] <- "Domain Stadium"
games_t4$stadium[games_t4$stadium == "Patersons Stadium"] <- "Domain Stadium";games_t4$stadium[games_t4$stadium == "Skilled Stadium"] <- "GMHBA Stadium";games_t4$stadium[games_t4$stadium == "Simonds Stadium"] <- "GMHBA Stadium"
games_t4$stadium[games_t4$stadium == "Aurora Stadium"] <- "University of Tasmania Stadium";games_t4$stadium[games_t4$stadium == "Skoda"] <- "Giants Stadium";games_t4$stadium[games_t4$stadium == "Sydney Showground Stadium"] <- "Giants Stadium";games_t4$stadium[games_t4$stadium == "Manuka Oval"] <- "UNSW Canberra Oval"
games_t4$stadium[games_t4$stadium == "Spotless Stadium"] <- "Giants Stadium";games_t4$stadium[games_t4$stadium == "StarTrack Oval, Canberra"] <- "UNSW Canberra Oval";games_t4$stadium[games_t4$stadium == "StarTrack Oval"] <- "UNSW Canberra Oval";games_t4$stadium[games_t4$stadium == "Cazalys Stadium"] <- "Cazaly's Stadium";games_t4$stadium[games_t4$stadium == "TIO Traeger Park"] <- "Traeger Park"

games_t4b = games_t4 %>% mutate(Game_ID_New =  paste0(substr(Game_ID,1,8),"_", stadium)) %>% select(-Game_ID)

betting_odds_t = read.csv("betting_odds.csv") %>% data.frame() %>% select(Home.Odds,Away.Odds, Home.Line.Close,Home.Line.Odds.Close,Game.Key) %>% rename(Game_ID = Game.Key) %>% 
  mutate(date = substr(Game_ID,1,8), time = substr(Game_ID,10,13), stadium = substr(Game_ID,15,50),Game_ID_New =  paste0(date,"_", stadium)
    ) %>% select(-date, -time, -Game_ID, -stadium)
head(betting_odds_t)

games_t5 = games_t4b %>% left_join(betting_odds_t) 

games_t6a = games_t5  %>%  na.omit(games_t5)
quantile(games_t6a$Diff, probs = seq(0, 1, by= 0.01))

games_t6b_all = games_t6a %>% mutate(Home_Line = ifelse(Home.Line.Close + Diff > 0, 1,0)) %>% filter(Diff < 34 & Diff > -23)
games_t6b = games_t6a %>% mutate(Home_Line = ifelse(Home.Line.Close + Diff > 0, 1,0)) %>% filter(Diff < 34 & Diff > -23) %>% 
  select(-Home.Line.Odds.Close,-stadium,-Game_ID_New,-Home_Team,-Away_Team, -Home_State, -Away_State,-Diff
    ,-home_ladder_rk,-away_ladder_rk) %>% filter(Round > 1) %>%  #%>%  
#     ) %>% # filter(Round >= 4) %>%     
  mutate(Home_Line = as.factor(Home_Line))


set.seed(456)
samp = sample(1:nrow(games_t6b),round(nrow(games_t6b)*.5,0))
train_samp = games_t6b[samp,];train_samp_val = games_t6b[samp,] %>% select(-Home_Line)
test_samp = games_t6b[-samp,] %>% select(-Home_Line);test_y = games_t6b[-samp,] %>% select(Home_Line);test_all = games_t6b[-samp,] 
pl_hex <- as.h2o(train_samp);pl_hex_test <- as.h2o(test_samp);pl_hex_test_val <- as.h2o(test_y);pl_train = as.h2o(games_t6b)
h2o.describe(pl_hex)
games_t6b

test <- h2o.automl(y = "Home_Line", training_frame = pl_hex,balance_classes = T, max_runtime_secs = 45, #Comment  to to run all the way
                   stopping_metric = "AUTO",sort_metric = "AUTO", seed = 123,include_algos = c( "GBM","StackedEnsemble","DRF","GLM","DeepLearning"))

h2o_pred = h2o.predict(test,pl_hex_test);h2o_pred_df = as.data.frame(h2o_pred)

test_samp$pred = round(h2o_pred_df$p1,3)
test_samp$actual = as.vector(pl_hex_test_val)
test_samp$Diff = games_t6b[-samp, "Diff"]
test_samp$odds = 1.91;bet_amt = 10
test_samp$return = round(ifelse(test_samp$actual == 1,  bet_amt*test_samp$pred*test_samp$odds - 1, bet_amt* (1-test_samp$pred)*test_samp$odds - 1),3)
test_samp$game_odds = ifelse(test_samp$Home.Line.Close < 0 & test_samp$pred > .5, 1*test_samp$Home.Odds * ifelse(test_samp$Diff > 0,1,0) - 1,
                        ifelse(test_samp$Home.Line.Close > 0 & test_samp$pred < .5, 1*test_samp$Away.Odds * ifelse(test_samp$Diff < 0,1,0) - 1,0) )

sum(test_samp$return) #1974.607 w bet amt 10,not R1 and duration 30 secs & 
sum(test_samp$game_odds)

#Review metrics
table(test_samp$actual, test_samp$pred)

pred_results = table(all_pred_lines$Pred_line, all_pred_lines$Act_line)
(pred_results[1,1]+pred_results[2,2])/sum(pred_results)

test_strat = all_pred_lines %>% filter(abs(Line_Diff) > 10) %>%  head(20)

test_table = table(test_strat$Pred_line, test_strat$Act_line)
(test_table[1,1]+test_table[2,2])/sum(test_table)

write.csv(all_pred_lines,"all_pred_lines.csv")








# Model results
#basic model - RMSE 35 and line prediction of 49%
#remove large game difference in excess of 80 points 




