setwd("C:/Users/Chris Zucchet/Documents/AFL-Prediction-task")
library(DMwR);library(tidyverse);library(DBI);library(xgboost);library(recipes);library(rsample);library(RSQLite);library(purrr);library(e1071);library(digest);library(mlr);library(parallelMap);library(caret)


range01 <- function(x){(x-min(x))/(max(x)-min(x))}
con = dbConnect(SQLite(), "PlayerRecords.sqlite")
num_cols = c("KI","MK","HB","GL","BH","HO","TK","RB","IF","CL","CG","FF","FA","BR","CP","UP","CM","MI","one_pc","BO","GA","game_played","Year","Round","Diff","Age_Years","Games","PercentWon")
scale_cols = c("KI","MK","HB","GL","BH","HO","TK","RB","IF","CL","CG","FF","FA","CP","UP","CM","MI","one_pc","BO","GA","game_played","Age_Years","Games","PercentWon","dt_score","error_rate","fairness","impact_plays")

teams = unique(dbGetQuery(con, "SELECT Team, GL FROM Player_Detail"));teams = sort(unique(teams[,"Team"]));state = c("SA","QLD","VIC","VIC","VIC","WA","VIC","QLD","NSW","VIC","VIC","VIC","SA","VIC","VIC","NSW","WA","VIC");Teams_States = data.frame(Team = teams,state) %>% mutate_if(is.factor, as.character)


records = dbGetQuery(con, "SELECT * FROM Player_Detail") %>%  mutate( );
records[records == "Â"] <- 0
records_t = records %>%  mutate_if(names(records) %in% num_cols, as.numeric) %>% mutate(dt_score = (KI*3) + (HB*2) + (MK *3) + (GL*6) + (HB *2) + (HO*1) + (TK*4) + (FF*1) - (FA*-3) )
records_t2 = records_t %>% arrange(Games) %>%  split(.$Player)

for(i in 1:length(records_t2)) {
  records_t2[[i]]$BR_cumsum = cumsum(records_t2[[i]]$BR)
  records_t2[[i]]$PG_BR_sum = lag(records_t2[[i]]$BR_cumsum, 1)
  records_t2[[i]]$avg_5_clr = round(ifelse(records_t2[[i]]$Games == 1, 0,ifelse(records_t2[[i]]$Games == 2, lag(records_t2[[i]]$CL, 1),
    ifelse(records_t2[[i]]$Games == 3, ((lag(records_t2[[i]]$CL, 1) + lag(records_t2[[i]]$CL, 2))/2) ,    
    ifelse(records_t2[[i]]$Games == 4, ((lag(records_t2[[i]]$CL, 1) + lag(records_t2[[i]]$CL, 2)+lag(records_t2[[i]]$CL, 3))/3),
    ifelse(records_t2[[i]]$Games == 5, ((lag(records_t2[[i]]$CL, 1) + lag(records_t2[[i]]$CL, 2)+lag(records_t2[[i]]$CL, 3)+lag(records_t2[[i]]$CL, 4))/4),
    ifelse(records_t2[[i]]$Games >= 6, ((lag(records_t2[[i]]$CL, 1) + lag(records_t2[[i]]$CL, 2)+lag(records_t2[[i]]$CL, 3)+lag(records_t2[[i]]$CL, 4)+lag(records_t2[[i]]$CL, 5))/5) , 0 )))))),2)
  records_t2[[i]]$avg_5_cp = round(ifelse(records_t2[[i]]$Games == 1, 0,ifelse(records_t2[[i]]$Games == 2, lag(records_t2[[i]]$CP, 1),
    ifelse(records_t2[[i]]$Games == 3, ((lag(records_t2[[i]]$CP, 1) + lag(records_t2[[i]]$CP, 2))/2) ,    
    ifelse(records_t2[[i]]$Games == 4, ((lag(records_t2[[i]]$CP, 1) + lag(records_t2[[i]]$CP, 2)+lag(records_t2[[i]]$CP, 3))/3),
    ifelse(records_t2[[i]]$Games == 5, ((lag(records_t2[[i]]$CP, 1) + lag(records_t2[[i]]$CP, 2)+lag(records_t2[[i]]$CP, 3)+lag(records_t2[[i]]$CP, 4))/4),
    ifelse(records_t2[[i]]$Games >= 6, ((lag(records_t2[[i]]$CP, 1) + lag(records_t2[[i]]$CP, 2)+lag(records_t2[[i]]$CP, 3)+lag(records_t2[[i]]$CP, 4)+lag(records_t2[[i]]$CP, 5))/5) , 0 )))))),2)
    records_t2[[i]]$avg_5_dt_score = round(ifelse(records_t2[[i]]$Games == 1, 0,ifelse(records_t2[[i]]$Games == 2, lag(records_t2[[i]]$dt_score, 1),
    ifelse(records_t2[[i]]$Games == 3, ((lag(records_t2[[i]]$dt_score, 1) + lag(records_t2[[i]]$dt_score, 2))/2) ,    
    ifelse(records_t2[[i]]$Games == 4, ((lag(records_t2[[i]]$dt_score, 1) + lag(records_t2[[i]]$dt_score, 2)+lag(records_t2[[i]]$dt_score, 3))/3),
    ifelse(records_t2[[i]]$Games == 5, ((lag(records_t2[[i]]$dt_score, 1) + lag(records_t2[[i]]$dt_score, 2)+lag(records_t2[[i]]$dt_score, 3)+lag(records_t2[[i]]$dt_score, 4))/4),
    ifelse(records_t2[[i]]$Games >= 6, ((lag(records_t2[[i]]$dt_score, 1) + lag(records_t2[[i]]$dt_score, 2)+lag(records_t2[[i]]$dt_score, 3)+lag(records_t2[[i]]$dt_score, 4)+lag(records_t2[[i]]$dt_score, 5))/5) , 0 )))))),2)
    records_t2[[i]]$avg_5_cg = round(ifelse(records_t2[[i]]$Games == 1, 0,ifelse(records_t2[[i]]$Games == 2, lag(records_t2[[i]]$CG, 1),
    ifelse(records_t2[[i]]$Games == 3, ((lag(records_t2[[i]]$CG, 1) + lag(records_t2[[i]]$CG, 2))/2) ,    
    ifelse(records_t2[[i]]$Games == 4, ((lag(records_t2[[i]]$CG, 1) + lag(records_t2[[i]]$CG, 2)+lag(records_t2[[i]]$CG, 3))/3),
    ifelse(records_t2[[i]]$Games == 5, ((lag(records_t2[[i]]$CG, 1) + lag(records_t2[[i]]$CG, 2)+lag(records_t2[[i]]$CG, 3)+lag(records_t2[[i]]$CG, 4))/4),
    ifelse(records_t2[[i]]$Games >= 6, ((lag(records_t2[[i]]$CG, 1) + lag(records_t2[[i]]$CG, 2)+lag(records_t2[[i]]$CG, 3)+lag(records_t2[[i]]$CG, 4)+lag(records_t2[[i]]$CG, 5))/5) , 0 )))))),2)
    records_t2[[i]]$avg_5_if = round(ifelse(records_t2[[i]]$Games == 1, 0,ifelse(records_t2[[i]]$Games == 2, lag(records_t2[[i]]$CG, 1),
    ifelse(records_t2[[i]]$Games == 3, ((lag(records_t2[[i]]$IF, 1) + lag(records_t2[[i]]$IF, 2))/2) ,    
    ifelse(records_t2[[i]]$Games == 4, ((lag(records_t2[[i]]$IF, 1) + lag(records_t2[[i]]$IF, 2)+lag(records_t2[[i]]$IF, 3))/3),
    ifelse(records_t2[[i]]$Games == 5, ((lag(records_t2[[i]]$IF, 1) + lag(records_t2[[i]]$IF, 2)+lag(records_t2[[i]]$IF, 3)+lag(records_t2[[i]]$IF, 4))/4),
    ifelse(records_t2[[i]]$Games >= 6, ((lag(records_t2[[i]]$IF, 1) + lag(records_t2[[i]]$IF, 2)+lag(records_t2[[i]]$IF, 3)+lag(records_t2[[i]]$IF, 4)+lag(records_t2[[i]]$IF, 5))/5) , 0 )))))),2)
    
    records_t2[[i]]$last_5_games = round(ifelse(records_t2[[i]]$Games == 1, 0,
    ifelse(records_t2[[i]]$Games == 2, ifelse(lag(records_t2[[i]]$Diff, 1) >0 ,1,0),
    ifelse(records_t2[[i]]$Games == 3, (ifelse(lag(records_t2[[i]]$Diff, 1) > 0 ,1,0) + ifelse(lag(records_t2[[i]]$Diff, 2) > 0 ,1,0)),#/2
    ifelse(records_t2[[i]]$Games == 4, ((ifelse(lag(records_t2[[i]]$Diff, 1) > 0 ,1,0) + ifelse(lag(records_t2[[i]]$Diff, 2) > 0 ,1,0) + ifelse(lag(records_t2[[i]]$Diff, 3) > 0 ,1,0)) ),#/3
    ifelse(records_t2[[i]]$Games == 5, ((ifelse(lag(records_t2[[i]]$Diff, 1) > 0 ,1,0) + ifelse(lag(records_t2[[i]]$Diff, 2) > 0 ,1,0) + ifelse(lag(records_t2[[i]]$Diff, 3) > 0 ,1,0)+ ifelse(lag(records_t2[[i]]$Diff, 4) > 0 ,1,0)) ),#/4 
    ifelse(records_t2[[i]]$Games == 6, ((ifelse(lag(records_t2[[i]]$Diff, 1) > 0 ,1,0) + ifelse(lag(records_t2[[i]]$Diff, 2) > 0 ,1,0) + ifelse(lag(records_t2[[i]]$Diff, 3) > 0 ,1,0)+ ifelse(lag(records_t2[[i]]$Diff, 4) > 0 ,1,0)+ 
        ifelse(lag(records_t2[[i]]$Diff, 5) > 0 ,1,0)) ),
    ifelse(records_t2[[i]]$Games == 7, ((ifelse(lag(records_t2[[i]]$Diff, 1) > 0 ,1,0) + ifelse(lag(records_t2[[i]]$Diff, 2) > 0 ,1,0) + ifelse(lag(records_t2[[i]]$Diff, 3) > 0 ,1,0)+ ifelse(lag(records_t2[[i]]$Diff, 4) > 0 ,1,0)+ 
        ifelse(lag(records_t2[[i]]$Diff, 5) > 0 ,1,0)+ifelse(lag(records_t2[[i]]$Diff, 6) > 0 ,1,0) ) ),
    ifelse(records_t2[[i]]$Games == 8, ((ifelse(lag(records_t2[[i]]$Diff, 1) > 0 ,1,0) + ifelse(lag(records_t2[[i]]$Diff, 2) > 0 ,1,0) + ifelse(lag(records_t2[[i]]$Diff, 3) > 0 ,1,0)+ ifelse(lag(records_t2[[i]]$Diff, 4) > 0 ,1,0)+ 
        ifelse(lag(records_t2[[i]]$Diff, 5) > 0 ,1,0)+ifelse(lag(records_t2[[i]]$Diff, 6) > 0 ,1,0)+ifelse(lag(records_t2[[i]]$Diff, 7) > 0 ,1,0) ) ), 0 )))))))),2) #/5
    }



#Games
game_results = records_t %>% mutate(game_key = paste0(ID,"_",Team), team_year = paste0(Team,"_", Year)) %>% select(game_key, Round, Diff, Home_Away,ID,Year,team_year, Team) %>% distinct()
#Checks
players = records_t %>% mutate(game_key = paste0(ID,"_",Team)) %>% group_by(game_key) %>% summarise(n_players = n(), avg_diff = mean(Diff)) %>% data.frame()
games_check = records_t  %>% group_by(ID) %>% summarise(diff_sum = sum(Diff)) %>% data.frame()
games_check %>% filter(diff_sum != 0)
games = left_join(game_results,players) %>% anti_join(games_check %>% filter(diff_sum != 0)) %>% filter(Diff == avg_diff) %>%  select(-n_players, -avg_diff) %>% arrange(Year, Round) 

games_t2 = games %>% split(.$team_year)
for(i in 1:length(games_t2)){
  games_t2[[i]]$points           = ifelse(games_t2[[i]]$Diff < 0,0,  ifelse(games_t2[[i]]$Diff > 0, 4,2))
  games_t2[[i]]$cum_points       = cumsum(games_t2[[i]]$points);games_t2[[i]]$last_rd_points   = lag(games_t2[[i]]$cum_points , 1)
  }
games_t3 = games_t2 %>% bind_rows() %>% select(-points, -cum_points) %>%  left_join(Teams_States);games_t3[is.na(games_t3)] <- 0 
head(games_t3)

games_home = games_t3 %>% filter(Home_Away == "Home") %>% select(ID, last_rd_points, Diff, Round, Year,state) %>% rename(Home_Points = last_rd_points,Home_State = state)
games_away = games_t3 %>% filter(Home_Away == "Away") %>% select(ID, last_rd_points,state) %>% rename(Away_Points = last_rd_points,Away_State = state)
  
games_t4 = left_join(games_home,games_away) 

records_t3 = records_t2 %>% bind_rows() %>% mutate(game_key = paste0(ID,"_",Team)) %>% anti_join(games_check %>% filter(diff_sum != 0)) ;records_t3[is.na(records_t3)] <- 0
head(records_t3);sum(is.na(records_t3))
records_t4 = records_t3  %>% group_by(ID,Home_Away) %>% summarise(avg_games = round(mean(Games),2), sum_clr = sum(avg_5_clr),
          sum_cp = sum(avg_5_cp),sum_dt = sum(avg_5_dt_score), sum_PG_BR = sum(PG_BR_sum)/sum(Games) , sum_cg = sum(avg_5_cg), sum_win_5_games = sum(last_5_games)) %>% data.frame() 

records_home = records_t4 %>% filter(Home_Away == "Home") %>% rename(home_avg_games = avg_games,home_sum_clr = sum_clr,home_sum_cp = sum_cp,home_sum_dt = sum_dt, home_pg_br = sum_PG_BR,home_sum_cg = sum_cg, home_last_5_games = sum_win_5_games) %>% select(-Home_Away)
records_away = records_t4 %>% filter(Home_Away == "Away") %>% rename(away_avg_games = avg_games,away_sum_clr = sum_clr,away_sum_cp = sum_cp,away_sum_dt = sum_dt, away_pg_br = sum_PG_BR,away_sum_cg = sum_cg, away_last_5_games = sum_win_5_games) %>% select(-Home_Away)

games_t5 = left_join(games_t4,records_home)  %>% left_join(records_away) %>% 
  mutate(avg_game_diff = home_avg_games-away_avg_games,sum_clr_diff  = home_sum_clr - away_sum_clr,sum_cp_diff  = home_sum_cp - away_sum_cp,sum_dt_diff  = home_sum_dt - away_sum_dt, 
    pg_sum_br_diff = round((home_pg_br - away_pg_br),2), cg_diff = home_sum_cg - away_sum_cg, last_5_games_diff = home_last_5_games - away_last_5_games,
    points_diff = (Home_Points - Away_Points)/Round,
    Is_Interstate = ifelse(Home_State != Away_State, 1,0)) %>% 
  select(-home_avg_games,-away_avg_games,-home_sum_clr,-away_sum_clr,-home_sum_cp,-away_sum_cp,-home_sum_dt,-away_sum_dt,-home_pg_br, -away_pg_br,-home_sum_cg, -away_sum_cg,-home_last_5_games, -away_last_5_games)

temp = games_t5 %>% filter(Year >= 2010)

game_testing = games_t5 %>% filter(Year >= 2005) %>% select(ID, Diff, Home_Points,Away_Points, points_diff,avg_game_diff, sum_clr_diff, sum_cp_diff, sum_dt_diff, pg_sum_br_diff,cg_diff,last_5_games_diff, Home_State, Away_State, Year, Round,Is_Interstate)
game_testing = game_testing %>% filter(Round >= 7)


corr = game_testing %>% select(Diff, avg_game_diff, sum_clr_diff, sum_cp_diff, pg_sum_br_diff,cg_diff,last_5_games_diff,sum_dt_diff,points_diff,Is_Interstate)
round(cor(corr, use = "complete.obs"),4) %>% data.frame()

lm_basic = lm(Diff ~avg_game_diff+sum_dt_diff+pg_sum_br_diff+points_diff, game_testing )
summary(lm_basic)
game_pred = game_testing %>% select(avg_game_diff,sum_dt_diff,pg_sum_br_diff,points_diff)
lm_pred = predict(lm_basic,game_pred)
game_testing$pred = round(lm_pred,1)

write.csv(game_testing,"game_testing_pred.csv", row.names = F)

test_rows = sample(nrow(game_testing), 10) 
game_testing[test_rows,]

game_testing %>% filter(Year == 2019)


records = records_t2 %>% bind_rows()
write.csv(records,"afl_game_records.csv", row.names = F)
write.csv(game_testing,"game_testing.csv", row.names = F)
test = records %>%  filter(ID == "2009_5_20090426_1310_Skilled Stadium" & Team == "Geelong") 
sum(test$BR_cumsum)


