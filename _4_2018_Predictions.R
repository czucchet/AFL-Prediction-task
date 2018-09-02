setwd("C:/Users/Chris Zucchet/Documents/AFL-Prediction-task")
library(DMwR);library(tidyverse);library(DBI);library(xgboost);library(recipes);library(rsample);library(RSQLite);library(purrr);library(e1071);library(digest);library(mlr);library(parallelMap);options(dplyr.width = Inf)
range01 <- function(x){(x-min(x))/(max(x)-min(x))}
con = dbConnect(SQLite(), "PlayerRecords.sqlite")
num_cols = c("KI","MK","HB","GL","BH","HO","TK","RB","IF","CL","CG","FF","FA","BR","CP","UP","CM","MI","one_pc","BO","GA","game_played","Year","Round","Diff","Age_Years","Games","PercentWon")
scale_cols = c("KI","MK","HB","GL","BH","HO","TK","RB","IF","CL","CG","FF","FA","CP","UP","CM","MI","one_pc","BO","GA","game_played","Age_Years","Games","PercentWon","dt_score","error_rate","fairness","impact_plays")
afl_model = xgb.load('afl_model')
afl_model = xgb.load('afl_model_pre2017')

records = dbGetQuery(con, "SELECT * FROM Player_Detail WHERE year = 2018") %>%group_by(Player,KI,MK,CP,Team,ID) %>%
  filter(row_number(HB) == 1) %>% ungroup() %>% rename(number = "#", one_pc = "1%", game_played = "%P") 
records[records == "Â"] <- "0" 
records2 = records 
records_t2 = records2 %>%  mutate_if(names(records) %in% num_cols, as.numeric)%>% na.omit() ;rm(records)

teams = unique(dbGetQuery(con, "SELECT Team, GL FROM Player_Detail"));teams = sort(unique(teams[,"Team"]));state = c("SA","QLD","VIC","VIC","VIC","WA","VIC","QLD","NSW","VIC","VIC","VIC","SA","VIC","VIC","NSW","WA","VIC")
home_team = data.frame(Team = teams,home_state = state) %>% mutate_if(is.factor, as.character);away_team = data.frame(Opponent = teams,away_state = state) %>% mutate_if(is.factor, as.character)

records_t3 = records_t2 %>%
  mutate(is_winner = ifelse(Diff >0,1,0),
         is_1_6_margin = ifelse(abs(Diff) <= 6,1,0),is_7_30_margin = ifelse(abs(Diff) > 6 & abs(Diff) <= 30,1,0),is_30_margin = ifelse(abs(Diff) >= 31,1,0),
         is_6_margin = ifelse(abs(Diff) <= 6,1,0),is_30_margin = ifelse(abs(Diff) <= 30,1,0),is_50_margin = ifelse(abs(Diff) <= 50,1,0),is_100_margin = ifelse(abs(Diff) <= 100,1,0),
         games_1_10 =  ifelse(Games <= 10, 1,0), games_0_50 = ifelse(Games <= 50, 1,0),         #games_51_more = ifelse(Games > 50, 1,0),
         dt_score = (KI*2.5)+(HB*1.5)+(MK*3)+(GL*6)+(BH*2)+(TK*2)+(HO*1.5)+(FF*1)+(FA*-3),
         error_rate = CG/(KI+HB),
         fairness = FF-FA,
         impact_plays = (IF*0.5)+(TK*0.25)+(MI*1)+(one_pc*3)+(BO*0.25)+(GA*1.5)+(RB*1)+(CG*-1)+(CL*1)+(CM*1),
         #         CP_score =(CP*1.5)+(UP*0.75),
         gl_dt =(GL*6)/((KI*2.5)+(HB*1.5)+(MK*3)+(GL*6)+(BH*2)+(TK*4)+(HO*1)+(FF*1)+(FA*-3))) %>%
  group_by(ID,Team) %>%
  mutate(GL_Sum = sum(GL),CG_Sum = sum(CG),MI_Sum = sum(MI)) %>%
  ungroup(ID) %>%
  mutate(GL_prop = GL/GL_Sum,CG_prop = CG/CG_Sum,MI_prop = MI/MI_Sum) %>% select(-GL_Sum,-CG_Sum,-MI_Sum)

records_t4 = records_t3 %>% left_join(home_team) %>% left_join(away_team);rm(records_t3);rm(home_team);rm(away_team)
records_t5 = records_t4 %>% mutate(interstate = ifelse(home_state != away_state, 1,0)) 


rec_split = records_t5 %>% split(.$ID)
for(i in 1:length(rec_split)){
  for(j in scale_cols){
    rec_split[[i]][,j] = range01(rec_split[[i]][,j])
  }
}
rec_all_temp = bind_rows(rec_split)
rec_all_temp$ID_Team = paste0(rec_all_temp$ID,"_",rec_all_temp$Team)
rec_split2 = rec_all_temp %>% split(.$ID_Team)
for(i in 1:length(rec_split2)){
  for(j in scale_cols){
    rec_split2[[i]][,j] = range01(rec_split2[[i]][,j])
  }
}
rec_all2 = bind_rows(rec_split2)
rec_all2 =  rec_all2 %>% select(scale_cols)
names(rec_all2) = paste0("Team","_", names(rec_all2))
rec_allt2 = cbind(rec_all_temp, rec_all2)
rec_all = rec_allt2 

rec_train =  recipe(BR ~., rec_all) %>%
  step_dummy(Team,Home_Away,Opponent, home_state, away_state) %>%
  step_other(Venue, threshold = .2) %>%
  step_dummy(Venue) %>%
  prep() %>%
  bake(newdata =  rec_all)

rec_lines_split = rec_train %>% split(.$ID)

for(i in 1:length(rec_lines_split)){
  rec_temp = rec_lines_split[[i]] %>% select(-number,-Player,-DI,-ID,-BR,-ID_Team)%>% data.frame()
  train_matrix <- xgb.DMatrix(data = as.matrix(rec_temp))
  afl_pred = round(predict(afl_model,train_matrix),6)
  vote_0_seq = seq(1,nrow(rec_lines_split[[i]])*4,4);vote_1_seq = seq(2,nrow(rec_lines_split[[i]])*4,4);vote_2_seq = seq(3,nrow(rec_lines_split[[i]])*4,4);vote_3_seq = seq(4,nrow(rec_lines_split[[i]])*4,4)
  vote_0 = afl_pred[vote_0_seq];vote_1 = afl_pred[vote_1_seq];vote_2 = afl_pred[vote_2_seq];vote_3 = afl_pred[vote_3_seq]
  rec_lines_split[[i]]$vote_3 = vote_3;rec_lines_split[[i]]$vote_2 = vote_2;rec_lines_split[[i]]$vote_1 = vote_1;rec_lines_split[[i]]$vote_0 = vote_0
}

Player_Team = records_t5 %>% select(Player,ID,Team) %>% unique()

afl_predict =  bind_rows(rec_lines_split) %>% mutate(Player = as.character(Player),number = as.character(number),ID = as.character(ID)) %>% data.frame() %>% left_join(Player_Team)

player_votes = afl_predict %>% select(ID, Player,Team, BR,vote_3,vote_2,vote_1,vote_0)  
player_votes$vote_3 = round(afl_predict$vote_3,6);player_votes$vote_2 = round(afl_predict$vote_2,6)
player_votes$vote_1 = round(afl_predict$vote_1,6);player_votes$vote_0 = round(afl_predict$vote_0,6)

player_votes_split = player_votes %>% split(.$ID)
for(i in 1:length(player_votes_split)){
  player_votes_split[[i]]$vote_3_pred = ifelse(max(player_votes_split[[i]]$vote_3) == player_votes_split[[i]]$vote_3,3,0)
  BR_3 = player_votes_split[[i]] %>% filter(vote_3_pred == 3) %>% select(ID,Player,Team,vote_3_pred)
  player_2_votes  = anti_join(player_votes_split[[i]],BR_3)
  player_2_votes$vote_2_pred = ifelse(max(player_2_votes$vote_2+player_2_votes$vote_3) == player_2_votes$vote_2+player_2_votes$vote_3,2,0)
  BR_2 = player_2_votes  %>% filter(vote_2_pred == 2) %>% select(ID,Player,Team,vote_2_pred)
  player_1_votes  = anti_join(player_votes_split[[i]],BR_2) %>% anti_join(BR_3)
  player_1_votes$vote_1_pred = ifelse(max(player_1_votes$vote_0) == player_1_votes$vote_1,1,0)
  BR_1 = player_1_votes  %>% filter(vote_1_pred == 1) %>% select(ID,Player,Team,vote_1_pred)
  player_votes_split[[i]] = left_join(player_votes_split[[i]],BR_3) %>% left_join(BR_2) %>% left_join(BR_1)
  player_votes_split[[i]][is.na(player_votes_split[[i]])] <- 0
  player_votes_split[[i]]$BR_Pred = player_votes_split[[i]]$vote_3_pred+player_votes_split[[i]]$vote_2_pred+player_votes_split[[i]]$vote_1_pred
  player_votes_split[[i]] = player_votes_split[[i]] %>% select(-vote_3_pred,-vote_2_pred,-vote_1_pred)
}
player_predict = bind_rows(player_votes_split)

player_predict %>% group_by(Player, Team) %>%
  summarise(BR_sum = sum(BR),BRPred_sum = sum(BR_Pred)) %>% arrange(desc(BRPred_sum)) %>% head(20)

records_votes = records_t5 %>% left_join(player_predict) %>% data.frame() %>% mutate(vote_diff = BR - BR_Pred, vote_sum = vote_3+vote_2+vote_1)

records_votes %>% filter(Player == "Mitchell, Tom" & Team == "Hawthorn") %>% filter(vote_diff != 0)
records_votes %>% filter(ID == "2018_1_20180324_1925_MCG") %>% arrange(desc(vote_sum)) %>% head(10)
