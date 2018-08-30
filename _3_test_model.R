setwd("C:/Users/Chris Zucchet/Documents/AFL-Prediction-task")
library(DMwR);library(tidyverse);library(DBI);library(xgboost);library(recipes);library(rsample);library(RSQLite);library(purrr);library(e1071);library(digest);library(mlr);library(parallelMap)
range01 <- function(x){(x-min(x))/(max(x)-min(x))}
con = dbConnect(SQLite(), "PlayerRecords.sqlite")
num_cols = c("KI","MK","HB","GL","BH","HO","TK","RB","IF","CL","CG","FF","FA","BR","CP","UP","CM","MI","one_pc","BO","GA","game_played","Year","Round","Diff","Age_Years","Games","PercentWon")
scale_cols = c("KI","MK","HB","GL","BH","HO","TK","RB","IF","CL","CG","FF","FA","CP","UP","CM","MI","one_pc","BO","GA","game_played","Age_Years","Games","PercentWon","dt_score","error_rate","fairness")
afl_model = xgb.load('afl_model')

records = dbGetQuery(con, "SELECT * FROM Player_Detail WHERE year = 2017") %>%group_by(Player,KI,MK,CP,Team,ID) %>% 
  filter(row_number(HB) == 1) %>% ungroup() %>% rename(number = "#", one_pc = "1%", game_played = "%P") %>%
  anti_join(records %>% group_by(ID) %>% summarise(BR_SUM = sum(as.numeric(BR))) %>%
              filter(BR_SUM != 6)) %>% na.omit()
records_t2 = records %>%  mutate_if(names(records) %in% num_cols, as.numeric)%>% na.omit() %>%select(-Â);rm(records)

teams = unique(dbGetQuery(con, "SELECT Team, GL FROM Player_Detail"));teams = sort(unique(teams[,"Team"]));state = c("SA","QLD","VIC","VIC","VIC","WA","VIC","QLD","NSW","VIC","VIC","VIC","SA","VIC","VIC","NSW","WA","VIC")
home_team = data.frame(Team = teams,home_state = state) %>% mutate_if(is.factor, as.character);away_team = data.frame(Opponent = teams,away_state = state) %>% mutate_if(is.factor, as.character)

records_t3 = records_t2 %>%
  mutate(is_winner = ifelse(Diff >0,1,0),
         games_1_10 =  ifelse(Games <= 10, 1,0),games_11_50 = ifelse(Games > 10 & Games <= 50, 1,0),games_51_more = ifelse(Games > 50, 1,0),
         dt_score = (KI*3)+(HB*2)+(MK*3)+(GL*6)+(BH*1)+(TK*3)+(HO*1)+(FF*1)+(FA*-3)+(GA*3),
         error_rate = CG/(KI+HB),
         fairness = FF-FA)
records_t4 = records_t3 %>% left_join(home_team) %>% left_join(away_team);rm(records_t3);rm(home_team);rm(away_team);rm(records_t2)
records_t5 = records_t4 %>% mutate(interstate = ifelse(home_state != away_state, 1,0));rm(records_t4) 

rec_split = records_t5 %>% split(.$ID)
for(i in 1:length(rec_split)){
  for(j in scale_cols){
    rec_split[[i]][,j] = range01(rec_split[[i]][,j])
    }
  }
rec_lines = bind_rows(rec_split)
rec_train =  recipe(rec_lines) %>%
  step_dummy(Team,Home_Away,Opponent, home_state, away_state) %>%
  step_other(Venue, threshold = .3) %>%
  step_dummy(Venue) %>%
  prep() %>%
  bake(newdata =  rec_lines) 

rec_lines_split = rec_train %>% split(.$ID)

for(i in 1:length(rec_lines_split)){
  rec_temp = rec_lines_split[[i]] %>% select(-number,-Player,-DI,-ID,-BR)%>% data.frame()
  train_matrix <- xgb.DMatrix(data = as.matrix(rec_temp))
  afl_pred = predict(afl_model,train_matrix)
  vote_0_seq = seq(1,length(rec_lines_split[[i]]$ID),4);vote_1_seq = seq(2,length(rec_lines_split[[i]]$ID),4);vote_2_seq = seq(3,length(rec_lines_split[[i]]$ID),4);vote_3_seq = seq(4,length(rec_lines_split[[i]]$ID),4)
  vote_0 = afl_pred[vote_0_seq];vote_1 = afl_pred[vote_1_seq];vote_2 = afl_pred[vote_2_seq];vote_3 = afl_pred[vote_3_seq]
  rec_lines_split[[i]]$vote_3 = vote_3;rec_lines_split[[i]]$vote_2 = vote_2;rec_lines_split[[i]]$vote_1 = vote_1;rec_lines_split[[i]]$vote_0 = vote_0
  }
  
  
votes_list = length(rec_lines_split) %>% rerun(votes_df)

for(i in 1:length(rec_lines_split)){
#  votes_list[[i]]$Player = rec_lines_split[[i]]$Player
#  votes_list[[i]]$ID = rec_lines_split[[i]]$ID  
#  vote_0_seq = seq(1,length(rec_lines_split[[i]]$ID),4);vote_1_seq = seq(2,length(rec_lines_split[[i]]$ID),4);vote_2_seq = seq(3,length(rec_lines_split[[i]]$ID),4);vote_3_seq = seq(4,length(rec_lines_split[[i]]$ID),4) 

    rec_train =  recipe(BR ~., rec_lines_split[[i]]) %>%
    select(-number,-Player,-DI) %>%
    step_dummy(Team,Home_Away,Opponent, home_state, away_state) %>%
    step_other(Venue, threshold = .2) %>%
    step_dummy(Venue) %>% 
    prep() %>%
    bake(newdata =  rec_all)
    train_matrix <- xgb.DMatrix(data = as.matrix(rec_train %>%select(-BR)))
    afl_pred = predict(afl_model,rec_train)
    votes_list[[i]]$vote_0 = afl_pred[vote_0_seq];votes_list[[i]]$vote_1 = afl_pred[vote_1_seq];votes_list[[i]]$vote_2 = afl_pred[vote_2_seq];votes_list[[i]]$vote_3 = afl_pred[vote_3_seq]

      }

votes_df = data.frame(vote_3,vote_2,vote_1,vote_0)
votes_list = length(rec_lines_split) %>% rerun(votes_df)

for(i in 1:length(votes_list)){
  vote_3 = rep(NA,nrow(rec_lines_split[[i]]));vote_2 = rep(NA,nrow(rec_lines_split[[i]]))
  vote_1 = rep(NA,nrow(rec_lines_split[[i]]));vote_0 = rep(NA,nrow(rec_lines_split[[i]]))
  
}

rec_all = bind_rows(rec_split) %>% select(-number,-Player,-DI,-ID);rm(rec_split)

rec_train =  recipe(BR ~., rec_all) %>%
  step_dummy(Team,Home_Away,Opponent, home_state, away_state) %>%
  step_other(Venue, threshold = .2) %>%
  step_dummy(Venue) %>% 
  prep() %>%
  bake(newdata =  rec_all)



afl_pred = predict(afl_model,train_matrix)

head(rec_lines)



