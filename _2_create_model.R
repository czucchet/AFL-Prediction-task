setwd("C:/Users/Chris Zucchet/Documents/AFL-Prediction-task")
library(tidyverse);library(DBI);library(xgboost);library(recipes);library(rsample);library(RSQLite);library(purrr);library(e1071);library(DMwR)
range01 <- function(x){(x-min(x))/(max(x)-min(x))}
con = dbConnect(SQLite(), "PlayerRecords.sqlite")
num_cols = c("KI","MK","HB","GL","BH","HO","TK","RB","IF","CL","CG","FF","FA","BR","CP","UP","CM","MI","one_pc","BO","GA","game_played","Year","Round","Diff","Age_Years","Games","PercentWon")
scale_cols = c("KI","MK","HB","GL","BH","HO","TK","RB","IF","CL","CG","FF","FA","CP","UP","CM","MI","one_pc","BO","GA","game_played","Age_Years","Games","PercentWon","dt_score","error_rate","fairness")

records = unique(dbGetQuery(con, "SELECT * FROM Player_Detail")) %>% na.omit()%>% 
  rename(number = "#", one_pc = "1%", game_played = "%P") %>%  replace("Â",0) 
records_t2 = records %>%  mutate_if(names(records) %in% num_cols, as.numeric)%>% na.omit() %>%select(-Â);rm(records)

teams = unique(dbGetQuery(con, "SELECT Team, GL FROM Player_Detail"));teams = sort(unique(teams[,"Team"]));state = c("SA","QLD","VIC","VIC","VIC","WA","VIC","QLD","NSW","VIC","VIC","VIC","SA","VIC","VIC","NSW","WA","VIC")
home_team = data.frame(Team = teams,home_state = state) %>% mutate_if(is.factor, as.character);away_team = data.frame(Opponent = teams,away_state = state) %>% mutate_if(is.factor, as.character)

records_t3 = records_t2 %>%
  mutate(is_winner = ifelse(Diff >0,1,0),
         games_1_10 =  ifelse(Games <= 10, 1,0),games_11_50 = ifelse(Games > 10 & Games <= 50, 1,0),games_51_more = ifelse(Games > 50, 1,0),
         dt_score = (KI*3)+(HB*2)+(MK*3)+(GL*6)+(BH*1)+(TK*3)+(HO*1)+(FF*1)+(FA*-3)+(GA*3),
         error_rate = CG/(KI+HB),
         fairness = FF-FA);rm(records_t2);rm(home_team);rm(away_team)
records_t4 = records_t3 %>% left_join(home_team) %>% left_join(away_team);rm(records_t3)
records_t5 = records_t4 %>% mutate(interstate = ifelse(home_state != away_state, 1,0));rm(records_t4) 

rec_split = records_t5 %>% split(.$ID)
for(i in 1:length(rec_split)){
  for(j in scale_cols){
    rec_split[[i]][,j] = range01(rec_split[[i]][,j])
    }
  }
rec_all = bind_rows(rec_split) %>% select(-number,-Player,-DI,-ID);rm(rec_split)
br_0_1 = rec_all %>% filter(BR == 0 | BR == 1) %>% droplevels() %>% mutate(BR = as.factor(BR));
br_0_2 = rec_all %>% filter(BR == 0 | BR == 2) %>% droplevels() %>% mutate(BR = as.factor(BR));
br_0_3 = rec_all %>% filter(BR == 0 | BR == 3) %>% droplevels() %>% mutate(BR = as.factor(BR));

smote_0_1 = SMOTE(BR~., br_0_1, perc.over = 1/(table(br_0_1$BR)[2]/sum(table(br_0_1$BR))),
              perc.under = 1/(table(br_0_1$BR)[1]/sum(table(br_0_1$BR))))


rec_wc = recipe(winner~., data = rec_all) %>%
  #            step_num2factor(matchID) %>%
  #            step_discretize(team_1_rank, options = list(cuts = 8)) %>%
  #            step_discretize(team_2_rank, options = list(cuts = 8)) %>%
  step_log(team_1_rank) %>%   step_scale(team_1_rank) %>%
  

         


