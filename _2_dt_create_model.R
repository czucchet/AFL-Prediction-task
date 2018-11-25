
library(DMwR);library(tidyverse);library(DBI);library(xgboost);library(recipes);library(rsample);library(RSQLite);library(purrr);library(e1071);library(digest);library(mlr);library(parallelMap);library(caret)

con = dbConnect(SQLite(), "PlayerRecords.sqlite")
teams = unique(dbGetQuery(con, "SELECT Team, GL FROM Player_Detail"));teams = sort(unique(teams[,"Team"]));state = c("SA","QLD","VIC","VIC","VIC","WA","VIC","QLD","NSW","VIC","VIC","VIC","SA","VIC","VIC","NSW","WA","VIC")
home_team = data.frame(Team = teams,home_state = state) %>% mutate_if(is.factor, as.character);away_team = data.frame(Opponent = teams,away_state = state) %>% mutate_if(is.factor, as.character)


num_cols = c("KI","MK","HB","GL","BH","HO","TK","RB","IF","CL","CG","FF","FA","BR","CP","UP","CM","MI","one_pc","BO","GA","game_played","Year","Diff","Age_Years","Games","PercentWon")
records = dbGetQuery(con, "SELECT * FROM Player_Detail") %>% rename(number = "#", one_pc = "1%", game_played = "%P");records[records == "Â"] <- 0
records_t2 = records %>% mutate_if(names(records) %in% num_cols, as.numeric) %>% distinct() %>% data.frame()
records_t2 = records_t2 %>% mutate(dream_team =(KI*3)+(HB*2)+(MK*3)+(GL*6)+(BO*1)+(HO*1)+(TK*4)+(FF*1)+(FA*-3))

records_t3 = records_t2 %>% group_by(Player, Team) %>% arrange(Games) %>% 
  mutate(win_loss = ifelse(Diff > 0, 1,0),
         prior_dt  = lag(dream_team),
         prior_dt_2  = lag(dream_team,2),
         prior_dt_3  = lag(dream_team,3),
         prior_dt_4  = lag(dream_team,4),
         prior_dt_5  = lag(dream_team,5),
         no_wins_last_3_games = lag(win_loss,1)+lag(win_loss,2)+lag(win_loss,3),
         no_wins_last_5_games = lag(win_loss,1)+lag(win_loss,2)+lag(win_loss,3)+lag(win_loss,4)+lag(win_loss,5),
         Age_Years = ifelse(is.na(Age_Years), lag(Age_Years),Age_Years)) %>%
         ungroup() %>% 
         mutate(rolling_last_3 = ifelse(is.na(prior_dt) | is.na(prior_dt_2) | is.na(prior_dt_3),NA, round(((prior_dt+prior_dt_2+prior_dt_3)/3),2)),
                rolling_last_5 = ifelse(is.na(prior_dt) | is.na(prior_dt_2) | is.na(prior_dt_3) | is.na(prior_dt_4) | is.na(prior_dt_5),NA, (prior_dt+prior_dt_2+prior_dt_3+prior_dt_4+prior_dt_5)/5)) %>% 
  select(-KI,-MK,-HB,-DI,-GL,-BH,-HO,-TK,-RB,-IF,-CL,-CG,-FF,-FA,-BR,-CP,-UP,-CM,-MI,-one_pc,-BO,-GA,-game_played,-number) %>%  distinct() %>% na.omit %>%  data.frame()
records_t4 = records_t3 %>% left_join(home_team) %>% left_join(away_team);rm(records_t3);rm(home_team);rm(away_team)
records_t5 = records_t4 %>% mutate(interstate = ifelse(home_state != away_state, 1,0));rm(records_t4)

records_t5[records_t5$Player == "Brodie, Will",]
records[records$Player == "Brodie, Will",]

#Need to fix number of games by player per game - currently actual game needs to adjust to prior for purpose of predcition
records_t3 %>% 
  group_by(Player, )


rec_train =  recipe(dream_team ~., records_t5) %>%
  step_dummy(Team,Home_Away,Opponent, home_state, away_state,Round) %>%
  step_dummy(Venue) %>%
  prep() %>%
  bake(newdata =  records_t5) %>%
  select(-ID,-Player) %>% data.frame()

cv.nround = 1000;cv.nfold = 5
min_error_index = 100
train_matrix <- xgb.DMatrix(data = as.matrix(rec_train %>%select(-dream_team)),
                            label = as.matrix(rec_train %>%select(dream_team)))

param <- list(objective = "reg:linear",eval_metric = "rmse",max_depth = 5,eta = 0.025,
              gamma = 0.01, min_child_weight = 4)
cv.nround = 1000;cv.nfold = 5
model_cv = xgb.cv(data=train_matrix, params = param,  
               nfold=cv.nfold, nrounds=cv.nround,
               verbose = 3)
min_error = min(model_cv$evaluation_log$test_rmse_mean)
min_error_index = which.min(model_cv$evaluation_log$test_rmse_mean)
#min_error_index = 898

xgb_model <- xgb.train(params = param,
                         data = train_matrix,
                         nrounds = min_error_index,
                         missing=NaN,
                         verbose = 3,
                         print_every_n = 5,
                         maximize = F)

xgb.save(xgb_model,'dt_xgb_model')


