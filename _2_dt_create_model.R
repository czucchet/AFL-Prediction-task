setwd("C:/Users/Chris Zucchet/Documents/AFL-Prediction-task")
library(DMwR);library(tidyverse);library(DBI);library(xgboost);library(recipes);library(rsample);library(RSQLite);library(purrr);library(e1071);library(digest);library(mlr);library(parallelMap);library(caret)

con = dbConnect(SQLite(), "PlayerRecords.sqlite")

num_cols = c("KI","MK","HB","GL","BH","HO","TK","RB","IF","CL","CG","FF","FA","BR","CP","UP","CM","MI","one_pc","BO","GA","game_played","Year","Round","Diff","Age_Years","Games","PercentWon")
records = dbGetQuery(con, "SELECT * FROM Player_Detail")
records_t2 = records %>% mutate_if(names(records) %in% num_cols, as.numeric) %>%data.frame()
records_t2 = records_t2 %>% mutate(dream_team =(KI*3)+(HB*2)+(MK*3)+(GL*6)+(BO*1)+(HO*1)+(TK*4)+(FF*1)+(FA*-3))
x = records_t2 %>% group_by(Player, Team) %>%
  mutate(prior_dt  = lag(dream_team),
         prior_dt_2  = lag(dream_team,2),
         prior_dt_3  = lag(dream_team,3),
         prior_dt_4  = lag(dream_team,4),
         prior_dt_5  = lag(dream_team,5),
         rolling_last_3 = mean(lag(dream_team),lag(dream_team,2),lag(dream_team,3), na.rm = T, trim = 1),
         rolling_last_5 = mean(lag(dream_team),lag(dream_team,2),lag(dream_team,3),lag(dream_team,4),lag(dream_team,5), na.rm = T, trim = .1),
         Age_Years = ifelse(is.na(Age_Years), lag(Age_Years),Age_Years)) %>%
  ungroup() %>% data.frame()
x[x$Player == "Dangerfield, Patrick",]
