setwd("C:/Users/Chris Zucchet/Documents/AFL-Prediction-task")
library(tidyverse);library(DBI);library(xgboost);library(recipes);library(rsample);library(RSQLite);library(purrr)

con = dbConnect(SQLite(), "PlayerRecords.sqlite")
num_cols = c("KI","MK","HB","BH","HO","TK","RB","IF","CL","CG","FF","FA","BR","CP","UP","CM","MI","one_pc","BO","GA","game_played","Year","Round","Diff","Age_Years","Games","PercentWon")
scale_cols = c("KI","MK","HB","BH","HO","TK","RB","IF","CL","CG","FF","FA","CP","UP","CM","MI","one_pc","BO","GA","game_played","Age_Years","Games","PercentWon")

records = unique(dbGetQuery(con, "SELECT * FROM Player_Detail"))%>% na.omit()%>% rename(number = "#", one_pc = "1%", game_played = "%P") %>%   mutate_if((names(records) %in% num_cols), as.numeric)

sum(is.na(records))


plot(records$Games,records$BR)

records_temp = records %>%
  mutate(is_winner = ifelse(Diff >0,1,0),
  ifelse(games_1_10 =  ifelse(Games <= 10, 1,0),
         ifelse(games_11_50 =  ifelse(Games <= 10, 1,0),
         
                              )           
         
         


#         %>% select(-number, -Player,-DI)
