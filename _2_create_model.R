setwd("C:/Users/Chris Zucchet/Documents/AFL-Prediction-task")
library(tidyverse);library(DBI);library(xgboost);library(recipes);library(rsample);library(RSQLite);library(purrr);library(e1071)
range01 <- function(x){(x-min(x))/(max(x)-min(x))}
con = dbConnect(SQLite(), "PlayerRecords.sqlite")
num_cols = c("KI","MK","HB","GL","BH","HO","TK","RB","IF","CL","CG","FF","FA","BR","CP","UP","CM","MI","one_pc","BO","GA","game_played","Year","Round","Diff","Age_Years","Games","PercentWon")
scale_cols = c("KI","MK","HB","GL","BH","HO","TK","RB","IF","CL","CG","FF","FA","CP","UP","CM","MI","one_pc","BO","GA","game_played","Age_Years","Games","PercentWon","dt_score","error_rate","fairness")

records = unique(dbGetQuery(con, "SELECT * FROM Player_Detail")) %>% na.omit()%>% rename(number = "#", one_pc = "1%", game_played = "%P") %>%  replace("Â",0) %>% mutate_if((names(records) %in% num_cols), as.numeric)%>% na.omit() %>%select(-Â)
teams = sort(unique(dbGetQuery(con, "SELECT Team FROM Player_Detail")))

records = records %>%
  mutate(is_winner = ifelse(Diff >0,1,0),
         games_1_10 =  ifelse(Games <= 10, 1,0),games_11_50 = ifelse(Games > 10 & Games <= 50, 1,0),games_51_more = ifelse(Games > 50, 1,0),
         dt_score = (KI*3)+(HB*2)+(MK*3)+(GL*6)+(BH*1)+(TK*3)+(HO*1)+(FF*1)+(FA*-3)+(GA*3),
         error_rate = CG/(KI+HB),
         fairness = FF-FA)

rec_split = records %>% split(.$ID)
for(i in 1:length(rec_split)){
  for(j in scale_cols){
    rec_split[[i]][,j] = range01(rec_split[[i]][,j])
    }
  }
rec_all = bind_rows(rec_split)  

         


