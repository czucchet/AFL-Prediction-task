setwd("C:/Users/Chris Zucchet/Documents/AFL-Prediction-task")
library(DMwR);library(tidyverse);library(DBI);library(xgboost);library(recipes);library(rsample);library(RSQLite);library(purrr);library(e1071);library(digest);library(mlr);library(parallelMap);library(caret)
range01 <- function(x){(x-min(x))/(max(x)-min(x))}
con = dbConnect(SQLite(), "PlayerRecords.sqlite")
num_cols = c("KI","MK","HB","GL","BH","HO","TK","RB","IF","CL","CG","FF","FA","BR","CP","UP","CM","MI","one_pc","BO","GA","game_played","Year","Round","Diff","Age_Years","Games","PercentWon")
scale_cols = c("KI","MK","HB","GL","BH","HO","TK","RB","IF","CL","CG","FF","FA","CP","UP","CM","MI","one_pc","BO","GA","game_played","Age_Years","Games","PercentWon","dt_score","error_rate","fairness","impact_plays")

records = dbGetQuery(con, "SELECT * FROM Player_Detail WHERE year <> 2017") %>%group_by(Player,KI,MK,CP,Team,ID) %>%
  filter(row_number(HB) == 1) %>% ungroup() %>% rename(number = "#", one_pc = "1%", game_played = "%P") 
records[records == "Â"] <- "0" 
records2 = records %>% anti_join(records %>% group_by(ID) %>% summarise(BR_SUM = sum(as.numeric(BR))) %>% 
            filter(BR_SUM != 6))
records_t2 = records2 %>%  mutate_if(names(records) %in% num_cols, as.numeric)%>% na.omit() ;rm(records)

teams = unique(dbGetQuery(con, "SELECT Team, GL FROM Player_Detail"));teams = sort(unique(teams[,"Team"]));state = c("SA","QLD","VIC","VIC","VIC","WA","VIC","QLD","NSW","VIC","VIC","VIC","SA","VIC","VIC","NSW","WA","VIC")
home_team = data.frame(Team = teams,home_state = state) %>% mutate_if(is.factor, as.character);away_team = data.frame(Opponent = teams,away_state = state) %>% mutate_if(is.factor, as.character)

records_t3 = records_t2 %>%
  mutate(is_winner = ifelse(Diff >0,1,0),
         is_1_6_margin = ifelse(abs(Diff) <= 6,1,0),is_7_30_margin = ifelse(abs(Diff) > 6 & abs(Diff) <= 30,1,0),is_30_margin = ifelse(abs(Diff) >= 31,1,0),
         is_6_margin = ifelse(abs(Diff) <= 6,1,0),is_30_margin = ifelse(abs(Diff) <= 30,1,0),is_50_margin = ifelse(abs(Diff) <= 50,1,0),is_100_margin = ifelse(abs(Diff) <= 100,1,0),
         games_1_10 =  ifelse(Games <= 10, 1,0), games_0_50 = ifelse(Games <= 50, 1,0),         #games_51_more = ifelse(Games > 50, 1,0),
         dt_score = (KI*2.5)+(HB*1.5)+(MK*3)+(GL*7)+(BH*2)+(TK*2)+(HO*1.5)+(FF*1)+(FA*-3),
         error_rate = CG/(KI+HB),
         fairness = FF-FA,
         is_no_errors =  ifelse(CG == 0,1,0),
         impact_plays = (IF*0.5)+(TK*0.25)+(MI*1)+(one_pc*3)+(BO*0.25)+(GA*1.5)+(RB*1)+(CG*-1)+(CL*1)+(CM*1),
#         CP_score =(CP*1.5)+(UP*0.75),
         gl_dt =(GL*6)/((KI*2.5)+(HB*1.5)+(MK*3)+(GL*6)+(BH*2)+(TK*4)+(HO*1)+(FF*1)+(FA*-3))) %>%
  group_by(ID,Team) %>%
  mutate(GL_Sum = sum(GL),CM_Sum = sum(CM),MI_Sum = sum(MI),CG_Sum = sum(CG)) %>%
  ungroup(ID) %>%
  mutate(GL_prop = GL/GL_Sum,CG_prop = CM/CM_Sum,MI_prop = MI/MI_Sum,CG_prop = CG/CG_Sum) %>% select(-GL_Sum,-CM_Sum,-MI_Sum,-CG_Sum)

    
records_t4 = records_t3 %>% left_join(home_team) %>% left_join(away_team);rm(records_t3);rm(home_team);rm(away_team)
records_t5 = records_t4 %>% mutate(interstate = ifelse(home_state != away_state, 1,0));rm(records_t4)

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

rec_all = rec_allt2 %>% select(-number,-Player,-DI,-ID,-ID_Team)
rec_train =  recipe(BR ~., rec_all) %>%
  step_dummy(Team,Home_Away,Opponent, home_state, away_state) %>%
  step_other(Venue, threshold = .2) %>%
  step_dummy(Venue) %>%
  prep() %>%
  bake(newdata =  rec_all)

br_0_1 = rec_train %>% filter(BR == 0 | BR == 1) %>% droplevels()%>% mutate(BR = as.factor(BR)) %>% data.frame();smote_0_1 = SMOTE(BR~., br_0_1, perc.over = sum(table(br_0_1$BR))/table(br_0_1$BR)[2]*100, perc.under = sum(table(br_0_1$BR))/table(br_0_1$BR)[1]*100)
br_0_2 = rec_train %>% filter(BR == 0 | BR == 2) %>% droplevels()%>% mutate(BR = as.factor(BR)) %>% data.frame();smote_0_2 = SMOTE(BR~., br_0_2, perc.over = sum(table(br_0_2$BR))/table(br_0_2$BR)[2]*100, perc.under = sum(table(br_0_2$BR))/table(br_0_2$BR)[1]*100)
br_0_3 = rec_train %>% filter(BR == 0 | BR == 3) %>% droplevels()%>% mutate(BR = as.factor(BR)) %>% data.frame();smote_0_3 = SMOTE(BR~., br_0_3, perc.over = sum(table(br_0_3$BR))/table(br_0_3$BR)[2]*100, perc.under = sum(table(br_0_3$BR))/table(br_0_3$BR)[1]*100)
bal_data = rbind(smote_0_1[smote_0_1$BR == 1,],smote_0_2[smote_0_2$BR == 2,],smote_0_3[smote_0_3$BR == 3,],rec_train[rec_train$BR == 0,])
bal_data$BR = as.numeric(bal_data$BR)-1 
bal_data_all = bal_data %>% na.omit()


train_matrix <- xgb.DMatrix(data = as.matrix(bal_data_all %>%select(-BR)),
                            label = as.matrix(bal_data_all %>%select(BR)))
param <- list(objective = "multi:softprob",eval_metric = "merror",num_class = 4,max_depth = 5,eta = 0.025,
              gamma = 0.01, min_child_weight = 4)

cv.nround = 1000;cv.nfold = 5
#model_cv = xgb.cv(data=train_matrix, params = param,  
#               nfold=cv.nfold, nrounds=cv.nround,
#               verbose = 3)
#min_error = min(model_cv$evaluation_log$test_merror_mean)
#min_error_index = which.min(model_cv$evaluation_log$test_merror_mean)
#min_error_index = 898
min_error_index = 100
xgb_model <- xgb.train (params = param,
                         data = train_matrix,
                         nrounds = min_error_index,
                         missing=NaN,
                         verbose = 3,
                         print_every_n = 5,
                         maximize = F)

xgb.save(xgb_model,'afl_model_pre2017')
#xgb.save(xgb_model,'afl_model')
xgb.importance(model = xgb_model) %>% head(25)

cor(records_t5$impact_plays,records_t5$BR)


