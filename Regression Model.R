##Read Data
setwd('C:/Users/wy/OneDrive/CSC424 Advanced Data Analysis/Final Project')
NCAA = read.csv('NCAA_Tournament_WPCT.csv')
## Build Model
library(VIF)
model1 = lm(TourneyWPCT ~ Wins+Losses+WinPerc+FGM+FGA+FGPerc+FGM3+FGA3+
              FG3Perc+FTM+FTA+FTPerc+Oreb+Dreb+Assists+TO+Steals+Blocks+
              Fouls+OppFGM+OppFGA+OppFGPerc+OppFGM3+OppFGA3+OppFG3Perc+
              OppFTM+OppFTA+OppFTPerc+OppOreb+OppDreb+OppAssists+OppTO+
              OppSteals+OppBlocks+OppFouls, data = NCAA)
print(summary(model1))
VIF(model1)
plot(NCAA)

V#Backward/Forward/Stepwise Selection
null = lm(TourneyWPCT ~ 1, data = NCAA)
full = lm(TourneyWPCT ~., data = NCAA)
step(null, scope = list(lower=null, upper=full), 
     direction="forward")
step(full, data = NCAA, direction = "backward")
step(null, scope = list(upper = full), data = NCAA, 
     direction = "both")

