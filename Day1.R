TM <- read.csv("~/Data/CollegeTM2018.csv")

library(lme4)
library(tidyverse)

TM <- TM%>%
  filter(!PlayResult == "Undefined")%>%
  mutate(slugnum = ifelse(
    PlayResult == "Single", 1, ifelse(
      PlayResult == "Double", 2, ifelse(
        PlayResult == "Triple", 3, ifelse(
          PlayResult == "HomeRun", 4, 0)))))%>%
  select(slugnum, ExitSpeed, Angle, Direction, Distance,
         PitcherId, BatterId, Stadium)

TM.clean <- TM%>%
  filter(!is.na(ExitSpeed))

TM.clean$PitcherId = as.factor(TM.clean$PitcherId)
TM.clean$BatterId = as.factor(TM.clean$BatterId)

model1 <- lmer(slugnum ~ I(ExitSpeed - mean(TM$ExitSpeed)) + Angle +
                 Direction + Distance + 
    (1 | PitcherId) +  (1 | BatterId) + (1 | Stadium), data = TM.clean)






