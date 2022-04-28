sex <- c("V3042","V3084","V3046","V3049")
sex1 <- da38090.0003[sex]
sex1
View(sex1)
library(dplyr)
sex2 <-sex1 %>% na.omit()
View(sex2)
sex2$sexual <- ifelse(sex2$V3084 == "(4) Something else ", "trans",
                      ifelse(sex2$V3084 == "(5) I don't know the answer", NA,
                      ifelse(sex2$V3084 == "(6) Refused", "Other",
                      ifelse(sex2$V3084 == "(8) Residue", NA,
                      ifelse(sex2$V3084 =="(2) Straight, that is, not lesbian or gay", "Straight",
                      ifelse(sex2$V3084 == "(1) Lesbian or gay " | sex2$V3084 == "(3) Bisexual", "LGB", NA))))))

sex2$sexual
View(sex2)
summary(sex2)
sex3 <- na.omit(sex2)
View(sex3)
summary(sex3)
sex2$frequent <-ifelse(sex2$V3042 == "(1) Yes", "Yes",
                     ifelse(sex2$V3042 == "(2) No", "No", NA))



sex3
View(sex3)
LGB = subset(sex3,sex3$sexual == "LGB")
summary(LGB)
LGB1 <- table(LGB$V3042)
pie(LGB1)

View(LGB)
nonLGBT = subset(sex3,sex3$sexual == "Straight")
nonL <- table(nonLGBT$V3042)
pie(nonL)
summary(nonLGBT)
