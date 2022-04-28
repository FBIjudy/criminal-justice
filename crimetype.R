wtype<- c("V3049","V3084")
wtype <- da38090.0003[wtype]

sex2 <-wtype %>% na.omit()
summary(sex2) 
sex2$orientation <- ifelse(sex2$V3084 == "(4) Something else ", "Other",
                           ifelse(sex2$V3084 == "(5) I don't know the answer", NA,
                                  ifelse(sex2$V3084 == "(6) Refused", "Other",
                                         ifelse(sex2$V3084 == "(8) Residue", NA,
                                                ifelse(sex2$V3084 =="(2) Straight, that is, not lesbian or gay", "Straight", 
                                                       ifelse(sex2$V3084 == "(1) Lesbian or gay " | sex2$V3084 == "(3) Bisexual", "LGB", NA))))))
sex2$type <- ifelse(sex2$V3049 == "(27) Not a crime", NA,
                    ifelse(sex2$V3049 == "(26) Crime against hh", NA,
                           ifelse(sex2$V3049 == "(16) Attempt assault", "Assault",
                                  ifelse(sex2$V3049 == "(19) Larceny", "Larceny",
                                         ifelse(sex2$V3049 =="(11) Rape", NA,
                                                ifelse(sex2$V3049 =="(24) Vandalism", "Vandalism",
                                                       ifelse(sex2$V3049 =="(15) Assault", "Assault",NA)))))))
sex2$type <- ifelse(sex2$V3049 == "(27) Not a crime", NA,
                    ifelse(sex2$V3049 == "(26) Crime against hh", NA,
                           ifelse(sex2$V3049 == "(16) Attempted assault", "Assault",
                                  ifelse(sex2$V3049 == "(19) Larceny", "Larceny",
                                         ifelse(sex2$V3049 =="(11) Rape", "Rape",
                                                ifelse(sex2$V3049 =="(12) Attempted rape", "Rape",
                                                ifelse(sex2$V3049 =="(24) Vandalism", "Vandalism",
                                                       ifelse(sex2$V3049 =="(15) Assault", "Assault",NA))))))))

                                                                                                                                                           ifelse(sex2$V3084 == "(1) Lesbian or gay " | sex2$V3084 == "(3) Bisexual", "LGB", NA))))))
View(sex2)
sex <- sex2 %>% na.omit()
View(sex)
LGB = subset(sex,sex$orientation == "LGB")
summary(LGB)
View(LGB)
library(RColorBrewer)
coul <- brewer.pal(5, "Set2") 
type <-table(LGB$type)
type
p2 <- as.matrix(prop.table(type)) * 100
p2
barplot(p2,main = "Crime type that LGB victims suffer",col=coul,ylab="percent",beside=TRUE, xlab = "crime type", ylim = c(0,100),legend=TRUE)

par(mar = c(4, 4, 4, 5))

Straight = subset(sex,sex$orientation == "Straight" | sex$orientation == "Other")
type <- table(Straight$type)
View(Straight)
coul <- brewer.pal(5, "Set3")

summary(Straight)
barplot(p2,main = "Crime Type that Straight victims suffer",col=coul,ylim=c(0,100), ylab="percent",beside=TRUE,xlab="crime type",legend=TRUE)
library("boxplot")

