
wsex<- c("V3046","V3084","V3086")
wsex <- da38090.0003[wsex]
sex2 <-wsex %>% na.omit()
View(sex2)
summary(sex2)
sex2$orientation <- ifelse(sex2$V3084 == "(4) Something else ", "Other",
                           ifelse(sex2$V3084 == "(5) I don't know the answer", NA,
                                  ifelse(sex2$V3084 == "(6) Refused", "Other",
                                         ifelse(sex2$V3084 == "(8) Residue", NA,
                                                ifelse(sex2$V3084 =="(2) Straight, that is, not lesbian or gay", "Straight",
                                                       ifelse(sex2$V3084 == "(1) Lesbian or gay " | sex2$V3084 == "(3) Bisexual", "LGB", NA))))))
sex2$frequent <-ifelse(sex2$V3046 == "(1) Yes", "Yes",
                       ifelse(sex2$V3046 == "(2) No", "No", NA))
View(sex2)
Straight = subset(sex2,sex2$orientation == "Straight" | sex2$orientation == "Other")
View(Straight)
LGB = subset(sex2,sex2$orientation == "LGB")
barplot(table(Straight$V3054),main = "Crime type that LGBT victims suffer",col=coul)
barplot(table(LGB$V3048),main = "Crime type that LGBT victims suffer",col=coul)
##########average times of LGB
table(LGB$V3047)
df <- data.frame(Freq <- c(1.4,1.42),
                 labels <-c("sex assault","sex assault")
)
table(Straight$V3047)
Straight2 <- table(Straight$frequent)
View(Straight2)

df <- data.frame(Freq <- c(109930,64),
                 labels <-c("No","Yes")
)
df <- data.frame(Freq <- c(64/109994,10/706),
                 labels <-c("straight","LGB"))
barplot(height=df$Freq....c.64.109994..10.706., names=df$labels....c..straight....LGB.., col=coul )

library(ggplot2)
# Barplot
bp<- ggplot(df, aes(x="", y=Freq, fill=labels))+
  geom_bar(width = 1, stat = "identity")
bp



barplot(Straight2,col=Freq, main = "Straight unwanted sex Frequency")
head(df)
bp<- ggplot(df, aes(x="", y=Freq, fill=labels))+
  geom_bar(width = 1, stat = "identity")
bp
pie <- bp + coord_polar("y", start=0)
pie

library(scales)
blank_theme <- theme_minimal()+
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.border = element_blank(),
    panel.grid=element_blank(),
    axis.ticks = element_blank(),
    plot.title=element_text(size=14, face="bold")
  )
Freq
percentlabels <- round(100*Freq/sum(Freq), 2)
pielabels<- paste(percentlabels, "%", sep="")
pie + scale_fill_brewer("Orientation") + blank_theme +
  theme(axis.text.x=element_blank())+
  geom_text(aes(y = Freq/2 + c(0, cumsum(Freq)[-length(Freq)]), 
                label = pielabels), size=7)+
  ggtitle("victims who suffer from sexual assault")
library(RColorBrewer)
coul <- brewer.pal(5, "Set2") 


