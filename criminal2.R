sex <- c("V3042","V3084","V3046","V3049")
sex <- c("V3084","V3054")
sex1 <- da38090.0003[sex]
sex1
sex2 <- sex1 %>% na.omit()
View(sex1)
library(dplyr)
#comparsion 3042 & 3084
attack <- c("V3042","V3084")
attack <- da38090.0003[attack]
sex2 <-attack %>% na.omit()
summary(sex2)
sex2$orientation <- ifelse(sex2$V3084 == "(4) Something else ", "Other",
                      ifelse(sex2$V3084 == "(5) I don't know the answer", NA,
                             ifelse(sex2$V3084 == "(6) Refused", "Other",
                                    ifelse(sex2$V3084 == "(8) Residue", NA,
                                           ifelse(sex2$V3084 =="(2) Straight, that is, not lesbian or gay", "Straight",
                                                  ifelse(sex2$V3084 == "(1) Lesbian or gay " | sex2$V3084 == "(3) Bisexual", "LGB", NA))))))
sex2$frequent <-ifelse(sex2$V3054 == "(1) Yes", "Yes",
                       ifelse(sex2$V3054 == "(2) No", "No", NA))
summary(sex2)

View(sex2)
LGB = subset(sex2,sex2$orientation == "LGB")
View(LGB)
LGB1 <- table(LGB$frequent)
View(LGB1)
install.packages("RColorBrewer")
library(RColorBrewer)
#########LGB FIGURE
Freq <- c(678,28)
labels <-c("No","Yes")

piepercent<- round(100*Freq/sum(Freq), 1)

# Give the chart file a name.
png(file = "attack_legends.jpg")

# Plot the chart.
pie(Freq, labels = piepercent, main = "LGB being attacked",col = rainbow(length(Freq)))
legend("topright", c("No","Yes"), cex = 0.8,
       fill = rainbow(length(Freq)))

# Save the file.
dev.off()

######another way
##########################

install.packages("ggplot2")
install.packages("stringr")
library(ggplot2)

df <- data.frame(
  labels = c("No","Yes"),
  Freq = c(678,28)
)
head(df)
bp<- ggplot(df, aes(x="", y=Freq, fill=labels))+
  geom_bar(width = 1, stat = "identity")
bp
barplot(LGB1,col=Freq, main = "LGB attacked Frequency")
pie <- bp + coord_polar("y", start=0)
pie
install.packages("scales")
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
pie + scale_fill_brewer("Answer") + blank_theme +
  theme(axis.text.x=element_blank())+
  geom_text(aes(y = Freq/2 + c(0, cumsum(Freq)[-length(Freq)]), 
                label = percent(Freq/sum(Freq))), size=7)+
  ggtitle("LGB being attacked")
########################################################
#########################straignt######

  


