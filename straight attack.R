Straight = subset(sex2,sex2$orientation == "Straight" | sex2$orientation == "Other")
View(Straight)

Straight2 <- table(Straight$frequent)
View(Straight2)

df <- data.frame(Freq <- c(109424,633),
                 labels <-c("No","Yes")
)


library(ggplot2)
# Barplot
bp<- ggplot(df, aes(x="", y=Freq, fill=labels))+
  geom_bar(width = 1, stat = "identity")
bp


)
barplot(nonL,col=Freq, main = "Straight attacked Frequency")
head(df)
bp<- ggplot(df, aes(x="", y=Freq, fill=label))+
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
pie + scale_fill_brewer("Answer") + blank_theme +
  theme(axis.text.x=element_blank())+
  geom_text(aes(y = Freq/2 + c(0, cumsum(Freq)[-length(Freq)]), 
                label = percent(Freq/sum(Freq))), size=7)+
  ggtitle("Straight being attacked")
library(dplyr)
df <- df %>% 
  group_by(label) %>% # Variable to be transformed
  count() %>% 
  ungroup() %>% 
  mutate(perc = Freq / sum(Freq)) %>% 
  arrange(perc) %>%
  mutate(labels = scales::percent(perc))
ggplot(df, aes(x = "", y = perc, fill = label)) +
  geom_col() +
  geom_text(aes(label = labels),
            position = position_stack(vjust = 0.5))
coord_polar(theta = "y")
summary(Straight)
barplot(Straight2,col=Freq, main = "Striaght attacked Frequency")
