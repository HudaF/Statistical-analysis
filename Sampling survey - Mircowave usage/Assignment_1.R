
#Loading dataset
mic<-read.table(file = "data/microwave.csv", header = TRUE, sep = ",")
micCat <- read.table(file = "data/microwaveCat.csv", header = TRUE, sep = ",")

#Remove empty columns
#mic <- Filter(function(x)!all(is.na(x)), mic)

str(mic)

#Finding summary of our data
apply(mic, 2, summary)

#Mean,median,mode,standard deviation, and data spread of time food was heated
mean(mic$Heated) #1.275833 minutes
median(mic$Heated) #1.5 minutes

getmode <- function(x) {
  ux <- unique(x)
  tab <- tabulate(match(x, ux))
  ux[tab == max(tab)]
}
getmode(mic$Heated) #1.5 minutes

sd(mic$Heated) #0.4736705
var(mic$Heated) # 0.2243637

#Mean,median,mode,standard deviation, and data spread of size of queue
mean(mic$Queue) 
median(mic$Queue) 
getmode(mic$Queue) 
sd(mic$Queue) #
var(mic$Queue)


hist(mic$Queue,main="Histogram of Queue size",col=c("blue"),breaks="FD")
hist(mic$Heated,main="Histogram of Queue size",col=c("blue"),breaks="FD")
golden <- 
  
boxplot(Queue~Day,data=mic, main="Queue size per day",
        xlab="Day number of week", ylab="Size of Queue")


library(ggplot2)
# Basic histogram
ggplot(mic, aes(x=Queue)) + geom_histogram()
# Change the width of bins
ggplot(mic, aes(x=Queue)) + 
  geom_histogram(binwidth=100)
p+ geom_vline(aes(xintercept=mean(Queue)),
              color="blue", linetype="dashed", size=1)
# Change colors
p<-ggplot(mic, aes(x=Queue)) + 
  geom_histogram(color="darkblue", fill="lightblue")
p


m<-micCat[micCat$Day=="Monday",]
t<-micCat[micCat$Day=="Tuesday",]
w<-micCat[micCat$Day=="Wednesday",]
th<-micCat[micCat$Day=="Thursday",]
f<-micCat[micCat$Day=="Friday",]

breaks <- pretty(range(m$Queue), n = nclass.FD(m$Queue), min.n = 1)
bwidth <- breaks[2]-breaks[1]
mp <- ggplot(m,aes(Queue))+geom_histogram(binwidth=0.5,fill = "white", colour = "DarkRed") + ggtitle("Frequency of Queue sizes on Monday")
mp

breaks <- pretty(range(t$Queue), n = nclass.FD(t$Queue), min.n = 1)
bwidth <- breaks[2]-breaks[1]
tp <- ggplot(t,aes(Queue))+geom_histogram(binwidth=0.5,fill = "white", colour = "DarkRed") + ggtitle("Frequency of Queue sizes on Tuesday")
tp

breaks <- pretty(range(w$Queue), n = nclass.FD(w$Queue), min.n = 1)
bwidth <- breaks[2]-breaks[1]
wp <- ggplot(w,aes(Queue))+geom_histogram(binwidth=0.5,fill = "white", colour = "DarkRed") + ggtitle("Frequency of Queue sizes on Wednesday")
wp



library(plyr)
mu <- ddply(micCat, "Day", summarise, grp.mean=mean(Heated))
head(mu)


breaks <- pretty(range(micCat$Queue), n = nclass.FD(mic$Queue), min.n = 1)
bwidth <- breaks[2]-breaks[1]
ggplot(micCat,aes(Queue))+geom_histogram(binwidth=0.5,fill="white", colour = "DarkRed") + ggtitle("Frequency of Queue sizes")

breaks <- pretty(range(mic$Heated), n = nclass.FD(mic$Heated), min.n = 1)
bwidth <- breaks[2]-breaks[1]
ggplot(mic,aes(Heated))+geom_histogram(binwidth=bwidth,fill = "white", colour = "DarkRed")  + ggtitle("Count of people w.r.t oven's timer time")
#"#3366FF"

breaks <- pretty(range(mic$Day), n = nclass.FD(mic$Day), min.n = 1)
bwidth <- breaks[2]-breaks[1]
ggplot(mic,aes(Day))+geom_histogram(binwidth=bwidth,fill = "white", colour = "DarkRed") + ggtitle("Count of People Each Day")


# Basic box plot
p <- ggplot(micCat, aes(x=Day, y=Heated)) + 
  geom_boxplot(varwidth = TRUE,fill = "white", colour = "DarkRed") #varwidth shows sample size
p
# Change outlier, color, shape and size
ggplot(micCat, aes(x=Day, y=Heated)) + 
  geom_boxplot(outlier.colour="red", outlier.shape=8,
               outlier.size=4)
p + stat_summary(fun.y=mean, geom="point", shape=23, size=4)



cor(micCat$Heated,micCat$Queue)





install.packages("ggpubr")

library("ggpubr")
theme_set(
  theme_bw() +
    theme(legend.position = "top")
)


ggplot(histogram, aes(Queue)) + 
  geom_histogram(data = m, fill = "red", alpha = 0.2) + 
  geom_histogram(data = t, fill = "blue", alpha = 0.2)

figure <- ggarrange(bxp, dp, lp,
                    labels = c("A", "B", "C"),
                    ncol = 2, nrow = 2)
figure



install.packages("gridExtra")
library(grid)
library(gridExtra)

grid.arrange(mp, tp, ncol = 2, main = "Main title")

