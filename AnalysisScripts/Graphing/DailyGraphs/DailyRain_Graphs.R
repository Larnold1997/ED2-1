##### Load Packages and Read-in Data ####
library(ggplot2)
library(gridExtra)

#### Set working Directory ####
#If output is saved, it will be saved to this directory 
pwd <- "~/Desktop/ED"
setwd(pwd)

df <- read.csv("NewSoil/BaselineSimulations/DailySoilWater.csv")

#### Modify Data for Graphing ####

#Dates of Data
nyear = 3
month28 = c(1:28)
month30 = c(1:30)
month31 = c(1:31)

days   = c(month31,month28,month31,month30,month31,month30,month31,month31,
           month30,month31,month30,month31)

months = c("01","02","03","04","05","06","07","08","09","10","11","12")

iday   = rep(days,nyear) 
imonth = rep(months,c(31,28,31,30,31,30,31,31,30,31,30,31)) 

#namindex will be used to to store information about the file name
#R should automatically repeat imonth and iday, so that nameindex is proper length
nameindex = data.frame(imonth,iday)


group <- rep(c("C4 Grass", "Acquisitive Shrub","Conservative Shrub",
               "Acquisitive Tree","Conservative Tree"), c(1095, 1095, 1095, 1095, 1095))

#again, R will automatically repeat nameindex
df <- data.frame(nameindex, as.vector(as.matrix(df)), group)
colnames(df) <- c('Month','Day',"Rain", "PFT")


#### Get Wet Season Data ####
df.wet <- NULL

for(i in 1:nrow(df)){
  if(df$Month[i] == "06" | df$Month[i] == "07" | 
     df$Month[i] == "08" | df$Month[i] == "09" |
     df$Month[i] == "10" | df$Month[i] == "11" |
     df$Month[i] == "12"){
    df.wet <- rbind(df.wet, df[i,])
  }
}


pdf('PFT_Rain.pdf')

colors <- c("C4 Grass" = "red", 
            "Acquisitive Shrub" = "light blue",
            "Conservative Shrub" = "light green", 
            "Acquisitive Tree" = "dark blue", 
            "Conservative Tree"= "dark green")


## All Data

# Basic histogram
ggplot(df, aes(x=Rain, color = PFT)) + geom_density(alpha=0) + 
  ggtitle("Probability Density Functions") + 
  xlab("Daily Rainfall (mm)") + ylab("Density") +
  scale_color_manual(values = colors,
                     breaks = group)

#Histogram of each PFT
p1 <- ggplot(df[df$PFT == "C4 Grass",], aes(x = Rain, group = "C4 Grass")) + 
  geom_histogram(color = "red", fill = "red") +
  ggtitle("C4 Grass") + 
  xlab("Daily Rainfall (mm)") + ylab("Counts") 
p2 <- ggplot(df[df$PFT == "Acquisitive Shrub",], aes(x = Rain, group = "Acquisitive Shrub")) + 
  geom_histogram(color = "light blue", fill = "light blue") +
  ggtitle("Acquisitive Shrub") + 
  xlab("Daily Rainfall (mm)") + ylab("Counts") 
p3 <- ggplot(df[df$PFT == "Conservative Shrub",], aes(x = Rain, group = "Conservative Shrub")) + 
  geom_histogram(color = "light green", fill = "light green") +
  ggtitle("Conservative Shrub") + 
  xlab("Daily Rainfall (mm)") + ylab("Counts") 
p4 <- ggplot(df[df$PFT == "Acquisitive Tree",], aes(x = Rain, group = "Acquisitive Tree")) + 
  geom_histogram(color = "dark blue", fill = "dark blue") +
  ggtitle("Acquisitive Tree") + 
  xlab("Daily Rainfall (mm)") + ylab("Counts") 
p5 <- ggplot(df[df$PFT == "Conservative Tree",], aes(x = Rain, group = "Conservative Tree")) + 
  geom_histogram(color = "dark green", fill = "dark green") +
  ggtitle("Conservative Tree") + 
  xlab("Daily Rainfall (mm)") + ylab("Counts") 
grid.arrange(p1, p2, p3, p4, p5, nrow = 5)


## Wet Sesaon 

# Basic histogram
ggplot(df.wet, aes(x=Rain, color = PFT)) + geom_density(alpha=0) + 
  ggtitle("Probability Density Functions (June - December)") + 
  xlab("Daily Rainfall (mm)") + ylab("Density") +
  scale_color_manual(values = colors,
                     breaks = group)

#Histogram of each PFT
p1 <- ggplot(df.wet[df.wet$PFT == "C4 Grass",], aes(x = Rain, group = "C4 Grass")) + 
  geom_histogram(color = "red", fill = "red") +
  ggtitle("C4 Grass (June - December)") + 
  xlab("Daily Rainfall (mm)") + ylab("Counts") +
  geom_vline(aes(xintercept=mean(Rain), group = "C4 Grass"),
               color="black", linetype="dashed", size=1)
p2 <- ggplot(df.wet[df.wet$PFT == "Acquisitive Shrub",], aes(x = Rain, group = "Acquisitive Shrub")) + 
  geom_histogram(color = "light blue", fill = "light blue") +
  ggtitle("Acquisitive Shrub (June - December)") + 
  xlab("Daily Rainfall (mm)") + ylab("Counts") +
  geom_vline(aes(xintercept=mean(Rain), group = "Acquisitive Shrub"),
             color="black", linetype="dashed", size=1)
p3 <- ggplot(df.wet[df.wet$PFT == "Conservative Shrub",], aes(x = Rain, group = "Conservative Shrub")) + 
  geom_histogram(color = "light green", fill = "light green") +
  ggtitle("Conservative Shrub (June - December)") + 
  xlab("Daily Rainfall (mm)") + ylab("Counts") +
  geom_vline(aes(xintercept=mean(Rain), group = "Conservative Shrub"),
             color="black", linetype="dashed", size=1)
p4 <- ggplot(df.wet[df.wet$PFT == "Acquisitive Tree",], aes(x = Rain, group = "Acquisitive Tree")) + geom_histogram(color = "dark blue", fill = "dark blue") +
  ggtitle("Acquisitive Tree (June - December)") + 
  xlab("Daily Rainfall (mm)") + ylab("Counts") +
  geom_vline(aes(xintercept=mean(Rain), group = "Acquisitive Tree"),
             color="black", linetype="dashed", size=1)
p5 <- ggplot(df.wet[df.wet$PFT == "Conservative Tree",], aes(x = Rain, group = "Conservative Tree")) + 
  geom_histogram(color = "dark green", fill = "dark green") +
  ggtitle("Conservative Tree (June - December)") + 
  xlab("Daily Rainfall (mm)") + ylab("Counts") +
  geom_vline(aes(xintercept=mean(Rain), group = "Conservative Tree"),
             color="black", linetype="dashed", size=1)
grid.arrange(p1, p2, p3, p4, p5, nrow = 5)



dev.off()