##### Load Packages and Read-in Data ####
library(ggplot2)
library(gridExtra)
library(knitr)

#### Set working Directory ####
#If output is saved, it will be saved to this directory 
pwd <- "~/Desktop/ED"
setwd(pwd)

# put location of file
df <- read.csv("WetYear/SandyClay/DailySoilWater_SandyClay_Wet.csv")

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

PFT <- c("C4 Grass", "Acquisitive Shrub","Conservative Shrub",
         "Acquisitive Tree","Conservative Tree")
group <- rep(PFT, c(1095, 1095, 1095, 1095, 1095))

#again, R will automatically repeat nameindex
df <- data.frame(nameindex, as.vector(as.matrix(df)), group)
colnames(df) <- c('Month','Day',"Soil.Water", "PFT")
df.PFT01 <- df[df$PFT == PFT[1],]
df.PFT02.shrub <- df[df$PFT == PFT[2],]
df.PFT04.shrub <- df[df$PFT == PFT[3],]
df.PFT02 <- df[df$PFT == PFT[4],]
df.PFT04 <- df[df$PFT == PFT[5],]

#For Graphing
colors <- c("C4 Grass" = "red", 
            "Acquisitive Shrub" = "light blue",
            "Conservative Shrub" = "light green", 
            "Acquisitive Tree" = "dark blue", 
            "Conservative Tree"= "dark green")



#### Get Wet Season Data ####
df.wet <- NULL
df.wet.2009 <- NULL #First year of simulated data
df.wet.2010 <- NULL #Second year of simulated data
df.wet.2011 <- NULL #Third year of simulated data

##All wet data
for(i in 1:nrow(df)){
  if(df$Month[i] == "06" | df$Month[i] == "07" | 
     df$Month[i] == "08" | df$Month[i] == "09" |
     df$Month[i] == "10" | df$Month[i] == "11" |
     df$Month[i] == "12"){
    df.wet <- rbind(df.wet, df[i,])
  }
}

##Wet data by year
#2009
for(i in 1:365){
  if(df.PFT01$Month[i] == "06" | df.PFT01$Month[i] == "07" | 
     df.PFT01$Month[i] == "08" | df.PFT01$Month[i] == "09" |
     df.PFT01$Month[i] == "10" | df.PFT01$Month[i] == "11" |
     df.PFT01$Month[i] == "12"){
    df.wet.2009 <- rbind(df.wet.2009, df.PFT01[i,], df.PFT02.shrub[i,],df.PFT04.shrub[i,],
                    df.PFT02[i,],df.PFT04[i,])
  }
}

#2010
for(i in 366:730){
  if(df.PFT01$Month[i] == "06" | df.PFT01$Month[i] == "07" | 
     df.PFT01$Month[i] == "08" | df.PFT01$Month[i] == "09" |
     df.PFT01$Month[i] == "10" | df.PFT01$Month[i] == "11" |
     df.PFT01$Month[i] == "12"){
    df.wet.2010 <- rbind(df.wet.2010, df.PFT01[i,], df.PFT02.shrub[i,],df.PFT04.shrub[i,],
                         df.PFT02[i,],df.PFT04[i,])
  }
}

#2011
for(i in 731:1095){
  if(df.PFT01$Month[i] == "06" | df.PFT01$Month[i] == "07" | 
     df.PFT01$Month[i] == "08" | df.PFT01$Month[i] == "09" |
     df.PFT01$Month[i] == "10" | df.PFT01$Month[i] == "11" |
     df.PFT01$Month[i] == "12"){
    df.wet.2011 <- rbind(df.wet.2011, df.PFT01[i,], df.PFT02.shrub[i,],df.PFT04.shrub[i,],
                         df.PFT02[i,],df.PFT04[i,])
  }
}

#Removing Shrubs
df.wet.no.shrubs <- df.wet[(df.wet$PFT != PFT[2]),]
df.wet.no.shrubs <- df.wet.no.shrubs[df.wet.no.shrubs$PFT != PFT[3],]

means <- matrix(c(mean(df.wet.no.shrubs[df.wet.no.shrubs$PFT == PFT[1],3]),
                  mean(df.wet.no.shrubs[df.wet.no.shrubs$PFT == PFT[4],3]),
                  mean(df.wet.no.shrubs[df.wet.no.shrubs$PFT == PFT[5],3])),
           ncol = 3)
colnames(means) <- c("C4 Grass","Acquisitive Tree","Conservative Tree")
rownames(means) <- c("Mean Soil Water")

#For Powerpoint Slides
ggplot(df.wet.no.shrubs, aes(x=Soil.Water, color = PFT)) + geom_density(alpha=0) +
  ggtitle("Sandy Clay (Wet Year)",
          subtitle = "Probability Density Functions (June - December)") +
  xlab("Daily Soil Water (m3 water/m3 soil)") + ylab("Density") +
  scale_color_manual(values = colors,
                     breaks = group) +
   #Mean lines
   geom_vline(aes(xintercept=means[1]),
              linetype="dashed", size=2, color = "red") +
   geom_vline(aes(xintercept=means[2]),
              linetype="dashed", size=2, color = "dark blue") +
   geom_vline(aes(xintercept=means[3]),
              linetype="dashed", size=2, color = "dark green") 
  
  



#### File to save graphs ####

#pdf('PFT_SoilWater_Shifted.pdf')
pdf('SoilWaterPDF_SandyLoam_Wet.pdf')

## All Data

# # Basic histogram
# ggplot(df, aes(x=Soil.Water, color = PFT)) + geom_density(alpha=0) + 
#   ggtitle("Probability Density Functions") + 
#   xlab("Daily Soil Water (m3 water/m3 soil)") + ylab("Density") +
#   scale_color_manual(values = colors,
#                      breaks = group)
# 
# #Histogram of each PFT
# p1 <- ggplot(df[df$PFT == "C4 Grass",], aes(x = Soil.Water, group = "C4 Grass")) + 
#   geom_histogram(color = "red", fill = "red") +
#   ggtitle("C4 Grass") + 
#   xlab("Daily Soil Water (m3 water/m3 soil)") + ylab("Counts") 
# p2 <- ggplot(df[df$PFT == "Acquisitive Shrub",], aes(x = Soil.Water, group = "Acquisitive Shrub")) + 
#   geom_histogram(color = "light blue", fill = "light blue") +
#   ggtitle("Acquisitive Shrub") + 
#   xlab("Daily Soil Water (m3 water/m3 soil)") + ylab("Counts") 
# p3 <- ggplot(df[df$PFT == "Conservative Shrub",], aes(x = Soil.Water, group = "Conservative Shrub")) + 
#   geom_histogram(color = "light green", fill = "light green") +
#   ggtitle("Conservative Shrub") + 
#   xlab("Daily Soil Water (m3 water/m3 soil)") + ylab("Counts") 
# p4 <- ggplot(df[df$PFT == "Acquisitive Tree",], aes(x = Soil.Water, group = "Acquisitive Tree")) + 
#   geom_histogram(color = "dark blue", fill = "dark blue") +
#   ggtitle("Acquisitive Tree") + 
#   xlab("Daily Soil Water (m3 water/m3 soil)") + ylab("Counts") 
# p5 <- ggplot(df[df$PFT == "Conservative Tree",], aes(x = Soil.Water, group = "Conservative Tree")) + 
#   geom_histogram(color = "dark green", fill = "dark green") +
#   ggtitle("Conservative Tree") + 
#   xlab("Daily Soil Water (m3 water/m3 soil)") + ylab("Counts") 
# grid.arrange(p1, p2, p3, p4, p5, nrow = 5)


## Wet Sesaon 

#Basic histogram
 ggplot(df.wet, aes(x=Soil.Water, color = PFT)) + geom_density(alpha=0) +
   ggtitle("Probability Density Functions (June - December)") +
   xlab("Daily Soil Water (m3 water/m3 soil)") + ylab("Density") +
   scale_color_manual(values = colors,
                      breaks = group) 


# ggplot(df.wet.2009, aes(x=Soil.Water, color = PFT)) + geom_density(alpha=0) +
#   ggtitle("Probability Density Functions (June - December) for 2009") +
#   xlab("Daily Soil Water (m3 water/m3 soil)") + ylab("Density") +
#   scale_color_manual(values = colors,
#                      breaks = group)
# 
# ggplot(df.wet.2010, aes(x=Soil.Water, color = PFT)) + geom_density(alpha=0) +
#   ggtitle("Probability Density Functions (June - December) for 2010") +
#   xlab("Daily Soil Water (m3 water/m3 soil)") + ylab("Density") +
#   scale_color_manual(values = colors,
#                      breaks = group)
# 
# ggplot(df.wet.2011, aes(x=Soil.Water, color = PFT)) + geom_density(alpha=0) +
#   ggtitle("Probability Density Functions (June - December) for 2011") +
#   xlab("Daily Soil Water (m3 water/m3 soil)") + ylab("Density") +
#   scale_color_manual(values = colors,
#                      breaks = group)



# #Histogram of each PFT
# p1 <- ggplot(df.wet[df.wet$PFT == "C4 Grass",], aes(x = Soil.Water, group = "C4 Grass")) + 
#   geom_histogram(color = "red", fill = "red") +
#   ggtitle("C4 Grass (June - December)") + 
#   xlab("Daily Soil Water (m3 water/m3 soil)") + ylab("Counts") +
#   geom_vline(aes(xintercept=mean(Soil.Water), group = "C4 Grass"),
#              color="black", linetype="dashed", size=1)
# p2 <- ggplot(df.wet[df.wet$PFT == "Acquisitive Shrub",], aes(x = Soil.Water, group = "Acquisitive Shrub")) + 
#   geom_histogram(color = "light blue", fill = "light blue") +
#   ggtitle("Acquisitive Shrub (June - December)") + 
#   xlab("Daily Soil Water (m3 water/m3 soil)") + ylab("Counts") +
#   geom_vline(aes(xintercept=mean(Soil.Water), group = "Acquisitive Shrub"),
#              color="black", linetype="dashed", size=1)
# p3 <- ggplot(df.wet[df.wet$PFT == "Conservative Shrub",], aes(x = Soil.Water, group = "Conservative Shrub")) + 
#   geom_histogram(color = "light green", fill = "light green") +
#   ggtitle("Conservative Shrub (June - December)") + 
#   xlab("Daily Soil Water (m3 water/m3 soil)") + ylab("Counts") +
#   geom_vline(aes(xintercept=mean(Soil.Water), group = "Conservative Shrub"),
#              color="black", linetype="dashed", size=1)
# p4 <- ggplot(df.wet[df.wet$PFT == "Acquisitive Tree",], aes(x = Soil.Water, group = "Acquisitive Tree")) + geom_histogram(color = "dark blue", fill = "dark blue") +
#   ggtitle("Acquisitive Tree (June - December)") + 
#   xlab("Daily Soil Water (m3 water/m3 soil)") + ylab("Counts") +
#   geom_vline(aes(xintercept=mean(Soil.Water), group = "Acquisitive Tree"),
#              color="black", linetype="dashed", size=1)
# p5 <- ggplot(df.wet[df.wet$PFT == "Conservative Tree",], aes(x = Soil.Water, group = "Conservative Tree")) + 
#   geom_histogram(color = "dark green", fill = "dark green") +
#   ggtitle("Conservative Tree (June - December)") + 
#   xlab("Daily Soil Water (m3 water/m3 soil)") + ylab("Counts") +
#   geom_vline(aes(xintercept=mean(Soil.Water), group = "Conservative Tree"),
#              color="black", linetype="dashed", size=1)
# grid.arrange(p1, p2, p3, p4, p5, nrow = 5)



dev.off()