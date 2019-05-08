####Load Data ####
load("PFT01.Rda")
load("PFT02.Rda")
load("PFT04.Rda")

library("ggplot2")

####Finding Averages

PFT01.avg <- rep(0,ncol(PFT01))
PFT02.avg <- rep(0,ncol(PFT02))
PFT02.avg.shrub <- rep(0,ncol(PFT02))
PFT04.avg <- rep(0,ncol(PFT04))
PFT04.avg.shrub <- rep(0,ncol(PFT04))

#grass and shrubs 
for(i in 1:ncol(PFT01)){
  PFT01.avg[i] = mean(PFT01[2:4,i])
  PFT02.avg.shrub[i] = mean(PFT02[2:4,i])
  PFT04.avg.shrub[i] = mean(PFT04[2:4,i])
}

#Early and late
for(i in 1:ncol(PFT01)){
  PFT02.avg[i] = mean(PFT02[18:20,i])
  PFT04.avg[i] = mean(PFT04[18:20,i])
}

PFT01.avg <- data.frame(t(PFT01.avg))
colnames(PFT01.avg) <- colnames(PFT01)
PFT01.avg$PFT <- "C4 Grass"

PFT02.avg <- data.frame(t(PFT02.avg))
colnames(PFT02.avg) <- colnames(PFT02)
PFT02.avg$PFT <- "Acquisitive Tree"

PFT02.avg.shrub <- data.frame(t(PFT02.avg.shrub))
colnames(PFT02.avg.shrub) <- colnames(PFT02)
PFT02.avg.shrub$PFT <- "Acquisitive Shrub"

PFT04.avg <- data.frame(t(PFT04.avg))
colnames(PFT04.avg) <- colnames(PFT04)
PFT04.avg$PFT <- "Conservative Tree"

PFT04.avg.shrub <- data.frame(t(PFT04.avg.shrub))
colnames(PFT04.avg.shrub) <- colnames(PFT04)
PFT04.avg.shrub$PFT <- "Conservative Shrub"

####Graping ####

#Bringing everything together
allPFTs <- rbind(PFT01.avg,
                 PFT02.avg.shrub,
                 PFT02.avg,
                 PFT04.avg.shrub,
                 PFT04.avg)
SuccessionOrder <- c(1,2,4,3,5)
allPFTs <- cbind(allPFTs,SuccessionOrder)



colors <- c("red", "light blue", "light green", "dark blue", "dark green")


pdf('PFT_BarGraphs.pdf')

#Biomass
ggplot(data = allPFTs, aes(x = reorder(PFT,SuccessionOrder), y = BIOMASS)) +
  geom_bar(stat="identity", color = colors, fill = colors) +
  xlab('PFT') + ylab('Biomass (kg C/m2)') +
  ggtitle('Above Ground Biomass', 
          subtitle = 'Average of Years 2-4 (Shrubs, Grass) or Years 18-20 (Trees)') 


#LAI
ggplot(data = allPFTs, aes(x = reorder(PFT,SuccessionOrder), y = LAI)) +
  geom_bar(stat="identity", color = colors, fill = colors) +
  coord_cartesian(ylim=c(0,2.5)) +
  xlab('PFT') + ylab('LAI (m2 leaf/m2 ground)') +
  ggtitle('Total LAI',
          subtitle = 'Average of Years 2-4 (Shrubs, Grass) or Years 18-20 (Trees)')

#Canopy Temp
ggplot(data = allPFTs, aes(x = reorder(PFT,SuccessionOrder), y = TEMP)) +
  geom_bar(stat="identity", color = colors, fill = colors) +
  coord_cartesian(ylim=c(25,30)) +
  xlab('PFT') + ylab('Temperature (degrees C)') +
  ggtitle('Canopy Temperature (Average Values)',
          subtitle = 'Average of Years 2-4 (Shrubs, Grass) or Years 18-20 (Trees)')

#Min temp
ggplot(data = allPFTs, aes(x = reorder(PFT,SuccessionOrder), y = TEMP_MIN)) +
  geom_bar(stat="identity", color = colors, fill = colors) +
  coord_cartesian(ylim=c(22,25)) +
  xlab('PFT') + ylab('Temperature (degrees C)') +
  ggtitle('Canopy Temperature (Minimum Values)',
          subtitle = 'Average of Years 2-4 (Shrubs, Grass) or Years 18-20 (Trees)')

#Max temp
ggplot(data = allPFTs, aes(x = reorder(PFT,SuccessionOrder), y = TEMP_MAX)) +
  geom_bar(stat="identity", color = colors, fill = colors) +
  coord_cartesian(ylim=c(32,37)) +
  xlab('PFT') + ylab('Temperature (degrees C)') +
  ggtitle('Canopy Temperature (Maximum Values)',
          subtitle = 'Average of Years 2-4 (Shrubs, Grass) or Years 18-20 (Trees)')

#Relative Humidity
ggplot(data = allPFTs, aes(x = reorder(PFT,SuccessionOrder), y = RH)) +
  geom_bar(stat="identity", color = colors, fill = colors) +
  coord_cartesian(ylim=c(65,75)) +
  xlab('PFT') + ylab('Relative Humidity') +
  ggtitle('Canopy Relative Humidity',
          subtitle = 'Average of Years 2-4 (Shrubs, Grass) or Years 18-20 (Trees)')

##Soil Water##

#Layer 20
ggplot(data = allPFTs, aes(x = reorder(PFT,SuccessionOrder), y = SOIL_WATER.20)) +
  geom_bar(stat="identity", color = colors, fill = colors) +
  coord_cartesian(ylim=c(.25,.3)) +
  xlab('PFT') + ylab('Soil Water (m3 water/m3 soil)') +
  ggtitle('Soil Water for Layer 20 (Top)', 
          subtitle = 'Average of Years 2-4 (Shrubs, Grass) or Years 18-20 (Trees)')

#Layer 15
ggplot(data = allPFTs, aes(x = reorder(PFT,SuccessionOrder), y = SOIL_WATER.15)) +
  geom_bar(stat="identity", color = colors, fill = colors) +
  coord_cartesian(ylim=c(.28,.33)) +
  xlab('PFT') + ylab('Soil Water (m3 water/m3 soil)') +
  ggtitle('Soil Water for Layer 15', 
          subtitle = 'Average of Years 2-4 (Shrubs, Grass) or Years 18-20 (Trees)')

#Layer 10
ggplot(data = allPFTs, aes(x = reorder(PFT,SuccessionOrder), y = SOIL_WATER.10)) +
  geom_bar(stat="identity", color = colors, fill = colors) +
  coord_cartesian(ylim=c(.3,.34)) +
  xlab('PFT') + ylab('Soil Water (m3 water/m3 soil)') +
  ggtitle('Soil Water for Layer 10', 
          subtitle = 'Average of Years 2-4 (Shrubs, Grass) or Years 18-20 (Trees)')

#Layer 05
ggplot(data = allPFTs, aes(x = reorder(PFT,SuccessionOrder), y = SOIL_WATER.5)) +
  geom_bar(stat="identity", color = colors, fill = colors) +
  coord_cartesian(ylim=c(.31,.34)) +
  xlab('PFT') + ylab('Soil Water (m3 water/m3 soil)') +
  ggtitle('Soil Water for Layer 5', 
          subtitle = 'Average of Years 2-4 (Shrubs, Grass) or Years 18-20 (Trees)')

#Layer 1
ggplot(data = allPFTs, aes(x = reorder(PFT,SuccessionOrder), y = SOIL_WATER.1)) +
  geom_bar(stat="identity", color = colors, fill = colors) +
  coord_cartesian(ylim=c(.31,.34)) +
  xlab('PFT') + ylab('Soil Water (m3 water/m3 soil)') +
  ggtitle('Soil Water for Layer 1 (Bottom)', 
          subtitle = 'Average of Years 2-4 (Shrubs, Grass) or Years 18-20 (Trees)')

##Soil Temperature##

#Layer 20
ggplot(data = allPFTs, aes(x = reorder(PFT,SuccessionOrder), y = SOIL_TEMP.20)) +
  geom_bar(stat="identity", color = colors, fill = colors) +
  coord_cartesian(ylim=c(27,34)) +
  xlab('PFT') + ylab('Soil Temperature (degrees C)') +
  ggtitle('Soil Temperature for Layer 20 (Top)', 
          subtitle = 'Average of Years 2-4 (Shrubs, Grass) or Years 18-20 (Trees)')

#Layer 15
ggplot(data = allPFTs, aes(x = reorder(PFT,SuccessionOrder), y = SOIL_TEMP.15)) +
  geom_bar(stat="identity", color = colors, fill = colors) +
  coord_cartesian(ylim=c(27,34)) +
  xlab('PFT') + ylab('Soil Temperature (degrees C)') +
  ggtitle('Soil Temperature for Layer 15', 
          subtitle = 'Average of Years 2-4 (Shrubs, Grass) or Years 18-20 (Trees)')

#Layer 10
ggplot(data = allPFTs, aes(x = reorder(PFT,SuccessionOrder), y = SOIL_TEMP.10)) +
  geom_bar(stat="identity", color = colors, fill = colors) +
  coord_cartesian(ylim=c(27,34)) +
  xlab('PFT') + ylab('Soil Temperature (degrees C)') +
  ggtitle('Soil Temperature for Layer 10', 
          subtitle = 'Average of Years 2-4 (Shrubs, Grass) or Years 18-20 (Trees)')

#Layer 05
ggplot(data = allPFTs, aes(x = reorder(PFT,SuccessionOrder), y = SOIL_TEMP.5)) +
  geom_bar(stat="identity", color = colors, fill = colors) +
  coord_cartesian(ylim=c(27,34)) +
  xlab('PFT') + ylab('Soil Temperature (degrees C)') +
  ggtitle('Soil Temperature for Layer 5', 
          subtitle = 'Average of Years 2-4 (Shrubs, Grass) or Years 18-20 (Trees)')

#Layer 1
ggplot(data = allPFTs, aes(x = reorder(PFT,SuccessionOrder), y = SOIL_TEMP.1)) +
  geom_bar(stat="identity", color = colors, fill = colors) +
  coord_cartesian(ylim=c(27,34)) +
  xlab('PFT') + ylab('Soil Temperature (degrees C)') +
  ggtitle('Soil Temperature for Layer 1 (Bottom)', 
          subtitle = 'Average of Years 2-4 (Shrubs, Grass) or Years 18-20 (Trees)')

#Transpiration
ggplot(data = allPFTs, aes(x = reorder(PFT,SuccessionOrder), y = TRANSP)) +
  geom_bar(stat="identity", color = colors, fill = colors) +
  xlab('PFT') + ylab('Transpiration (mm/day)') +
  ggtitle('Transpiration',
          subtitle = 'Average of Years 2-4 (Shrubs, Grass) or Years 18-20 (Trees)')

#Evaporation
ggplot(data = allPFTs, aes(x = reorder(PFT,SuccessionOrder), y = EVAP)) +
  geom_bar(stat="identity", color = colors, fill = colors) +
  coord_cartesian(ylim=c(1,2)) +
  xlab('PFT') + ylab('Evaporation (mm/day)') +
  ggtitle('Evaporation',
          subtitle = 'Average of Years 2-4 (No Veg., Grass) or Years 18-20 (Early, Late)')

#Albedo
ggplot(data = allPFTs, aes(x = reorder(PFT,SuccessionOrder), y = ALBEDO)) +
  geom_bar(stat="identity", color = colors, fill = colors) +
  #coord_cartesian(ylim=c(1,2.5)) +
  xlab('PFT') + ylab('Albedo') +
  ggtitle('Albedo',
          subtitle = 'Average of Years 2-4 (No Veg., Grass) or Years 18-20 (Early, Late)')

#Net Radiation
ggplot(data = allPFTs, aes(x = reorder(PFT,SuccessionOrder), y = NET.RADIATION)) +
  geom_bar(stat="identity", color = colors, fill = colors) +
  coord_cartesian(ylim=c(90,140)) +
  xlab('PFT') + ylab('Net Radiation (W/m2)') +
  ggtitle('Net Radiation',
          subtitle = 'Average of Years 2-4 (No Veg., Grass) or Years 18-20 (Early, Late)')


dev.off()
