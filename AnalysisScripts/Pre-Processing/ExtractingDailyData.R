#Read in data and change names accordingly 

#First year is 2008, so Jan. 1, 2009 is 367th point of data
PFT01 <- DailySoilWater01[367:(366 + 3* 365),2]
PFT02.shrub <- DailySoilWater02[367:(366 + 3* 365),2]
PFT04.shrub <- DailySoilWater04[367:(366 + 3* 365),2]

#The 6,211 should be the first day of January of the 18th year 
PFT02 <- DailySoilWater02[6211:(6210 + 3* 365),2]
PFT04 <- DailySoilWater04[6211:(6210 + 3* 365),2]

allPFTs <- cbind(PFT01, PFT02.shrub, PFT04.shrub, PFT02, PFT04)
colnames(allPFTs) <- c("C4 Grass", "Acquisitive Shrub", "Conservative Shrub",
                       "Acquisitve Tree", "Conservative Tree")

write.csv(allPFTs, file = "DailySoilWater_SandyLoam_Wet.csv") # will have to adjust file name manually

