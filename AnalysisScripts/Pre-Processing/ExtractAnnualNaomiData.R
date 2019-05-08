# Function to convert specific humidity to relative humidity
# Params: specific humidity, temperature, pressure 
# temp is in Kelvin (as outputed by model)
# pressure is in Paschals
SH2RH <- function(q, T, p){ 
  T0 = 273.16 #reference temperature 
  RH = 0.263*p*q/(exp(17.67*(T-T0)/(T - 29.65)))
  return(RH)
}

#Function to add variables; used to compute total biomass and LAI of trees of different sizes
variable_summation <- function(dat, nameindex, possible_pft){
  summed_dat = matrix(-6999,possible_pft,nrow(nameindex))
  for (i in 1:nrow(nameindex)){
    summed_dat[,i] <- rowSums(matrix(dat[[i]],nrow = possible_pft))
  }
  return(summed_dat)
}


### R-script to load HDF files ###
#Calculating averages for Naimoi's time period, June-December
m_start = 6 #June
m_end = 12 #December
length = 7

#### Load libraries ####
library(rhdf5)

#### set file location ####
indir01  = '/Users/LoganArnold2/Desktop/ED/NewSoil/BaselineSimulations/PFT01/' 
indir02  = '/Users/LoganArnold2/Desktop/ED/NewSoil/BaselineSimulations/PFT02/'  
indir04  = '/Users/LoganArnold2/Desktop/ED/NewSoil/BaselineSimulations/PFT04/' 
indir_bare = '/Users/LoganArnold2/Desktop/ED/OldSoil/New Simulations/bareRun/' #Bare not included in new soil simulations

#### create index for file names ####

#These variables must be input manually 
nfile  = 359   # number of files
nyear  = 31    # number of distinct years
first_year_files = 6; #Number of files from the first year of simulation
last_year_files = 5; #Number of files from the last year of simulation

times  = integer(nyear)
for (i in 1:nyear) {
  if (i != 1 && i != nyear){
    times[i] = 12
  }
}
times[1] = first_year_files #Number of files from first year
times[nyear] = last_year_files #Number of files from last year
YEARS = 2007:2037 #years of simulation
iyear  = rep(YEARS,times)

months = c("01","02","03","04","05","06","07","08","09","10","11","12")
imonth = rep(months,nyear - 1) #Month; repeat each month for number of days it has
imonth = imonth[-c(1:first_year_files)] #Remove months that aren't present in first year
imonth = c(imonth,months[1:last_year_files]) #Add months that are present for last year

iday   = rep('00',nfile) #Output is monthly average, so "day" is zero
itime  = rep(c('0000'),nfile) #Time of day
nameindex = data.frame(iyear,imonth,iday,itime)
colnames(nameindex) = c('Year','Month','Day','Time') #Files should be listed in this order

#### load HDF5 files and read-in ####
maxcohort1 = 149   # max. number of cohort in patch 1 to create null matrices
possible_pft = 17 #largest number of pft

#Initialize variables 
pft     = matrix(-6999,maxcohort1,nrow(nameindex))

biomass_bare = vector("list", nrow(nameindex))
biomass01 = vector("list", nrow(nameindex))
biomass02 = vector("list", nrow(nameindex))
biomass04 = vector("list", nrow(nameindex))

lai_bare = vector("list", nrow(nameindex))
lai01 = vector("list", nrow(nameindex))
lai02 = vector("list", nrow(nameindex))
lai04 = vector("list", nrow(nameindex))

soil_water_bare = matrix(-6999,20,nrow(nameindex))
soil_water01 = matrix(-6999,20,nrow(nameindex))
soil_water02 = matrix(-6999,20,nrow(nameindex))
soil_water04 = matrix(-6999,20,nrow(nameindex))

temp_soil_bare = matrix(-6999,20,nrow(nameindex))
temp_soil01 = matrix(-6999,20,nrow(nameindex))
temp_soil02 = matrix(-6999,20,nrow(nameindex))
temp_soil04 = matrix(-6999,20,nrow(nameindex))

temp_canopy_bare = matrix(-6999,1,nrow(nameindex))
temp_canopy01 = matrix(-6999,1,nrow(nameindex))
temp_canopy02 = matrix(-6999,1,nrow(nameindex))
temp_canopy04 = matrix(-6999,1,nrow(nameindex))

SH_canopy_bare = matrix(-6999,1,nrow(nameindex))
SH_canopy01 = matrix(-6999,1,nrow(nameindex))
SH_canopy02 = matrix(-6999,1,nrow(nameindex))
SH_canopy04 = matrix(-6999,1,nrow(nameindex))

pressure_canopy_bare = matrix(-6999,1,nrow(nameindex))
pressure_canopy01 = matrix(-6999,1,nrow(nameindex))
pressure_canopy02 = matrix(-6999,1,nrow(nameindex))
pressure_canopy04 = matrix(-6999,1,nrow(nameindex))

transpiration_bare = matrix(-6999,1,nrow(nameindex))
transpiration01 = matrix(-6999,1,nrow(nameindex))
transpiration02 = matrix(-6999,1,nrow(nameindex))
transpiration04 = matrix(-6999,1,nrow(nameindex))

evap_lc_bare = matrix(-6999,1,nrow(nameindex))
evap_wc_bare = matrix(-6999,1,nrow(nameindex))
evap_gc_bare = matrix(-6999,1,nrow(nameindex))
evap_bare = matrix(-6999,1,nrow(nameindex))

evap_lc01 = matrix(-6999,1,nrow(nameindex))
evap_wc01 = matrix(-6999,1,nrow(nameindex))
evap_gc01 = matrix(-6999,1,nrow(nameindex))
evap01 = matrix(-6999,1,nrow(nameindex))

evap_lc02 = matrix(-6999,1,nrow(nameindex))
evap_wc02 = matrix(-6999,1,nrow(nameindex))
evap_gc02 = matrix(-6999,1,nrow(nameindex))
evap02 = matrix(-6999,1,nrow(nameindex))

evap_lc04 = matrix(-6999,1,nrow(nameindex))
evap_wc04 = matrix(-6999,1,nrow(nameindex))
evap_gc04 = matrix(-6999,1,nrow(nameindex))
evap04 = matrix(-6999,1,nrow(nameindex))

albedo_bare        = matrix(-6999,1,nrow(nameindex))
net_radiation_bare = matrix(-6999,1,nrow(nameindex))

albedo01        = matrix(-6999,1,nrow(nameindex))
net_radiation01 = matrix(-6999,1,nrow(nameindex))

albedo02        = matrix(-6999,1,nrow(nameindex))
net_radiation02 = matrix(-6999,1,nrow(nameindex))

albedo04        = matrix(-6999,1,nrow(nameindex))
net_radiation04 = matrix(-6999,1,nrow(nameindex))



#Loop through all files
for (ii in 1:nrow(nameindex)){        
  
  #Name of each individual file
  input.bare  = paste(indir_bare,'PV_site1-E-',nameindex$Year[ii],'-',nameindex$Month[ii],'-',nameindex$Day[ii],'-',nameindex$Time[ii],'00-g01.h5',sep='')
  input.name01  = paste(indir01,'PV_site1-E-',nameindex$Year[ii],'-',nameindex$Month[ii],'-',nameindex$Day[ii],'-',nameindex$Time[ii],'00-g01.h5',sep='')
  input.name02  = paste(indir02,'PV_site1-E-',nameindex$Year[ii],'-',nameindex$Month[ii],'-',nameindex$Day[ii],'-',nameindex$Time[ii],'00-g01.h5',sep='')
  input.name04  = paste(indir04,'PV_site1-E-',nameindex$Year[ii],'-',nameindex$Month[ii],'-',nameindex$Day[ii],'-',nameindex$Time[ii],'00-g01.h5',sep='')
  
  
  #Read-in values for a particular file
  tmppft                      = h5read(input.name02, 'PFT')
  pft[1:nrow(tmppft),ii]      = tmppft
  
  biomass_bare[[ii]] = h5read(input.bare, 'AGB_PY')
  biomass01[[ii]] = h5read(input.name01, 'AGB_PY')
  biomass02[[ii]] = h5read(input.name02, 'AGB_PY')
  biomass04[[ii]] = h5read(input.name04, 'AGB_PY')
  
  lai_bare[[ii]] = h5read(input.bare, 'MMEAN_LAI_PY')
  lai01[[ii]] = h5read(input.name01, 'MMEAN_LAI_PY')
  lai02[[ii]] = h5read(input.name02, 'MMEAN_LAI_PY')
  lai04[[ii]] = h5read(input.name04, 'MMEAN_LAI_PY')
  
  soil_water_bare[,ii] = h5read(input.bare, "MMEAN_SOIL_WATER_PY")[,1]
  soil_water01[,ii] = h5read(input.name01, "MMEAN_SOIL_WATER_PY")[,1]
  soil_water02[,ii] = h5read(input.name02, "MMEAN_SOIL_WATER_PY")[,1]
  soil_water04[,ii] = h5read(input.name04, "MMEAN_SOIL_WATER_PY")[,1]
  
  temp_soil_bare[,ii] = h5read(input.bare, "MMEAN_SOIL_TEMP_PY")[,1]
  temp_soil01[,ii] = h5read(input.name01, "MMEAN_SOIL_TEMP_PY")[,1]
  temp_soil02[,ii] = h5read(input.name02, "MMEAN_SOIL_TEMP_PY")[,1]
  temp_soil04[,ii] = h5read(input.name04, "MMEAN_SOIL_TEMP_PY")[,1]
  
  temp_canopy_bare[ii] = h5read(input.bare, "MMEAN_CAN_TEMP_PY")
  temp_canopy01[ii] = h5read(input.name01, "MMEAN_CAN_TEMP_PY")
  temp_canopy02[ii] = h5read(input.name02, "MMEAN_CAN_TEMP_PY")
  temp_canopy04[ii] = h5read(input.name04, "MMEAN_CAN_TEMP_PY")
  
  SH_canopy_bare[ii] = h5read(input.bare, "MMEAN_CAN_SHV_PY")
  SH_canopy01[ii] = h5read(input.name01, "MMEAN_CAN_SHV_PY") #Specific Humidity
  SH_canopy02[ii] = h5read(input.name02, "MMEAN_CAN_SHV_PY") #Specific Humidity
  SH_canopy04[ii] = h5read(input.name04, "MMEAN_CAN_SHV_PY") #Specific Humidity
  
  pressure_canopy_bare[ii] = h5read(input.bare, "MMEAN_CAN_PRSS_PY")
  pressure_canopy01[ii] = h5read(input.name01, "MMEAN_CAN_PRSS_PY") #Pascals
  pressure_canopy02[ii] = h5read(input.name02, "MMEAN_CAN_PRSS_PY") #Pascals
  pressure_canopy04[ii] = h5read(input.name04, "MMEAN_CAN_PRSS_PY") #Pascals
  
  transpiration_bare[ii] = h5read(input.bare, "MMEAN_TRANSP_PY")
  transpiration01[ii] = h5read(input.name01, "MMEAN_TRANSP_PY")
  transpiration02[ii] = h5read(input.name02, "MMEAN_TRANSP_PY")
  transpiration04[ii] = h5read(input.name04, "MMEAN_TRANSP_PY")
  
  evap_lc_bare[ii] = h5read(input.bare, "MMEAN_VAPOR_LC_PY")
  evap_wc_bare[ii] = h5read(input.bare, "MMEAN_VAPOR_WC_PY")
  evap_gc_bare[ii] = h5read(input.bare, "MMEAN_VAPOR_GC_PY")
  evap_bare[ii] = evap_lc_bare[ii] + evap_wc_bare[ii] + evap_gc_bare[ii]
  
  evap_lc01[ii] = h5read(input.name01, "MMEAN_VAPOR_LC_PY")
  evap_wc01[ii] = h5read(input.name01, "MMEAN_VAPOR_WC_PY")
  evap_gc01[ii] = h5read(input.name01, "MMEAN_VAPOR_GC_PY")
  evap01[ii] = evap_lc01[ii] + evap_wc01[ii] + evap_gc01[ii]
  
  evap_lc02[ii] = h5read(input.name02, "MMEAN_VAPOR_LC_PY")
  evap_wc02[ii] = h5read(input.name02, "MMEAN_VAPOR_WC_PY")
  evap_gc02[ii] = h5read(input.name02, "MMEAN_VAPOR_GC_PY")
  evap02[ii] = evap_lc02[ii] + evap_wc02[ii] + evap_gc02[ii]
  
  evap_lc04[ii] = h5read(input.name04, "MMEAN_VAPOR_LC_PY")
  evap_wc04[ii] = h5read(input.name04, "MMEAN_VAPOR_WC_PY")
  evap_gc04[ii] = h5read(input.name04, "MMEAN_VAPOR_GC_PY")
  evap04[ii] = evap_lc04[ii] + evap_wc04[ii] + evap_gc04[ii]
  
  albedo_bare[ii]        = h5read(input.bare, "MMEAN_ALBEDO_PA")
  net_radiation_bare[ii] = h5read(input.bare, "MMEAN_RNET_PY")
  
  albedo01[ii]        = h5read(input.name01, "MMEAN_ALBEDO_PA")
  net_radiation01[ii] = h5read(input.name01, "MMEAN_RNET_PY")
  
  albedo02[ii]        = h5read(input.name02, "MMEAN_ALBEDO_PA")
  net_radiation02[ii] = h5read(input.name02, "MMEAN_RNET_PY")
  
  albedo04[ii]        = h5read(input.name04, "MMEAN_ALBEDO_PA")
  net_radiation04[ii] = h5read(input.name04, "MMEAN_RNET_PY")
  
}

#### Calculating biomass and LAI for all tree sizes of a given pft ####
summed_biomass_bare = variable_summation(biomass_bare,nameindex,possible_pft)
summed_biomass01 = variable_summation(biomass01,nameindex,possible_pft)
summed_biomass02 = variable_summation(biomass02,nameindex,possible_pft)
summed_biomass04 = variable_summation(biomass04,nameindex,possible_pft)

summed_lai_bare = variable_summation(lai_bare,nameindex,possible_pft)
summed_lai01 = variable_summation(lai01,nameindex,possible_pft)
summed_lai02 = variable_summation(lai02,nameindex,possible_pft)
summed_lai04 = variable_summation(lai04,nameindex,possible_pft)

#### Calculating Annual Averages ####

#Removing incomplete years
summed_biomass.yearly.bare = summed_biomass_bare[,-((nfile-last_year_files + 1):nfile)]
summed_biomass.yearly.bare = summed_biomass.yearly.bare[,-(1:first_year_files)]
summed_biomass.yearly01 = summed_biomass01[,-((nfile-last_year_files + 1):nfile)]
summed_biomass.yearly01 = summed_biomass.yearly01[,-(1:first_year_files)]
summed_biomass.yearly02 = summed_biomass02[,-((nfile-last_year_files + 1):nfile)]
summed_biomass.yearly02 = summed_biomass.yearly02[,-(1:first_year_files)]
summed_biomass.yearly04 = summed_biomass04[,-((nfile-last_year_files + 1):nfile)]
summed_biomass.yearly04 = summed_biomass.yearly04[,-(1:first_year_files)]

soil_water.yearly.bare = soil_water_bare[,-((nfile-last_year_files + 1):nfile)]
soil_water.yearly.bare = soil_water.yearly.bare[,-(1:first_year_files)]
soil_water.yearly01 = soil_water01[,-((nfile-last_year_files + 1):nfile)]
soil_water.yearly01 = soil_water.yearly01[,-(1:first_year_files)]
soil_water.yearly02 = soil_water02[,-((nfile-last_year_files + 1):nfile)]
soil_water.yearly02 = soil_water.yearly02[,-(1:first_year_files)]
soil_water.yearly04 = soil_water04[,-((nfile-last_year_files + 1):nfile)]
soil_water.yearly04 = soil_water.yearly04[,-(1:first_year_files)]

temp_soil.yearly.bare = temp_soil_bare[,-((nfile-last_year_files + 1):nfile)]
temp_soil.yearly.bare = temp_soil.yearly.bare[,-(1:first_year_files)]
temp_soil.yearly01 = temp_soil01[,-((nfile-last_year_files + 1):nfile)]
temp_soil.yearly01 = temp_soil.yearly01[,-(1:first_year_files)]
temp_soil.yearly02 = temp_soil02[,-((nfile-last_year_files + 1):nfile)]
temp_soil.yearly02 = temp_soil.yearly02[,-(1:first_year_files)]
temp_soil.yearly04 = temp_soil04[,-((nfile-last_year_files + 1):nfile)]
temp_soil.yearly04 = temp_soil.yearly04[,-(1:first_year_files)]

summed_lai.yearly.bare = summed_lai_bare[,-((nfile-last_year_files + 1):nfile)]
summed_lai.yearly.bare = summed_lai.yearly.bare[,-(1:first_year_files)]
summed_lai.yearly01 = summed_lai01[,-((nfile-last_year_files + 1):nfile)]
summed_lai.yearly01 = summed_lai.yearly01[,-(1:first_year_files)]
summed_lai.yearly02 = summed_lai02[,-((nfile-last_year_files + 1):nfile)]
summed_lai.yearly02 = summed_lai.yearly02[,-(1:first_year_files)]
summed_lai.yearly04 = summed_lai04[,-((nfile-last_year_files + 1):nfile)]
summed_lai.yearly04 = summed_lai.yearly04[,-(1:first_year_files)]


temp_canopy.yearly.bare = temp_canopy_bare[-((nfile-last_year_files + 1):nfile)]
temp_canopy.yearly.bare = t(as.matrix(temp_canopy.yearly.bare[-(1:first_year_files)]))
temp_canopy.yearly01 = temp_canopy01[-((nfile-last_year_files + 1):nfile)]
temp_canopy.yearly01 = t(as.matrix(temp_canopy.yearly01[-(1:first_year_files)]))
temp_canopy.yearly02 = temp_canopy02[-((nfile-last_year_files + 1):nfile)]
temp_canopy.yearly02 = t(as.matrix(temp_canopy.yearly02[-(1:first_year_files)]))
temp_canopy.yearly04 = temp_canopy04[-((nfile-last_year_files + 1):nfile)]
temp_canopy.yearly04 = t(as.matrix(temp_canopy.yearly04[-(1:first_year_files)]))

SH_canopy.yearly.bare = SH_canopy_bare[-((nfile-last_year_files + 1):nfile)]
SH_canopy.yearly.bare = t(as.matrix(SH_canopy.yearly.bare[-(1:first_year_files)]))
SH_canopy.yearly01 = SH_canopy01[-((nfile-last_year_files + 1):nfile)]
SH_canopy.yearly01 = t(as.matrix(SH_canopy.yearly01[-(1:first_year_files)]))
SH_canopy.yearly02 = SH_canopy02[-((nfile-last_year_files + 1):nfile)]
SH_canopy.yearly02 = t(as.matrix(SH_canopy.yearly02[-(1:first_year_files)]))
SH_canopy.yearly04 = SH_canopy04[-((nfile-last_year_files + 1):nfile)]
SH_canopy.yearly04 = t(as.matrix(SH_canopy.yearly04[-(1:first_year_files)]))

pressure_canopy.yearly.bare = pressure_canopy_bare[-((nfile-last_year_files + 1):nfile)]
pressure_canopy.yearly.bare = t(as.matrix(pressure_canopy.yearly.bare[-(1:first_year_files)]))
pressure_canopy.yearly01 = pressure_canopy01[-((nfile-last_year_files + 1):nfile)]
pressure_canopy.yearly01 = t(as.matrix(pressure_canopy.yearly01[-(1:first_year_files)]))
pressure_canopy.yearly02 = pressure_canopy02[-((nfile-last_year_files + 1):nfile)]
pressure_canopy.yearly02 = t(as.matrix(pressure_canopy.yearly02[-(1:first_year_files)]))
pressure_canopy.yearly04 = pressure_canopy04[-((nfile-last_year_files + 1):nfile)]
pressure_canopy.yearly04 = t(as.matrix(pressure_canopy.yearly04[-(1:first_year_files)]))

transpiration.yearly.bare = transpiration_bare[-((nfile-last_year_files + 1):nfile)]
transpiration.yearly.bare = t(as.matrix(transpiration.yearly.bare[-(1:first_year_files)]))
transpiration.yearly01 = transpiration01[-((nfile-last_year_files + 1):nfile)]
transpiration.yearly01 = t(as.matrix(transpiration.yearly01[-(1:first_year_files)]))
transpiration.yearly02 = transpiration02[-((nfile-last_year_files + 1):nfile)]
transpiration.yearly02 = t(as.matrix(transpiration.yearly02[-(1:first_year_files)]))
transpiration.yearly04 = transpiration04[-((nfile-last_year_files + 1):nfile)]
transpiration.yearly04 = t(as.matrix(transpiration.yearly04[-(1:first_year_files)]))

evap.yearly.bare = evap_bare[-((nfile-last_year_files + 1):nfile)]
evap.yearly.bare =  t(as.matrix(evap.yearly.bare[-(1:first_year_files)]))
evap.yearly01 = evap01[-((nfile-last_year_files + 1):nfile)]
evap.yearly01 =  t(as.matrix(evap.yearly01[-(1:first_year_files)]))
evap.yearly02 = evap02[-((nfile-last_year_files + 1):nfile)]
evap.yearly02 =  t(as.matrix(evap.yearly02[-(1:first_year_files)]))
evap.yearly04 = evap04[-((nfile-last_year_files + 1):nfile)]
evap.yearly04 =  t(as.matrix(evap.yearly04[-(1:first_year_files)]))

albedo.yearly.bare = albedo_bare[-((nfile-last_year_files + 1):nfile)]
albedo.yearly.bare = t(as.matrix(albedo.yearly.bare[-(1:first_year_files)]))
albedo.yearly01 = albedo01[-((nfile-last_year_files + 1):nfile)]
albedo.yearly01 = t(as.matrix(albedo.yearly01[-(1:first_year_files)]))
albedo.yearly02 = albedo02[-((nfile-last_year_files + 1):nfile)]
albedo.yearly02 = t(as.matrix(albedo.yearly02[-(1:first_year_files)]))
albedo.yearly04 = albedo04[-((nfile-last_year_files + 1):nfile)]
albedo.yearly04 = t(as.matrix(albedo.yearly04[-(1:first_year_files)]))

net_radiation.yearly.bare = net_radiation_bare[-((nfile-last_year_files + 1):nfile)]
net_radiation.yearly.bare = t(as.matrix(net_radiation.yearly.bare[-(1:first_year_files)]))
net_radiation.yearly01 = net_radiation01[-((nfile-last_year_files + 1):nfile)]
net_radiation.yearly01 = t(as.matrix(net_radiation.yearly01[-(1:first_year_files)]))
net_radiation.yearly02 = net_radiation02[-((nfile-last_year_files + 1):nfile)]
net_radiation.yearly02 = t(as.matrix(net_radiation.yearly02[-(1:first_year_files)]))
net_radiation.yearly04 = net_radiation04[-((nfile-last_year_files + 1):nfile)]
net_radiation.yearly04 = t(as.matrix(net_radiation.yearly04[-(1:first_year_files)]))

#Convert relative humidity to specific humidity
RH_canopy.yearly.bare <- SH2RH(SH_canopy.yearly.bare,temp_canopy.yearly.bare,pressure_canopy.yearly.bare)
RH_canopy.yearly01 <- SH2RH(SH_canopy.yearly01,temp_canopy.yearly01,pressure_canopy.yearly01)
RH_canopy.yearly02 <- SH2RH(SH_canopy.yearly02,temp_canopy.yearly02,pressure_canopy.yearly02)
RH_canopy.yearly04 <- SH2RH(SH_canopy.yearly04,temp_canopy.yearly04,pressure_canopy.yearly04)

#Calculating annual average
annual_average.soil_water.bare <- matrix(-6999,20,nyear-2)
annual_average.soil_water01 <- matrix(-6999,20,nyear-2)
annual_average.soil_water02 <- matrix(-6999,20,nyear-2)
annual_average.soil_water04 <- matrix(-6999,20,nyear-2)

annual_average.temp_soil.bare <- matrix(-6999,20,nyear-2)
annual_average.temp_soil01 <- matrix(-6999,20,nyear-2)
annual_average.temp_soil02 <- matrix(-6999,20,nyear-2)
annual_average.temp_soil04 <- matrix(-6999,20,nyear-2)


m = m_start #starting month
for (i in 1:ncol(annual_average.soil_water02)){
  for (j in 1:nrow(annual_average.soil_water02)){
    annual_average.soil_water.bare[j,i] = sum(soil_water.yearly.bare[j,m:(m+length-1)])/length
    annual_average.soil_water01[j,i] = sum(soil_water.yearly01[j,m:(m+length-1)])/length
    annual_average.soil_water02[j,i] = sum(soil_water.yearly02[j,m:(m+length-1)])/length
    annual_average.soil_water04[j,i] = sum(soil_water.yearly04[j,m:(m+length-1)])/length
    
    annual_average.temp_soil.bare[j,i] = sum(temp_soil.yearly.bare[j,m:(m+length-1)])/length
    annual_average.temp_soil01[j,i] = sum(temp_soil.yearly01[j,m:(m+length-1)])/length
    annual_average.temp_soil02[j,i] = sum(temp_soil.yearly02[j,m:(m+length-1)])/length
    annual_average.temp_soil04[j,i] = sum(temp_soil.yearly04[j,m:(m+length-1)])/length
    
  }
  m = m + 12
}

annual_average.biomass.bare <- matrix(-6999,17,nyear-2)
annual_average.biomass01 <- matrix(-6999,17,nyear-2)
annual_average.biomass02 <- matrix(-6999,17,nyear-2)
annual_average.biomass04 <- matrix(-6999,17,nyear-2)

annual_average.lai.bare <- matrix(-6999,17,nyear-2)
annual_average.lai01 <- matrix(-6999,17,nyear-2)
annual_average.lai02 <- matrix(-6999,17,nyear-2)
annual_average.lai04 <- matrix(-6999,17,nyear-2)

m = m_start #starting month
for (i in 1:ncol(annual_average.lai02 )){
  for (j in 1:nrow(annual_average.lai02 )){
    annual_average.biomass.bare[j,i] = sum(summed_biomass.yearly.bare[j,m:(m+length-1)])/length
    annual_average.biomass01[j,i] = sum(summed_biomass.yearly01[j,m:(m+length-1)])/length
    annual_average.biomass02[j,i] = sum(summed_biomass.yearly02[j,m:(m+length-1)])/length
    annual_average.biomass04[j,i] = sum(summed_biomass.yearly04[j,m:(m+length-1)])/length
    
    annual_average.lai.bare[j,i] = sum(summed_lai.yearly.bare[j,m:(m+length-1)])/length
    annual_average.lai01[j,i] = sum(summed_lai.yearly01[j,m:(m+length-1)])/length
    annual_average.lai02[j,i] = sum(summed_lai.yearly02[j,m:(m+length-1)])/length
    annual_average.lai04[j,i] = sum(summed_lai.yearly04[j,m:(m+length-1)])/length
  }
  m = m + 12
}

annual_average.temp_canpoy.bare <- matrix(-6999,1,nyear-2)
annual_average.temp_canpoy01 <- matrix(-6999,1,nyear-2)
annual_average.temp_canpoy02 <- matrix(-6999,1,nyear-2)
annual_average.temp_canpoy04 <- matrix(-6999,1,nyear-2)

annual_average.SH_canopy.bare <- matrix(-6999,1,nyear-2)
annual_average.SH_canopy01 <- matrix(-6999,1,nyear-2)
annual_average.SH_canopy02 <- matrix(-6999,1,nyear-2)
annual_average.SH_canopy04 <- matrix(-6999,1,nyear-2)

annual_average.RH_canopy.bare <- matrix(-6999,1,nyear-2)
annual_average.RH_canopy01 <- matrix(-6999,1,nyear-2)
annual_average.RH_canopy02 <- matrix(-6999,1,nyear-2)
annual_average.RH_canopy04 <- matrix(-6999,1,nyear-2)

annual_average.transpiration.bare <- matrix(-6999,1,nyear-2)
annual_average.transpiration01 <- matrix(-6999,1,nyear-2)
annual_average.transpiration02 <- matrix(-6999,1,nyear-2)
annual_average.transpiration04 <- matrix(-6999,1,nyear-2)

annual_average.evap.bare <- matrix(-6999,1,nyear-2)
annual_average.evap01 <- matrix(-6999,1,nyear-2)
annual_average.evap02 <- matrix(-6999,1,nyear-2)
annual_average.evap04 <- matrix(-6999,1,nyear-2)

annual_average.albedo.bare <- matrix(-6999,1,nyear-2)
annual_average.albedo01 <- matrix(-6999,1,nyear-2)
annual_average.albedo02 <- matrix(-6999,1,nyear-2)
annual_average.albedo04 <- matrix(-6999,1,nyear-2)

annual_average.net_radiation.bare <- matrix(-6999,1,nyear-2)
annual_average.net_radiation01 <- matrix(-6999,1,nyear-2)
annual_average.net_radiation02 <- matrix(-6999,1,nyear-2)
annual_average.net_radiation04 <- matrix(-6999,1,nyear-2)

m = m_start #starting month
for (j in 1:ncol(annual_average.temp_canpoy02)){
  
  annual_average.temp_canpoy.bare[j] = sum(temp_canopy.yearly.bare[1,m:(m+length-1)])/length
  annual_average.temp_canpoy01[j] = sum(temp_canopy.yearly01[1,m:(m+length-1)])/length
  annual_average.temp_canpoy02[j] = sum(temp_canopy.yearly02[1,m:(m+length-1)])/length
  annual_average.temp_canpoy04[j] = sum(temp_canopy.yearly04[1,m:(m+length-1)])/length
  
  annual_average.SH_canopy.bare[j] = sum(SH_canopy.yearly.bare[1,m:(m+length-1)])/length
  annual_average.SH_canopy01[j] = sum(SH_canopy.yearly01[1,m:(m+length-1)])/length
  annual_average.SH_canopy02[j] = sum(SH_canopy.yearly02[1,m:(m+length-1)])/length
  annual_average.SH_canopy04[j] = sum(SH_canopy.yearly04[1,m:(m+length-1)])/length
  
  annual_average.RH_canopy.bare[j] = sum(RH_canopy.yearly.bare[1,m:(m+length-1)])/length
  annual_average.RH_canopy01[j] = sum(RH_canopy.yearly01[1,m:(m+length-1)])/length
  annual_average.RH_canopy02[j] = sum(RH_canopy.yearly02[1,m:(m+length-1)])/length
  annual_average.RH_canopy04[j] = sum(RH_canopy.yearly04[1,m:(m+length-1)])/length
  
  annual_average.transpiration.bare[j] = sum(transpiration.yearly.bare[1,m:(m+length-1)])/length
  annual_average.transpiration01[j] = sum(transpiration.yearly01[1,m:(m+length-1)])/length
  annual_average.transpiration02[j] = sum(transpiration.yearly02[1,m:(m+length-1)])/length
  annual_average.transpiration04[j] = sum(transpiration.yearly04[1,m:(m+length-1)])/length
  
  annual_average.evap.bare[j] = sum(evap.yearly.bare[1,m:(m+length-1)])/length
  annual_average.evap01[j] = sum(evap.yearly01[1,m:(m+length-1)])/length
  annual_average.evap02[j] = sum(evap.yearly02[1,m:(m+length-1)])/length
  annual_average.evap04[j] = sum(evap.yearly04[1,m:(m+length-1)])/length
  
  annual_average.albedo.bare[j] = sum(albedo.yearly.bare[1,m:(m+length-1)])/length
  annual_average.albedo01[j] = sum(albedo.yearly01[1,m:(m+length-1)])/length
  annual_average.albedo02[j] = sum(albedo.yearly02[1,m:(m+length-1)])/length
  annual_average.albedo04[j] = sum(albedo.yearly04[1,m:(m+length-1)])/length
  
  annual_average.net_radiation.bare[j] = sum(net_radiation.yearly.bare[1,m:(m+length-1)])/length
  annual_average.net_radiation01[j] = sum(net_radiation.yearly01[1,m:(m+length-1)])/length
  annual_average.net_radiation02[j] = sum(net_radiation.yearly02[1,m:(m+length-1)])/length
  annual_average.net_radiation04[j] = sum(net_radiation.yearly04[1,m:(m+length-1)])/length
  
  m = m + 12
}

#Convert temperature from Kelvin to degrees Celsius 
annual_average.temp_canpoy.bare = annual_average.temp_canpoy.bare - 273
annual_average.temp_canpoy01 = annual_average.temp_canpoy01 - 273
annual_average.temp_canpoy02 = annual_average.temp_canpoy02 - 273
annual_average.temp_canpoy04 = annual_average.temp_canpoy04 - 273

annual_average.temp_soil.bare = annual_average.temp_soil.bare - 273
annual_average.temp_soil01 = annual_average.temp_soil01 - 273
annual_average.temp_soil02 = annual_average.temp_soil02 - 273
annual_average.temp_soil04 = annual_average.temp_soil04 - 273


#Convert transpiration and evaporation from kg/m2/s to mm/day
annual_average.transpiration.bare = annual_average.transpiration.bare*86400
annual_average.transpiration01 = annual_average.transpiration01*86400
annual_average.transpiration02 = annual_average.transpiration02*86400
annual_average.transpiration04 = annual_average.transpiration04*86400

annual_average.evap.bare = annual_average.evap.bare*86400
annual_average.evap01 = annual_average.evap01*86400
annual_average.evap02 = annual_average.evap02*86400
annual_average.evap04 = annual_average.evap04*86400

#Get min/max temperatures
MinMax_file <- paste(indir_bare,'MinMaxBare.month.csv',sep='')
MinMax_data00 <- read.csv(MinMax_file)
MinMax_file <- paste(indir01,'MinMax01.month.csv',sep='')
MinMax_data01 <- read.csv(MinMax_file)
MinMax_file <- paste(indir02,'MinMax02.month.csv',sep='')
MinMax_data02 <- read.csv(MinMax_file)
MinMax_file <- paste(indir04,'MinMax04.month.csv',sep='')
MinMax_data04 <- read.csv(MinMax_file)

annual_average.temp.min.bare <- matrix(-6999,1,nyear-2)
annual_average.temp.min01 <- matrix(-6999,1,nyear-2)
annual_average.temp.min02 <- matrix(-6999,1,nyear-2)
annual_average.temp.min04 <- matrix(-6999,1,nyear-2)

annual_average.temp.max.bare <- matrix(-6999,1,nyear-2)
annual_average.temp.max01 <- matrix(-6999,1,nyear-2)
annual_average.temp.max02 <- matrix(-6999,1,nyear-2)
annual_average.temp.max04 <- matrix(-6999,1,nyear-2)


m = m_start #starting month
for (j in 1:ncol(annual_average.temp_canpoy02)){
  
  annual_average.temp.min.bare[j] <- sum(MinMax_data00$Average.Monthly.Min.Temperature[m:(m+length-1)])/length - 273
  annual_average.temp.min01[j] <- sum(MinMax_data01$Average.Monthly.Min.Temperature[m:(m+length-1)])/length - 273
  annual_average.temp.min02[j] <- sum(MinMax_data02$Average.Monthly.Min.Temperature[m:(m+length-1)])/length - 273
  annual_average.temp.min04[j] <- sum(MinMax_data04$Average.Monthly.Min.Temperature[m:(m+length-1)])/length - 273
  
  annual_average.temp.max.bare[j] <- sum(MinMax_data00$Average.Monthly.Max.Temperature[m:(m+length-1)])/length - 273
  annual_average.temp.max01[j] <- sum(MinMax_data01$Average.Monthly.Max.Temperature[m:(m+length-1)])/length - 273
  annual_average.temp.max02[j] <- sum(MinMax_data02$Average.Monthly.Max.Temperature[m:(m+length-1)])/length - 273
  annual_average.temp.max04[j] <- sum(MinMax_data04$Average.Monthly.Max.Temperature[m:(m+length-1)])/length - 273
  
  m = m + 12
}


####Creating Data Frames####

years = 1:length(annual_average.biomass02[1,])

PFT00.Naomi = data.frame(years, 
                       PFT = 0,
                       BIOMASS = annual_average.biomass.bare[1,],
                       LAI = annual_average.lai.bare[1,],
                       TEMP = annual_average.temp_canpoy.bare[,],
                       TEMP_MIN = annual_average.temp.min.bare[,],
                       TEMP_MAX = annual_average.temp.max.bare[,],
                       TEMP_DIFF = annual_average.temp_canpoy.bare[,] - annual_average.temp_canpoy.bare[,],
                       RH = annual_average.RH_canopy.bare[,],
                       RH_DIFF = annual_average.RH_canopy.bare[,] - annual_average.RH_canopy.bare[,],
                       TRANSP = annual_average.transpiration.bare[,],
                       EVAP = annual_average.evap.bare[,],
                       ALBEDO = annual_average.albedo.bare[,],
                       NET.RADIATION = annual_average.net_radiation.bare[,],
                       SOIL_WATER = t(annual_average.soil_water.bare),
                       SOIL_WATER_DIFF = t(annual_average.soil_water.bare - annual_average.soil_water.bare),
                       SOIL_TEMP = t(annual_average.temp_soil.bare),
                       SOIL_TEMP_DIFF = t(annual_average.temp_soil.bare - annual_average.temp_soil.bare))

PFT01.Naomi = data.frame(years, 
                       PFT = 1,
                       BIOMASS = annual_average.biomass01[1,],
                       LAI = annual_average.lai01[1,],
                       TEMP = annual_average.temp_canpoy01[,],
                       TEMP_MIN = annual_average.temp.min01[,],
                       TEMP_MAX = annual_average.temp.max01[,],
                       TEMP_DIFF = annual_average.temp_canpoy01[,] - annual_average.temp_canpoy.bare[,],
                       RH = annual_average.RH_canopy01[,],
                       RH_DIFF = annual_average.RH_canopy01[,] - annual_average.RH_canopy.bare[,],
                       TRANSP = annual_average.transpiration01[,],
                       EVAP = annual_average.evap01[,],
                       ALBEDO = annual_average.albedo01[,],
                       NET.RADIATION = annual_average.net_radiation01[,],
                       SOIL_WATER = t(annual_average.soil_water01),
                       SOIL_WATER_DIFF = t(annual_average.soil_water01 - annual_average.soil_water.bare),
                       SOIL_TEMP = t(annual_average.temp_soil01),
                       SOIL_TEMP_DIFF = t(annual_average.temp_soil01 - annual_average.temp_soil.bare))

PFT02.Naomi = data.frame(years, 
                       PFT = 2,
                       BIOMASS = annual_average.biomass02[2,],
                       LAI = annual_average.lai02[2,],
                       TEMP = annual_average.temp_canpoy02[,],
                       TEMP_MIN = annual_average.temp.min02[,],
                       TEMP_MAX = annual_average.temp.max02[,],
                       TEMP_DIFF = annual_average.temp_canpoy02[,] - annual_average.temp_canpoy.bare[,],
                       RH = annual_average.RH_canopy02[,],
                       RH_DIFF = annual_average.RH_canopy02[,] - annual_average.RH_canopy.bare[,],
                       TRANSP = annual_average.transpiration02[,],
                       EVAP = annual_average.evap02[,],
                       ALBEDO = annual_average.albedo02[,],
                       NET.RADIATION = annual_average.net_radiation02[,],
                       SOIL_WATER = t(annual_average.soil_water02),
                       SOIL_WATER_DIFF = t(annual_average.soil_water02 - annual_average.soil_water.bare),
                       SOIL_TEMP = t(annual_average.temp_soil02),
                       SOIL_TEMP_DIFF = t(annual_average.temp_soil02 - annual_average.temp_soil.bare))

PFT04.Naomi = data.frame(years, 
                       PFT = 4,
                       BIOMASS = annual_average.biomass04[4,],
                       LAI = annual_average.lai04[4,],
                       TEMP = annual_average.temp_canpoy04[,],
                       TEMP_MIN = annual_average.temp.min04[,],
                       TEMP_MAX = annual_average.temp.max04[,],
                       TEMP_DIFF = annual_average.temp_canpoy04[,] - annual_average.temp_canpoy.bare[,],
                       RH = annual_average.RH_canopy04[,],
                       RH_DIFF = annual_average.RH_canopy04[,] - annual_average.RH_canopy.bare[,],
                       TRANSP = annual_average.transpiration04[,],
                       EVAP = annual_average.evap04[,],
                       ALBEDO = annual_average.albedo04[,],
                       NET.RADIATION = annual_average.net_radiation04[,],
                       SOIL_WATER = t(annual_average.soil_water04),
                       SOIL_WATER_DIFF = t(annual_average.soil_water04 - annual_average.soil_water.bare),
                       SOIL_TEMP = t(annual_average.temp_soil04),
                       SOIL_TEMP_DIFF = t(annual_average.temp_soil04 - annual_average.temp_soil.bare))

#save(PFT00.Naomi, file = "PFT00.Naomi.Rda")
save(PFT01.Naomi, file = "PFT01.Naomi.Rda")
save(PFT02.Naomi, file = "PFT02.Naomi.Rda")
save(PFT04.Naomi, file = "PFT04.Naomi.Rda")