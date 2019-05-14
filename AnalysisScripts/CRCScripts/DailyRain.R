### Calculating ED2 Daily Min/Max Temperatures based on FMEAN Output ###

#Function to add new row to data frame
#(r is number of row)
insertRow <- function(existingDF, newrow, r) {
  existingDF[seq(r+1,nrow(existingDF)+1),] <- existingDF[seq(r,nrow(existingDF)),]
  existingDF[r,] <- newrow
  return(existingDF)
}



#### Load libraries ####
library(rhdf5)

#### create index for file names ####

#These variables must be input manually 
start_year = 2008 #first full year
end_year   = 2036 #last full year
nyear = end_year - start_year + 1 #number of years

files_per_year = 365*24 #24 files per day
nfile          = nyear*files_per_year  #total number of files

#Creating data frame with row for every hour to read-in data
times  = c("000000","010000","020000","030000","040000","050000","060000",
           "070000","080000","090000","100000","110000","120000","130000",
           "140000","150000","160000","170000","180000","190000","200000",
           "210000","220000","230000")

month28 = c(rep("01",24),rep("02",24),rep("03",24),rep("04",24),rep("05",24),
            rep("06",24),rep("07",24),rep("08",24),rep("09",24),rep("10",24),
            rep("11",24),rep("12",24),rep("13",24),rep("14",24),rep("15",24),
            rep("16",24),rep("17",24),rep("18",24),rep("19",24),rep("20",24),
            rep("21",24),rep("22",24),rep("23",24),rep("24",24),rep("25",24),
            rep("26",24),rep("27",24),rep("28",24))
month30 = c(rep("01",24),rep("02",24),rep("03",24),rep("04",24),rep("05",24),
            rep("06",24),rep("07",24),rep("08",24),rep("09",24),rep("10",24),
            rep("11",24),rep("12",24),rep("13",24),rep("14",24),rep("15",24),
            rep("16",24),rep("17",24),rep("18",24),rep("19",24),rep("20",24),
            rep("21",24),rep("22",24),rep("23",24),rep("24",24),rep("25",24),
            rep("26",24),rep("27",24),rep("28",24),rep("29",24),rep("30",24))
month31 =  c(rep("01",24),rep("02",24),rep("03",24),rep("04",24),rep("05",24),
             rep("06",24),rep("07",24),rep("08",24),rep("09",24),rep("10",24),
             rep("11",24),rep("12",24),rep("13",24),rep("14",24),rep("15",24),
             rep("16",24),rep("17",24),rep("18",24),rep("19",24),rep("20",24),
             rep("21",24),rep("22",24),rep("23",24),rep("24",24),rep("25",24),
             rep("26",24),rep("27",24),rep("28",24),rep("29",24),rep("30",24),
             rep("31",24))

days   = c(month31,month28,month31,month30,month31,month30,month31,month31,
           month30,month31,month30,month31)

months = c("01","02","03","04","05","06","07","08","09","10","11","12")

itime  = times
iday   = rep(days,nyear) 
imonth = rep(months,c(31*24,28*24,31*24,30*24,31*24,30*24,31*24,31*24,30*24,31*24,30*24,31*24)) 
iyear  = rep(start_year:end_year,rep(files_per_year,nyear)) 

#namindex will be used to to store information about the file name
#R should automatically repeat iyear,imonth,iday, and itime to so that nameindex is proper length
nameindex = data.frame(iyear,imonth,iday,itime)
colnames(nameindex) = c('Year','Month','Day','Time') #Files should be listed in this order

#Model correctly includes leap day, so add those in manually
i = 1
leap = 0 #number of leap years
while(i < nrow(nameindex)){
  #if division by 4 has remainder 0, it is leap year
  if(nameindex$Year[i]%%4 == 0){ 
    FEB29 = data.frame(nameindex$Year[i],"02",29,itime)
    colnames(FEB29) = c('Year','Month','Day','Time')
    
    leap = leap + 1
    
    for(j in 1:24){
      nameindex <- insertRow(nameindex,FEB29[j,],i + (59*24 + j - 1)) #Feb 29 is 60th day of year 
    }
    i = i + 366*24 #jump one year ahead
  }
  i = i + 1
}
nfile = nfile + 24*leap #adding leap day files to total files

#### load HDF5 files and read-in ####
rain = rep(0,nrow(nameindex))

for (ii in 1:nrow(nameindex)){        
  
  #Name of each individual file
  input  = paste(nameindex$Year[ii],'/PV_site1-I-',nameindex$Year[ii],'-',
                 nameindex$Month[ii],'-',nameindex$Day[ii],'-',nameindex$Time[ii],
                 '-g01.h5',sep='')
  rain[[ii]]  = h5read(input, "FMEAN_PCPG_PY") #units: mm/s
  
}

#### Calculating daily values ####
rain.daily = rep(0,nfile/24)
day = 1

i = 1
while(i < length(rain)){
  rain.daily[day] = mean(rain[i:(i+23)])*86400 #average rate across the day times seconds in a day
  day = day + 1
  i = i + 24
}



#### Output data ####
write.csv(rain.dailye, file = "DailyRain.csv") #units: mm
