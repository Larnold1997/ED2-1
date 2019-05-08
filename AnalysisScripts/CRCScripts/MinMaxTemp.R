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
temp = rep(0,nrow(nameindex)) #temperature at each hour

for (ii in 1:nrow(nameindex)){        
  
  #Name of each individual file
  input  = paste(nameindex$Year[ii],'/PV_site1-I-',nameindex$Year[ii],'-',
                 nameindex$Month[ii],'-',nameindex$Day[ii],'-',nameindex$Time[ii],
                 '-g01.h5',sep='')
  temp[[ii]] = h5read(input, "FMEAN_CAN_TEMP_PY")
  
}

#### Calculating daily min and max temperatures ####
dmax_temp = rep(0,nfile/24)
dmin_temp = rep(0,nfile/24)
day = 1

i = 1
while(i < length(temp)){
  dmin_temp[day] = min(temp[i:(i+23)])
  dmax_temp[day] = max(temp[i:(i+23)])
  day = day + 1
  i = i + 24
}

monthly_average.temp_min = rep(0,12*length(start_year:end_year))
monthly_average.temp_max = rep(0,12*length(start_year:end_year))

y = start_year #year
m = 1 #month
d = 1 # day
for(i in 1:length(monthly_average.temp_max)){
  
  #leap year
  if(y%%4 == 0){
    #31 days
    if (m == 1 | m == 3 | m == 5 | m == 7 | m == 8 | m == 10 | m == 12){
        monthly_average.temp_max[i] <- mean(dmax_temp[d:(d + 30)])
        monthly_average.temp_min[i] <- mean(dmin_temp[d:(d + 30)])
        d = d + 31
    } 
    
    #30 Days
    else if (m == 4 | m == 6 | m == 9 | m == 11) {
      monthly_average.temp_max[i] <- mean(dmax_temp[d:(d + 29)])
      monthly_average.temp_min[i] <- mean(dmin_temp[d:(d + 29)])
      d = d + 30
    } 
    
    #February 
    else { 
      monthly_average.temp_max[i] <- mean(dmax_temp[d:(d + 28)])
      monthly_average.temp_min[i] <- mean(dmin_temp[d:(d + 28)])
      d = d + 29
    }
    
  } else {
    #31 days
    if (m == 1 | m == 3 | m == 5 | m == 7 | m == 8 | m == 10 | m == 12){
      monthly_average.temp_max[i] <- mean(dmax_temp[d:(d + 30)])
      monthly_average.temp_min[i] <- mean(dmin_temp[d:(d + 30)])
      d = d + 31
    } 
    
    #30 Days
    else if (m == 4 | m == 6 | m == 9 | m == 11) {
      monthly_average.temp_max[i] <- mean(dmax_temp[d:(d + 29)])
      monthly_average.temp_min[i] <- mean(dmin_temp[d:(d + 29)])
      d = d + 30
    } 
    
    #February 
    else { 
      monthly_average.temp_max[i] <- mean(dmax_temp[d:(d + 27)])
      monthly_average.temp_min[i] <- mean(dmin_temp[d:(d + 27)])
      d = d + 28
    }
    
  }
  
  #a new year
  if(m == 12){
    y = y + 1
    m = 1
    } 
  else {
    m = m + 1
    }
}

annual_average.temp_min = rep(0,length(start_year:end_year))
annual_average.temp_max = rep(0,length(start_year:end_year))

day = 1
k = 1
for(i in start_year:end_year){
  #leap year
  if(i%%4 == 0){
    annual_average.temp_min[k] = mean(dmin_temp[day:(day + 365)])
    annual_average.temp_max[k] = mean(dmax_temp[day:(day + 365)])
    day = day + 366
  }
  #not a leap year
  else{
    annual_average.temp_min[k] = mean(dmin_temp[day:(day + 364)])
    annual_average.temp_max[k] = mean(dmax_temp[day:(day + 364)])
    day = day + 355
  }
  k = k + 1
}

#### Output data ####
MyData.month <- data.frame(monthly_average.temp_max, monthly_average.temp_min)
colnames(MyData.month) = c('Average Monthly Max Temperature','Average Monthly Min Temperature')


MyData <- data.frame(annual_average.temp_max, annual_average.temp_min)
colnames(MyData) = c('Average Annual Max Temperature','Average Annual Min Temperature')


write.csv(MyData, file = "MyData.csv")
write.csv(MyData.month, file = "MyData.month.csv")

