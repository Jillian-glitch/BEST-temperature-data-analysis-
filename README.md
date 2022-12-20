# BEST-temperature-data-analysis-
setwd("D:/")

####Wrangling raster data

#load and prep data

# Load library
library('sf')

# Load shapefile
shapename <- read_sf('D://uganda')

#best comes in anomaly space and you have to add anomalies to climatology to get temperature levels

clim <- brick("Average.nc",var = "climatology")
anom <-brick("Average.nc",var = "temperature")


years <- 1900:2022


#get dates from the layer names
dates <- data.frame(
  year = as.numeric(unlist(lapply(strsplit(names(anom),"\\."),
                                  function(x){substr(x[[1]],2,5)}))),
  month = as.numeric(unlist(lapply(strsplit(names(anom),"\\."),
                                   function(x){round(0.5+12*as.numeric(paste(".",x[[2]], sep="") ) )})))
)

#subset raster brick on years
anom <- subset(anom, which(dates$year %in% years), drop = T)


#re-define dates data frame to match to subset anomalies
dates <- data.frame(
  year = as.numeric(unlist(lapply(strsplit(names(anom),"\\."),
                                  function(x){substr(x[[1]],2,5)}))),
  month = as.numeric(unlist(lapply(strsplit(names(anom),"\\."),
                                   function(x){round(0.5+12*as.numeric(paste(".",x[[2]], sep="") ) )})))
) %>% as.data.frame()



#initialize temperature object as the anomalies and then add in the monthly climatologies
temps <- anom

#loop over raster brick layers and add anomalies to relevant monthly clim
for (i in 1:dim(temps)[3]){	
  temps[[i]] <- temps[[i]] + clim[[as.numeric(dates[i,"month"])]]
  
}					

#crop best data
temps <- raster::crop(temps, extent(uganda)) #africa is just a shapefile of africa. can crop to whatever region if you want.
grid <- raster(temps[[1]]); grid[]<-1:ncell(grid) #define grid and populate with cell number

#crop best grid to match
grid <- raster::crop(grid, extent(uganda))	
temps <- raster::crop(temps, extent(uganda))

grid[]<-1:ncell(grid)


#match best grid cells to countries            


cellVals <- raster::extract(x = grid, y = uganda)

for(i in 1:length(cellVals)){
  cellVals[[i]] <- data.frame(cellVals[[i]])
  cellVals[[i]]$country <- uganda@data$ISO3[i]
  names(cellVals[[i]])[1]<-"cell"  
}
cellVals <- data.frame(data.table::rbindlist(cellVals)) #slow

#use step 1 to pull out data          

#rows are best grid cells, columns are cell id and then each month of data
tmp.dat <- data.frame(cellVals, tmp = temps[cellVals$cell])
names(tmp.dat)[3:ncol(tmp.dat)]<-paste("best_",dates$year,"_", dates$month,sep="")



#reshape data to prepare for merge        

#get country-level averages by taking mean across all grid cells that overlap each country
tmp.dat <- tmp.dat %>% group_by(country) %>% summarise_at(vars(best_1900_1:best_2022_12), mean, na.rm = T)

#create year and month variables from variable name
tmp <- tmp.dat %>% gather(date, tmp, -country) %>% 
  mutate(year = as.numeric(substr(date, 6,9)),
         month = as.numeric(substr(date, 11,12))) %>% 
  dplyr::select(-date)


#sort data
tmp <- arrange(tmp, country, year, month)

#aggregate from monthly to annual
tmp_annual <- tmp %>% group_by(country, year) %>% summarise(tmp = mean(tmp, na.rm = T))



#merge best data with your data

data <- left_join(Stunting, tmp_annual, by = c("country","year"))
write.csv(data, "Stunting_average.csv")
