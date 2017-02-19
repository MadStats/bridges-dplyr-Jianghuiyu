library(tidyverse)
library(plyr)
library(choroplethr)
library(dplyr)
library(readr)
library(data.table)
library(ggplot2)


dest= rep("", 52)
for(i in 1:52) dest[i]=paste("https://www.fhwa.dot.gov/bridge/nbi/2016/delimited/", states[i,2],"16.txt", sep = "") 
x16 = ldply(dest, fread, colClasses = classes)  

M = x16
M = M[,-14]
fun = function(x){ return(which(x>20)) }
bad =  is.na(M) %>% colSums %>% fun
M = M[,-bad]
jold =1
for(j in jold:ncol(M)){
  nc = nchar(M[,j], keepNA = T)
}
M = M[,-j]
jold = j

keep = c("STATE_CODE_001", "STRUCTURE_NUMBER_008" , "COUNTY_CODE_003", "LAT_016", "LONG_017", "TOLL_020" , "ADT_029"           ,      "YEAR_ADT_030" ,
         "YEAR_BUILT_027" , "DECK_COND_058" , "SUPERSTRUCTURE_COND_059", "SUBSTRUCTURE_COND_060"  , "CHANNEL_COND_061","CULVERT_COND_062", "DATE_OF_INSPECT_090"   ,  "FRACTURE_092A"     ,      "UNDWATER_LOOK_SEE_092B" , "SPEC_INSPECT_092C"  )
M = as.tbl(M)
bridges = select(M, one_of(keep))  
bridges = rename(bridges, year = YEAR_BUILT_027)
bridges = mutate(bridges, fips = STATE_CODE_001*1000+COUNTY_CODE_003)

traffic = read_csv("http://pages.stat.wisc.edu/~karlrohe/classes/data/countyBridgeSummaries.csv")
unemployment = read_csv("unemployment.csv")
unemployment = rename (unemployment, fips = region, year = period)

budata = traffic %>% inner_join(unemployment,by=c("fips","year")) %>% inner_join(bridges,by=c("fips","year"))
budata %>% group_by(fips) %>% summarize(maxT = max(maxTraffic, na.rm=T), 
           minT = min(maxTraffic, na.rm = T), ratio = maxT/minT) %>% 
           transmute(region = fips, value = log(ratio)) %>% county_choropleth()
budata %>% group_by(fips) %>% summarize(maxU = max(totalForce, na.rm=T), 
           minU = min(totalForce, na.rm = T), ratio = maxU/minU) %>% 
           transmute(region = fips, value = log(ratio)) %>% county_choropleth()
set = budata %>% group_by(year) %>% summarize(avetraffic = mean(maxTraffic, na.rm=T), 
           aveunemployrate = mean(rate, na.rm=T), avecond = mean(good, na.rm=T), 
           aveunemployforce= mean(totalForce,na.rm=T))
set %>% ggplot(aes(x = year, y = avetraffic)) + geom_line() + geom_smooth()
set %>% ggplot(aes(x = year, y = avecond)) + geom_line() + geom_smooth()
set %>% ggplot(aes(x = year, y = aveunemployrate)) + geom_line() + geom_smooth()
set %>% ggplot(aes(x = year, y = aveunemployforce)) + geom_line() + geom_smooth()
filter(budata,year == 2015) %>% ggplot(aes(x = good, y = maxTraffic)) +geom_line()
