library(tidyverse)
library(plyr)
library(choroplethr)
library(dplyr)
library(readr)
library(data.table)
library(ggplot2)

dest = "https://www.fhwa.dot.gov/bridge/nbi/2016/delimited/AK16.txt"
tmp = fread(dest) 
tmp = as.tbl(tmp)
tmp1 = read_csv(dest, col_types = "c")  # could make them all characters...
classes = sapply(tmp, class)

states= read_csv("http://pages.stat.wisc.edu/~karlrohe/classes/data/stateAbv.txt")
states=states[-(1:12),]
states[51,] = c("WashDC", "DC")
states[52,] = c("Puerto Rico", "PR")
dat=list()

dest= rep("", 52)
for(i in 1:52) dest[i]=paste("https://www.fhwa.dot.gov/bridge/nbi/2016/delimited/", states[i,2],"16.txt", sep = "") 
x16 = ldply(dest, fread, colClasses = classes)  

M = x16
M = M[,-14]
fun = function(x){ return(which(x>20)) }
bad =  is.na(M) %>% colSums %>% fun
M = M[,-bad]
M = as.tbl(M)
x = select(M, one_of(keep)) 

x = filter(x,x$DECK_COND_058!="N"&x$SUPERSTRUCTURE_COND_059!="N")
x = filter(x,!is.na(x$LAT_016)&!is.na(x$LONG_017)&x$LONG_017>0)
x = mutate(x,condition = pmin(SUPERSTRUCTURE_COND_059, SUBSTRUCTURE_COND_060, 
          CHANNEL_COND_061,CULVERT_COND_062,na.rm = T))
min2dec = function(x){
  as.numeric(substr(x,1,2)) + as.numeric(substr(x,3,8))/6e+05 %>% return
}
x = mutate(x,lat = min2dec(LAT_016), lon = min2dec(LONG_017))
x = filter(x, lon<100 & lon>70 & lat<80)

ggplot(data = x) +geom_point(mapping = aes(y = log(ADT_029), x = STATE_CODE_001))
ggplot(data = x) +geom_point(mapping = aes(y = lat, x = lon, col = log(ADT_029)))
ggplot(data = x) +geom_point(mapping = aes(y = lat, x = lon, col = YEAR_BUILT_027))
ggplot(data = x) +geom_point(mapping = aes(y = log(ADT_029), x = YEAR_BUILT_027, col = condition))

wi.vs.ma = filter(x,STATE_CODE_001==25|STATE_CODE_001==55)
ggplot(data = wi.vs.ma) +geom_point(mapping = aes(y = log(ADT_029), x = YEAR_BUILT_027, col = STATE_CODE_001))
