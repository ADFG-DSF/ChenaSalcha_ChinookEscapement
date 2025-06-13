###########################
#
# 1.  Reading Sonar data
#
###########################


## The functions required to read batches of sonar files are currently bundled as the rDIDSON package.
## The ProcessFiles functions and their helpers parse the raw text files saved by the sonars and read 
## them into data.frames in R.  Sometimes these will error out because of a bad text file - just remove
## the offending file (look at the text file first, it will almost certainly just be empty) and the 
## function should run after that.

# library(rDIDSON)
# # Chena_South <- ProcessFiles("C:/Users/mbtyers/documents/2014/Analyses/Chena and Delta Clearwater/2018 analysis/2018 TOWERS/2018 CHENA TEXT FILES/CHENA SOUTH (ddf)")
# # Chena_North <- ProcessFiles_ARIS("C:/Users/mbtyers/documents/2014/Analyses/Chena and Delta Clearwater/2018 analysis/2018 TOWERS/2018 CHENA TEXT FILES/CHENA NORTH")
# # Salcha_South_did <- ProcessFiles("C:/Users/mbtyers/documents/2014/Analyses/Chena and Delta Clearwater/2018 analysis/2018 TOWERS/2018 SALCHA TEXT FILES/SALCHA SOUTH/DIDSON")
# # Salcha_South_aris <- ProcessFiles_ARIS("C:/Users/mbtyers/documents/2014/Analyses/Chena and Delta Clearwater/2018 analysis/2018 TOWERS/2018 SALCHA TEXT FILES/SALCHA SOUTH/ARIS")
# # Salcha_North <- ProcessFiles_ARIS("C:/Users/mbtyers/documents/2014/Analyses/Chena and Delta Clearwater/2018 analysis/2018 TOWERS/2018 SALCHA TEXT FILES/SALCHA NORTH")


library(tidyverse)

# reading data, incorporating some fixes to ensure that the standardize_ functions won't break

Salcha_South_raw <- read.csv("2024/flat_data/Salcha Gravel 2024.csv", stringsAsFactors=F)
Salcha_North_raw <- read.csv("2024/flat_data/Salcha Tower 2024.csv", stringsAsFactors=F)

names(Salcha_South_raw)
summary(Salcha_South_raw)
head(Salcha_South_raw)

names(Salcha_North_raw)
summary(Salcha_North_raw)
head(Salcha_North_raw)


Salcha_South <- Salcha_South_raw %>%
  rename(Date1 = Date.1) %>%
  rename(L_cm = Length) %>%
  mutate(Date1 = ifelse(!is.na(Date), Date, Date1))  # Date1 seems to be the master date column

Salcha_North <- Salcha_North_raw  %>%
  rename(Date1 = Date.1) %>%
  rename(L_cm = Length) %>%
  mutate(Date1 = ifelse(!is.na(Date), Date, Date1))    



# ## Some minor data fixage so the standardize_ functions below won't break.
# 
# names(Salcha_South)[names(Salcha_South)=="Date.1"] <- "Date1"
# names(Salcha_North)[names(Salcha_North)=="Date.1"] <- "Date1"
# 
# names(Salcha_South)[names(Salcha_South)=="Length"] <- "L_cm"
# names(Salcha_North)[names(Salcha_North)=="Length"] <- "L_cm"



# ## More data fixage.

# table(is.na(Salcha_South$Date), is.na(Salcha_South$Date1))
# table(is.na(Salcha_North$Date), is.na(Salcha_North$Date1))

# 
# Chena_North$Date1 <- Chena_North$Date
# Salcha_South$Date1 <- Salcha_South$Date
# Salcha_North$Date1 <- Salcha_North$Date
# Salcha_South$Date1 <- as.Date(Salcha_South$Date1, format="%m/%d/%Y")
# 
# Chena_North$StartTime <- paste0(Chena_North$StartTime,":00")
# Salcha_North$StartTime <- paste0(Salcha_North$StartTime,":00")
# Salcha_South$StartTime <- paste0(Salcha_South$StartTime,":00")



## These should have been bundled with the rDIDSON package, but weren't in the version I grabbed off 
## my machine when I left.  Don't know why, so here they are as raw functions.

## The standardize_ functions serve two purposes:
##  - The ProcessFiles_ functions for DIDSON and ARIS return differently formatted data.frames
##  - Information on count hour and 8-hour shift will be needed later, and these funcs pull this info from Time

## Important: Rows with NA for length correspond to sonar files with no fish recorded.  This 
## is where "zero" counts will come from in expansion.

standardize_DIDSON <- function(x, river, station) {
  date <- as.Date(as.character(x$Date), format="%m/%d/%Y")
  StartTime_spl <- strsplit(as.character(x$StartTime), split=":")
  hour <- as.numeric(sapply(StartTime_spl, "[", 1))
  min <- as.numeric(sapply(StartTime_spl, "[", 2))
  sec <- as.numeric(sapply(StartTime_spl, "[", 3))
  x$Time <- ifelse(is.na(x$Time), 0, x$Time)
  min1 <- min + floor(x$Time)
  sec1 <- round(sec + (x$Time-floor(x$Time))*60)
  min2 <- min1 + (sec1>=60)
  sec2 <- sec1 %% 60
  hour1 <- hour + (min2>=60)
  min3 <- min2 %% 60
  date1 <- date + (hour1>=24)
  hour2 <- hour1 %% 24
  
  return(data.frame(river=river, 
                    station=station,
                    sonartype="DIDSON",
                    date=date1,
                    hour=hour2,
                    min=min3,
                    sec=sec2,
                    timefrac = hour2/24 + min3/(24*60) + sec2/(24*60*60),
                    length=x$Length*1000,
                    shift = floor(hour2/8) + 1,
                    shift_hr = hour2 %% 8 + 1,
                    min20=floor(min3/20)+1,
                    block=paste(date1,floor(hour2/8) + 1,hour2 %% 8 + 1,floor(min3/20)+1,sep="_"),
                    stringsAsFactors=F))
}
standardize_ARIS <- function(x, river, station) {
  date <- as.Date(x$Date1)  # was Date2
  
  Time_spl1 <- strsplit(as.character(x$Time), split=":")
  hour1 <- as.numeric(sapply(Time_spl1, "[", 1))
  min1 <- as.numeric(sapply(Time_spl1, "[", 2))
  sec1 <- as.numeric(sapply(Time_spl1, "[", 3))
  
  Time_spl2 <- strsplit(as.character(x$StartTime), split=":")
  hour2 <- as.numeric(sapply(Time_spl2, "[", 1))
  min2 <- as.numeric(sapply(Time_spl2, "[", 2))
  sec2 <- as.numeric(sapply(Time_spl2, "[", 3))
  
  hour <- ifelse(!is.na(hour1), hour1, hour2)
  min <- ifelse(!is.na(min1), min1, min2)
  sec <- ifelse(!is.na(sec1), sec1, sec2)
  
  return(data.frame(river=river, 
                    station=station,
                    sonartype="ARIS",
                    date=date,
                    hour=hour,
                    min=min,
                    sec=sec,
                    timefrac = hour/24 + min/(24*60) + sec/(24*60*60),
                    length=as.numeric(as.character(x$L_cm))*10,
                    shift = floor(hour/8) + 1,
                    shift_hr = hour %% 8 + 1,
                    min20=floor(min/20)+1,
                    block=paste(date,floor(hour/8) + 1,hour %% 8 + 1,floor(min/20)+1,sep="_"),
                    stringsAsFactors=F))
}




## bundling all sonar data into one monster data.frame

all_sonar <- rbind(standardize_ARIS(Salcha_South, river="Salcha", station="Salcha South"),
                   standardize_ARIS(Salcha_North, river="Salcha", station="Salcha North"))



## i hate data checks

head(all_sonar)
summary(all_sonar)
dim(all_sonar)

# # with(all_sonar, boxplot(date~station))   ## oops the date in Salcha South is formatted wrong, fixed above
# 
# ## why the NA values?
# with(all_sonar, table(is.na(date), is.na(length)))
# subset(all_sonar, is.na(date))
# subset(all_sonar, is.na(length))
# 
# with(all_sonar, table(is.na(date), station))    ## none with Chena South, which was the only DIDSON.  ARIS problem??
# with(all_sonar, table(is.na(length), station))
# with(all_sonar, table(is.na(sec), station))
# 
# ## looking at the way 2018 was formatted..
# load(file="2018 materials/sonardata2018.Rdata")
# summary(all_sonar)  # old version of all_sonar from 2018 .. only NA values in length, signifying no fish in file




## looking for relationships in weird values and missing data

all_sonar_numeric <- all_sonar[,-ncol(all_sonar)]
for(j in 1:3) all_sonar_numeric[,j] <- as.numeric(as.factor(all_sonar_numeric[,j]))
# plot(all_sonar_numeric)   # time consuming
plot(all_sonar_numeric$length, col=all_sonar_numeric$station)

all_sonar_isNA <- all_sonar
for(j in 1:ncol(all_sonar_isNA)) all_sonar_isNA[,j] <- is.na(all_sonar[,j])
# plot(all_sonar_isNA)   # time consuming



## all_fish differs from all_sonar: it culls out lengths less than 400mm and removes NA
## values (which signify a sonar file with no fish)

# all_fish <- subset(all_sonar, !is.na(length))
all_fish <- subset(all_sonar, length>=400)
summary(all_fish)
dim(all_fish)

## I think these checks didn't work and can be taken out

# # block2 <- all_sonar
# checktable <- with(all_sonar, table(block,station)) > 0
# 
# # plot(NA, xlim=c(1,nrow(checktable)), ylim=c(1,4))
# # for(i in 1:4) points(1:nrow(checktable), rep(i, nrow(checktable)), pch=1+15*checktable[,i])
# 
# mean(rowSums(checktable[,1:2])==0)
# mean(rowSums(checktable[,1:2])==1)
# mean(rowSums(checktable[,1:2])==2)
# mean(rowSums(checktable[,3:4])==0)
# mean(rowSums(checktable[,3:4])==1)
# mean(rowSums(checktable[,3:4])==2)
# 
# # plot(all_sonar$length, col=as.numeric(as.factor(all_sonar$station)))
# # 
# # with(all_sonar, table(shift,shift_hr,date,river))



## saving the sonar data!

save(all_sonar, all_fish, file="sonardata2019.Rdata")
# load(file="sonardata2019.Rdata")





