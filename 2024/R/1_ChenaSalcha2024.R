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

save_output <- TRUE
# save_output <- FALSE  # whether to save output to external files
                      # these files live in /2024/Rdata as .Rdata files


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

if(save_output) {
  save(all_sonar, all_fish, file="2024/Rdata/sonardata2024.Rdata")
}
# load(file="2024/Rdata/sonardata2024.Rdata")





##################
#
#  2. Length priors 
#
###################

## Carcass data .csv file has been appended each year

lengthdata_raw <- read.csv("2024/flat_data/AllCarcassData_upto2024.csv",
                       header=T, stringsAsFactors = FALSE)
summary(lengthdata_raw)
table(lengthdata_raw$Sex)

## Bunch o data fixes, mostly inherited from prior years
lengthdata <- lengthdata_raw %>%
  mutate(Sex = ifelse(!Sex %in% c("male", "female"), NA, Sex)) %>%
  mutate(Length = as.numeric(Length)) %>%
  filter(Length >= 200)
summary(lengthdata)  


# not yet sure if this is needed
lengthdata$sexfac <- as.numeric(as.factor(lengthdata$Sex))
lengthdata$yearfac <- as.numeric(as.factor(lengthdata$year))
lengthdata$riverfac <- as.numeric(as.factor(lengthdata$river))
lengthdata$speciesfac <- as.numeric(as.factor(lengthdata$species))



## Setting up for hierarchical model below

lengthdata$cat <- NA
lengthdata$cat[lengthdata$river=="Chena" & lengthdata$species=="Chinook" & lengthdata$Sex=="male"] <- 1
lengthdata$cat[lengthdata$river=="Chena" & lengthdata$species=="Chinook" & lengthdata$Sex=="female"] <- 2
lengthdata$cat[lengthdata$river=="Chena" & lengthdata$species=="Chum" & lengthdata$Sex=="male"] <- 3
lengthdata$cat[lengthdata$river=="Chena" & lengthdata$species=="Chum" & lengthdata$Sex=="female"] <- 4
lengthdata$cat[lengthdata$river=="Salcha" & lengthdata$species=="Chinook" & lengthdata$Sex=="male"] <- 5
lengthdata$cat[lengthdata$river=="Salcha" & lengthdata$species=="Chinook" & lengthdata$Sex=="female"] <- 6
lengthdata$cat[lengthdata$river=="Salcha" & lengthdata$species=="Chum" & lengthdata$Sex=="male"] <- 7
lengthdata$cat[lengthdata$river=="Salcha" & lengthdata$species=="Chum" & lengthdata$Sex=="female"] <- 8
summary(lengthdata)
head(lengthdata)

lengthdata <- subset(lengthdata, !is.na(cat) & !is.na(year) & !is.na(Length))
summary(lengthdata)
head(lengthdata)

carcassyrs <- sort(unique(lengthdata$year))



## plotting the distribution of each years' carcass lengths

par(mfrow=c(2,1))
for(riv in c("Chena","Salcha")) {
  for(spec in c("Chinook","Chum")) {
    for(sexx in c("male","female")) {
      asdf <- subset(lengthdata, river==riv & species==spec & Sex==sexx)
      boxplot(asdf$Length ~ asdf$year, main=paste(riv,spec,sexx), ylim=range(lengthdata$Length,na.rm=T), ylab="length (mm)",las=3) # xaxt='n',
      # axis(1,labels=sort(unique(asdf$year)),at=1:length(unique(asdf$year)))
      grid(nx=0,ny=NULL)
    }
  }
}



## Hierarchical model to estimate mean, sd, and se of carcass lengths for each category.
## Output from this model will be used as priors for the big sonar mixture model!

library(jagsUI)
library(jagshelper)
ncores <- 6  # number of cores for parallel chains.  I use 6 on the big desktop, 3 for laptop

# specify model, which is written to a temporary file
length_jags <- tempfile()
cat('model {
  for (i in 1:N) {
    y[i] ~ dnorm(mu[cat[i],year[i]], tau[cat[i]])
  }
  for(j in 1:Ncat) {
    for(k in 1:Nyear) {
      mu[j,k] ~ dnorm(mumu[j], taumu[j])
    }
    mumu[j] ~ dnorm(500,0.0001)
    sigmu[j] ~ dunif(0,100)
    taumu[j] <- pow(sigmu[j], -2)
    tau[j] <- pow(sigma[j], -2)
    sigma[j] ~ dunif(0,500)
  }
catmean[1:Ncat] <- mu[1:Ncat,Nyear]

}', file=length_jags)


niter <- 10*1000 #50000   
# 2k takes 1.3 minutes, 10k takes 5 min
# 50k took 35 minutes previously

{
tstart <- Sys.time()
print(tstart)
length_data_jags <- list(y=lengthdata$Length, year=lengthdata$yearfac, cat=lengthdata$cat,
                    N=nrow(lengthdata), Ncat=max(lengthdata$cat), Nyear= max(lengthdata$yearfac))
length_jags_out <- jagsUI::jags(model.file=length_jags, data=length_data_jags,
                                parameters.to.save=c("mu","mumu","sigmu","sigma","catmean"), 
                                n.chains=ncores, parallel=T, n.iter=niter, n.burnin=niter/2, n.thin=niter/2000)
Sys.time()-tstart
}
nbyname(length_jags_out)
plotRhats(length_jags_out)
traceworstRhat(length_jags_out, parmfrow = c(3, 3))


# # jags.names(length_jags_out,425)
# # 
# # par(mfrow=c(5,5))
# # # jags.trace(length_jags_out)
# # jags.trace(length_jags_out,which.param = 1:8)
# # jags.trace(length_jags_out,which.param = 18:25)
# 
prior_mn <- length_jags_out$mean$catmean
prior_se <- length_jags_out$sd$catmean
prior_sd_mn <- length_jags_out$mean$sigma
prior_sd_sd <- length_jags_out$sd$sigma
# 
mu_mn <- length_jags_out$mean$mu
mu_sd <- length_jags_out$sd$mu



## overlaying model output on top of carcass length data.
##  - horizontal lines: point estimates for means
##  - heavy blue bars (not visible in many cases): +/- SE for means
##  - light blue bars: +/- SD for observations

cat <- 0
par(mfrow=c(2,1))
for(riv in c("Chena","Salcha")) {
  for(spec in c("Chinook","Chum")) {
    for(sexx in c("male","female")) {
      cat <- cat+1
      asdf <- subset(lengthdata, river==riv & species==spec & Sex==sexx)
      boxplot(asdf$Length ~ asdf$year, main=paste(riv,spec,sexx), ylim=range(lengthdata$Length,na.rm=T), ylab="length (mm)",las=3,border="grey80") # xaxt='n',
      grid(nx=0,ny=NULL)
      
      whichyrs <- sort(unique(asdf$yearfac))
      mn <- mu_mn[cat,whichyrs]
      selo <- mn-mu_sd[cat,whichyrs]
      sehi <- mn+mu_sd[cat,whichyrs]
      siglo <- mn-prior_sd_mn[cat]
      sighi <- mn+prior_sd_mn[cat]
      # points(mn,pch="-",col=4)
      segments((1:length(mn))-.3, mn, (1:length(mn))+.3, mn, lwd=2,col=4,lend=1)
      segments(1:length(mn),selo,1:length(mn),sehi,lwd=5,lend=1,col=4)
      segments(1:length(mn),siglo,1:length(mn),sighi,lwd=2,col=4)
    }
  }
}

par(mfrow=c(1,1))
subset(lengthdata, year==2024) %>% 
  boxplot(Length ~ factor(cat, levels=1:8), data=., col="grey90", border="grey")
subset(lengthdata, year==2024) %>% 
  points(Length ~ jitter(cat), data=.)
caterpillar(length_jags_out, "catmean", add=TRUE, lwd=2)






########################################
#
#  3.  differential run-timing priors 
#
########################################

## The sonar species apportionment model will use priors for species that are weakly informed by a 
## logistic curve.  Since the chum run has always been observed to be slightly later than the king
## run, the daily king proportion follows a very nearly logistic curve, visible in each year's 
## visual count data.

## To estimate these parameters, a hierarchical logistic regression model has been used, using the
## non-expanded visual counts for each day as input data.  Regression coefficients are treated as 
## hierarchically distributed.  Since the timing of the chum and king runs for the Chena and Salcha
## rivers tend to be similar each year, the coefficient pairs (intercepts for both rivers, slopes
## for both rivers) are assumed to be MVN, allowing the inclusion of a correlation parameter.
## If one river's sonar season is compromised, this allows the borrowing of information from the
## other river.

## So far, I've only used years in which data exist for both rivers.  I realize now this may not 
## be necessary (having just run the model without 2019 data for the Salcha!)


## Run timing data .csv file has been appended each year from visual tower counts (non-expanded)
Chena_runtiming <- read.csv(file="2024/flat_data/Chena_historic_2024.csv")

Chena_runtiming$date205 <- Chena_runtiming$julian - 205             # approximately centering date
# Chena_mat <- cbind(Chena_runtiming$chinook,Chena_runtiming$chum)
# Chena_mat[Chena_mat<0] <- 0
Chena_runtiming$chinook[Chena_runtiming$chinook<0] <- 0
Chena_runtiming$chum[Chena_runtiming$chum<0] <- 0


Salcha_runtiming <- read.csv(file="2024/flat_data/Salcha_historic_2024.csv")
Salcha_runtiming$date205 <- Salcha_runtiming$julian - 205
# Salcha_mat <- cbind(Salcha_runtiming$chinook,Salcha_runtiming$chum)
# Salcha_mat[Salcha_mat<0] <- 0
Salcha_runtiming$chinook[Salcha_runtiming$chinook<0] <- 0
Salcha_runtiming$chum[Salcha_runtiming$chum<0] <- 0

## only including years that have data for both rivers
yearsboth <- intersect(unique(Chena_runtiming$year),unique(Salcha_runtiming$year))
Chena_sub <- subset(Chena_runtiming,year %in% yearsboth)
Salcha_sub <- subset(Salcha_runtiming,year %in% yearsboth)

## only including dates with nonzero counts
Chena_sub <- Chena_sub[rowSums(Chena_sub[,2:3])>0,]
Salcha_sub <- Salcha_sub[rowSums(Salcha_sub[,2:3])>0,]

### if we wanted to mess with effective sample size, here's a place we could do it
# Chena_sub[,2:3] <- round(Chena_sub[,2:3]/6)
# Salcha_sub[,2:3] <- round(Salcha_sub[,2:3]/6)

# specify model, which is written to a temporary file
CS_jags <- tempfile()
cat('model {
  for(i in 1:Cn) {
    logit(Cpi[i]) <- a0[Cyear[i],1] + a1[Cyear[i],1]*Cday[i]
    Cchin[i] ~ dbin(Cpi[i],Ctot[i])
  }
  for(i in 1:Sn) {
    logit(Spi[i]) <- a0[Syear[i],2] + a1[Syear[i],2]*Sday[i]
    Schin[i] ~ dbin(Spi[i],Stot[i])
  }
    
  for(i in 1:nyear) {
    a0[i,1:2] ~ dmnorm(b0[],tau0[,])
    a1[i,1:2] ~ dmnorm(b1[],tau1[,])
  }

    
  tau0[1:2,1:2] <- inverse(Sigma0[,])
  tau1[1:2,1:2] <- inverse(Sigma1[,])
    
  Sigma0[1,1] <- pow(sig01,2)
  Sigma0[2,2] <- pow(sig02,2)
  Sigma0[1,2] <- rho0*sig01*sig02
  Sigma0[2,1] <- Sigma0[1,2]
  sig01 ~ dunif(0,10)   # was 1000
  sig02 ~ dunif(0,10)   # was 1000
  rho0 ~ dunif(-1,1)
    
  Sigma1[1,1] <- pow(sig11,2)
  Sigma1[2,2] <- pow(sig12,2)
  Sigma1[1,2] <- rho1*sig11*sig12
  Sigma1[2,1] <- Sigma1[1,2]
  sig11 ~ dunif(0,10)
  sig12 ~ dunif(0,10)
  rho1 ~ dunif(-1,1)
    
  b0[1] ~ dnorm(0,0.001)
  b1[1] ~ dnorm(0,0.001)
  b0[2] ~ dnorm(0,0.001)
  b1[2] ~ dnorm(0,0.001)
}', file=CS_jags)


CS_data <- list(Stot=Salcha_sub$chinook+Salcha_sub$chum,Schin=Salcha_sub$chin,Sday=Salcha_sub$date205,Syear=as.numeric(as.factor(Salcha_sub$year)),
                Ctot=Chena_sub$chinook+Chena_sub$chum,Cchin=Chena_sub$chin,Cday=Chena_sub$date205,Cyear=as.numeric(as.factor(Chena_sub$year)),
                nyear=length(unique(Salcha_sub$year)),Sn=nrow(Salcha_sub),Cn=nrow(Chena_sub))


niter <- 100*1000 #500000   
# 10k in 30 sec, 50k in 2.5 min, 100k in 5 min

ncores <- 6


{
tstart <- Sys.time()
print(tstart)
CS_jags_out <- jagsUI::jags(model.file=CS_jags,data=CS_data, parameters.to.save=c("b0","b1","a0","a1","rho0","rho1"),
                            n.chains=ncores, parallel=T, n.iter=niter, n.thin=niter/2000, n.burnin=niter/2)
Sys.time()-tstart
}

nbyname(CS_jags_out)
plotRhats(CS_jags_out)
traceworstRhat(CS_jags_out, parmfrow = c(3, 3))


## running the model again, taking out the 2019 and 2024 Salcha data

CS_data_alt <- CS_data
CS_data_alt$Schin[Salcha_sub$year %in% c(2019, 2024)] <- NA
{
  tstart <- Sys.time()
  print(tstart)
  CS_jags_out_alt <- jagsUI::jags(model.file=CS_jags,data=CS_data_alt, parameters.to.save=c("b0","b1","a0","a1","rho0","rho1"),
                                n.chains=ncores, parallel=T, n.iter=niter, n.thin=niter/2000, n.burnin=niter/2)
  Sys.time()-tstart
}

nbyname(CS_jags_out_alt)
plotRhats(CS_jags_out_alt)
traceworstRhat(CS_jags_out_alt, parmfrow = c(3, 3))


## saving point estimates for regression parameters

a0 <- CS_jags_out$mean$a0
a1 <- CS_jags_out$mean$a1
b0 <- CS_jags_out$mean$b0
b1 <- CS_jags_out$mean$b1

a0_alt <- CS_jags_out_alt$mean$a0
a1_alt <- CS_jags_out_alt$mean$a1
b0_alt <- CS_jags_out_alt$mean$b0
b1_alt <- CS_jags_out_alt$mean$b1

# save(CS_jags_out, length.jags.out, lengthdata, file="priorouts.Rdata")




## Plotting!

# library(MTfuncs)
# expit <- function(x)  exp(x)/(1+exp(x))

par(mfrow=c(3,4))
for(i in 1:length(yearsboth)) {
  asdf <- subset(Chena_sub, year==yearsboth[i])
  glm1 <- glm(as.matrix(asdf[,2:3])~asdf$date205,family="binomial")
  ayr <- as.numeric(glm1$coefficients)
  plot(asdf$date205,asdf$chinook/(asdf$chinook+asdf$chum),main=paste("Chena",yearsboth[i]),ylab="proportion Chinook",xlab="day",ylim=0:1,xlim=range(Chena_sub$date205))
  curve(expit(ayr[1]+ayr[2]*x),add=T)
  curve(expit(a0[i,1]+a1[i,1]*x),add=T,col=4,lty=2,lwd=2)
  curve(expit(b0[1]+b1[1]*x),add=T,lty=3,col=2)
}
plot(NA,main="",xlab="",ylab="",xlim=0:1,ylim=0:1,yaxt="n",xaxt="n")
legend("topleft",legend=c("year alone","all years","hierarchical"),lwd=c(1,1,2),col=c(1,2,4),lty=c(1,3,2),bty="n")

par(mfrow=c(3,4))
for(i in 1:length(yearsboth)) {
  asdf <- subset(Salcha_sub, year==yearsboth[i])
  glm1 <- glm(as.matrix(asdf[,2:3])~asdf$date205,family="binomial")
  ayr <- as.numeric(glm1$coefficients)
  plot(asdf$date205,asdf$chinook/(asdf$chinook+asdf$chum),main=paste("Salcha",yearsboth[i]),ylab="proportion Chinook",xlab="day",ylim=0:1,xlim=range(Chena_sub$date205))
  curve(expit(ayr[1]+ayr[2]*x),add=T)
  curve(expit(a0[i,2]+a1[i,2]*x),add=T,col=4,lty=2,lwd=2)
  curve(expit(b0[2]+b1[2]*x),add=T,lty=3,col=2)
}
plot(NA,main="",xlab="",ylab="",xlim=0:1,ylim=0:1,yaxt="n",xaxt="n")
legend("topleft",legend=c("year alone","all years","hierarchical"),lwd=c(1,1,2),col=c(1,2,4),lty=c(1,3,2),bty="n")


par(mfrow=c(2,2))

for(i in length(yearsboth)) {
  asdf <- subset(Chena_sub, year==yearsboth[i])
  glm1 <- glm(as.matrix(asdf[,2:3])~asdf$date205,family="binomial")
  ayr <- as.numeric(glm1$coefficients)
  plot(asdf$date205,asdf$chinook/(asdf$chinook+asdf$chum),main=paste("Chena",yearsboth[i]),ylab="proportion Chinook",xlab="day",ylim=0:1,xlim=range(Chena_sub$date205))
  curve(expit(ayr[1]+ayr[2]*x),add=T)
  curve(expit(a0[i,1]+a1[i,1]*x),add=T,col=4,lty=2,lwd=2)
  curve(expit(b0[1]+b1[1]*x),add=T,lty=3,col=2)
}
# plot(NA,main="",xlab="",ylab="",xlim=0:1,ylim=0:1,yaxt="n",xaxt="n")
legend("bottomleft",legend=c("year alone","all years","hierarchical"),lwd=c(1,1,2),col=c(1,2,4),lty=c(1,3,2),bty="n")

for(i in length(yearsboth)) {
  asdf <- subset(Salcha_sub, year==yearsboth[i])
  glm1 <- glm(as.matrix(asdf[,2:3])~asdf$date205,family="binomial")
  ayr <- as.numeric(glm1$coefficients)
  plot(asdf$date205,asdf$chinook/(asdf$chinook+asdf$chum),main=paste("Salcha",yearsboth[i]),ylab="proportion Chinook",xlab="day",ylim=0:1,xlim=range(Chena_sub$date205))
  curve(expit(ayr[1]+ayr[2]*x),add=T)
  curve(expit(a0[i,2]+a1[i,2]*x),add=T,col=4,lty=2,lwd=2)
  curve(expit(b0[2]+b1[2]*x),add=T,lty=3,col=2)
}
# plot(NA,main="",xlab="",ylab="",xlim=0:1,ylim=0:1,yaxt="n",xaxt="n")
legend("bottomleft",legend=c("year alone","all years","hierarchical"),lwd=c(1,1,2),col=c(1,2,4),lty=c(1,3,2),bty="n")


plot(NA,ylim=0:1,xlim=c(-30,20),ylab="proportion Chinook",xlab="day",main="Chena")
for(i in 1:length(yearsboth)) {
  curve(expit(a0[i,1]+a1[i,1]*x),add=T)
}
curve(expit(b0[1]+b1[1]*x),add=T,lty=2,lwd=3)
curve(expit(a0[length(yearsboth),1]+a1[length(yearsboth),1]*x),add=T,lwd=3)
legend("topright",legend=c("yearly","average",yearsboth[length(yearsboth)]),lwd=c(1,3,3),lty=c(1,2,1))

plot(NA,ylim=0:1,xlim=c(-30,20),ylab="proportion Chinook",xlab="day",main="Salcha")
for(i in 1:length(yearsboth)) {
  curve(expit(a0[i,2]+a1[i,2]*x),add=T)
}
curve(expit(b0[2]+b1[2]*x),add=T,lty=2,lwd=3)
curve(expit(a0[length(yearsboth),2]+a1[length(yearsboth),2]*x),add=T,lwd=3)
legend("topright",legend=c("yearly","average",yearsboth[length(yearsboth)]),lwd=c(1,3,3),lty=c(1,2,1))




par(mfrow=c(3,4))
for(i in 1:length(yearsboth)) {
  asdf <- subset(Chena_sub, year==yearsboth[i])
  glm1 <- glm(as.matrix(asdf[,2:3])~asdf$date205,family="binomial")
  ayr <- as.numeric(glm1$coefficients)
  plot(asdf$date205,asdf$chinook/(asdf$chinook+asdf$chum),main=paste("Chena",yearsboth[i]),ylab="proportion Chinook",xlab="day",ylim=0:1,xlim=range(Chena_sub$date205))
  curve(expit(ayr[1]+ayr[2]*x),add=T)
  curve(expit(a0_alt[i,1]+a1_alt[i,1]*x),add=T,col=4,lty=2,lwd=2)
  curve(expit(b0_alt[1]+b1_alt[1]*x),add=T,lty=3,col=2)
}
plot(NA,main="",xlab="",ylab="",xlim=0:1,ylim=0:1,yaxt="n",xaxt="n")
legend("topleft",legend=c("year alone","all years","hierarchical"),lwd=c(1,1,2),col=c(1,2,4),lty=c(1,3,2),bty="n")

par(mfrow=c(3,4))
for(i in 1:length(yearsboth)) {
  asdf <- subset(Salcha_sub, year==yearsboth[i])
  glm1 <- glm(as.matrix(asdf[,2:3])~asdf$date205,family="binomial")
  ayr <- as.numeric(glm1$coefficients)
  plot(asdf$date205,asdf$chinook/(asdf$chinook+asdf$chum),main=paste("Salcha",yearsboth[i]),ylab="proportion Chinook",xlab="day",ylim=0:1,xlim=range(Chena_sub$date205))
  curve(expit(ayr[1]+ayr[2]*x),add=T)
  curve(expit(a0_alt[i,2]+a1_alt[i,2]*x),add=T,col=4,lty=2,lwd=2)
  curve(expit(b0_alt[2]+b1_alt[2]*x),add=T,lty=3,col=2)
}
plot(NA,main="",xlab="",ylab="",xlim=0:1,ylim=0:1,yaxt="n",xaxt="n")
legend("topleft",legend=c("year alone","all years","hierarchical"),lwd=c(1,1,2),col=c(1,2,4),lty=c(1,3,2),bty="n")



par(mfrow=c(2,2))

for(i in length(yearsboth)) {
  asdf <- subset(Chena_sub, year==yearsboth[i])
  glm1 <- glm(as.matrix(asdf[,2:3])~asdf$date205,family="binomial")
  ayr <- as.numeric(glm1$coefficients)
  plot(asdf$date205,asdf$chinook/(asdf$chinook+asdf$chum),main=paste("Chena",yearsboth[i]),ylab="proportion Chinook",xlab="day",ylim=0:1,xlim=range(Chena_sub$date205))
  curve(expit(ayr[1]+ayr[2]*x),add=T)
  curve(expit(a0_alt[i,1]+a1_alt[i,1]*x),add=T,col=4,lty=2,lwd=2)
  curve(expit(b0_alt[1]+b1_alt[1]*x),add=T,lty=3,col=2)
}
# plot(NA,main="",xlab="",ylab="",xlim=0:1,ylim=0:1,yaxt="n",xaxt="n")
legend("bottomleft",legend=c("year alone","all years","hierarchical"),lwd=c(1,1,2),col=c(1,2,4),lty=c(1,3,2),bty="n")

for(i in length(yearsboth)) {
  asdf <- subset(Salcha_sub, year==yearsboth[i])
  glm1 <- glm(as.matrix(asdf[,2:3])~asdf$date205,family="binomial")
  ayr <- as.numeric(glm1$coefficients)
  plot(asdf$date205,asdf$chinook/(asdf$chinook+asdf$chum),main=paste("Salcha",yearsboth[i]),ylab="proportion Chinook",xlab="day",ylim=0:1,xlim=range(Chena_sub$date205))
  curve(expit(ayr[1]+ayr[2]*x),add=T)
  curve(expit(a0_alt[i,2]+a1_alt[i,2]*x),add=T,col=4,lty=2,lwd=2)
  curve(expit(b0_alt[2]+b1_alt[2]*x),add=T,lty=3,col=2)
}
# plot(NA,main="",xlab="",ylab="",xlim=0:1,ylim=0:1,yaxt="n",xaxt="n")
legend("bottomleft",legend=c("year alone","all years","hierarchical"),lwd=c(1,1,2),col=c(1,2,4),lty=c(1,3,2),bty="n")


plot(NA,ylim=0:1,xlim=c(-30,20),ylab="proportion Chinook",xlab="day",main="Chena")
for(i in 1:length(yearsboth)) {
  curve(expit(a0_alt[i,1]+a1_alt[i,1]*x),add=T)
}
curve(expit(b0_alt[1]+b1_alt[1]*x),add=T,lty=2,lwd=3)
curve(expit(a0_alt[length(yearsboth),1]+a1_alt[length(yearsboth),1]*x),add=T,lwd=3)
legend("topright",legend=c("yearly","average",yearsboth[length(yearsboth)]),lwd=c(1,3,3),lty=c(1,2,1))

plot(NA,ylim=0:1,xlim=c(-30,20),ylab="proportion Chinook",xlab="day",main="Salcha")
for(i in 1:length(yearsboth)) {
  curve(expit(a0_alt[i,2]+a1_alt[i,2]*x),add=T)
}
curve(expit(b0_alt[2]+b1_alt[2]*x),add=T,lty=2,lwd=3)
curve(expit(a0_alt[length(yearsboth),2]+a1_alt[length(yearsboth),2]*x),add=T,lwd=3)
legend("topright",legend=c("yearly","average",yearsboth[length(yearsboth)]),lwd=c(1,3,3),lty=c(1,2,1))


# 
# 
# 
# ## Tracing all the Bayes output
# 
# weirdtrace2 <- function(x, nline=NULL, n=NULL, lwd=1, main="") {             
#   if(is.null(nline)) nline <- length(x)/n
#   if(is.null(n)) n <- length(x)/nline
#   cols <- adjustcolor(rainbow(nline),red.f=.9,blue.f=.9,green.f=.9,alpha.f=.6)
#   plot(NA,xlim=c(0,n),ylim=range(x,na.rm=T),main=main)
#   # lwd <- c(1,1,1,1,1,1,1,1,1,2,2,2)
#   for(i in 1:nline) {
#     lines(1:n, x[(n*(i-1)+1):(n*i)], col=cols[i],lwd=lwd)
#   }
# }
# trace_df <- function(df,nline=ncores,...) {
#   for(i in 1:ncol(df)) {
#     weirdtrace2(df[,i],main=names(df)[i],nline=nline,...=...)
#   }
# }
# 
# length.jags.out_df <- as.data.frame(as.matrix(length_jags_out$samples))
# CS_jags_out_df <- as.data.frame(as.matrix(CS_jags_out$samples))
# dim(length.jags.out_df)
# dim(CS_jags_out_df)
# 
# par(mfrow=c(5,2))
# trace_df(CS_jags_out_df)
# 
# par(mfrow=c(5,3))
# trace_df(length.jags.out_df)
# 
# 
# 
# ## playing with diagnostics...
# 
# par(mfrow=c(3,2))
# hist(unlist(CS_jags_out$Rhat))
# hist(unlist(CS_jags_out$n.eff))
# hist(unlist(CS_jags_out_alt$Rhat))
# hist(unlist(CS_jags_out_alt$n.eff))
# hist(unlist(length.jags.out$Rhat))
# hist(unlist(length.jags.out$n.eff))
# 
# sapply(CS_jags_out$Rhat, function(x) sum(x>1.1, na.rm=T))
# sapply(CS_jags_out$n.eff, function(x) sum(x<500, na.rm=T))
# sapply(length.jags.out$Rhat, function(x) sum(x>1.1, na.rm=T))
# sapply(length.jags.out$n.eff, function(x) sum(x<500, na.rm=T))
# 
# sapply(CS_jags_out$Rhat, function(x) which(x>1.1))
# sapply(CS_jags_out$n.eff, function(x) which(x<500))
# sapply(length.jags.out$Rhat, function(x) which(x>1.1))
# sapply(length.jags.out$n.eff, function(x) which(x<500))


if(save_output) {
  save(a0, a1, a0_alt, a1_alt,
       prior_mn, prior_sd_mn, prior_sd_sd, prior_se, 
       file="2024/Rdata/CSpriors2024.Rdata")
}
  # save(all_fish, file="sonardata2018.Rdata")