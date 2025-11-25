## The purpose of this script is to apply the expansion methods to the visual
## data and the sonar (mixture) model output, and to apply the running average
## interpolation and hierarchical run-timing (Hamachan) model as appropriate
## to fill in missing dates.

## This script then compiles all estimates into output spreadsheets (.csv) for 
## each River x Species.

## Note: much of this script is devoted to running the Hamachan model.  Since 
## the Chena was so data-limited, the model was very unstable, resulting in 
## a handful of wild MCMC samples that vastly inflated the overall variance.
## In future years this might not be so much of a problem!


library(tidyverse)
library(jagsUI)
library(jagshelper)

load(file="2025/Rdata/CSpriors2025.Rdata")
load(file="2025/Rdata/sonardata2025.Rdata")
load(file="2025/Rdata/vis_2025.Rdata")
load(file="2025/Rdata/mixmodel2025.Rdata")

# save_output <- TRUE  # FALSE  # whether to write output to external files
save_output <- FALSE 

run_model <- TRUE  # whether to (re)run the interpolation model
# run_model <- FALSE  




######################################################
#
#   expanding the mixture model (sonar) output
#
######################################################


# slightly kludgy thing for post-truncation of sonar based on input length...
## in past years, 450 seemed to work best.  
trunc <- 450

trunc_subset <- (all_fish$length >= trunc)
specmat <- specmat[,trunc_subset]
# specmat_alt <- specmat_alt[,trunc_subset]
all_fish <- all_fish[trunc_subset,]
modlength <- modlength[trunc_subset]


# simplifying to TRUE or FALSE for each species
chinmat <- specmat==1
chummat <- specmat==2



## Formatting posterior totals as a set of arrays, in which each cell corresponds to a 20-min
## counting block.  Array dimensions correspond to date, 8-hr shift, hour period, and 20-min block.
## Arrays are generated for point estimates (median) and variance, for each species and each
## sonar station.

## The 650 stuff is left over from a comparison we did - totals of fish larger/smaller than a 
## 650mm threshold (or whatever we wanted the shreshold to be).

dates <- sort(unique(paste(2025, substr(as.character(Salcha_2025_Chin_vis$Day),6,10),sep="-")))
# stations <- sort(unique(all_sonar$station))
CN_650down <- CS_650down <- SN_650down <- SS_650down <- CN_650up <- CS_650up <- SN_650up <- SS_650up <- 
  CN_650down <- CS_650down <- SN_650down <- SS_650down <- CN_650up <- CS_650up <- SN_650up <- SS_650up <- 
  CN_chum <- CS_chum <- SN_chum <- SS_chum <- CN_chin <- CS_chin <- SN_chin <- SS_chin <- 
  vCN_chum <- vCS_chum <- vSN_chum <- vSS_chum <- vCN_chin <- vCS_chin <- vSN_chin <- vSS_chin <- array(0, dim=c(length(dates),3,8,3))

idate <- 1
# all_fish$length <- all_fish$modlength    ########## don't do this

lthresh <- 650#650



# this is a silly fix, oh well  --- 2025
all_fish$station <- ifelse(all_fish$station=="towerside", "Chena North",
                           ifelse(all_fish$station=="coachman", "Chena South", 
                                  all_fish$station))
all_sonar$station <- ifelse(all_sonar$station=="towerside", "Chena North",
                           ifelse(all_sonar$station=="coachman", "Chena South", 
                                  all_sonar$station))
all_sonar <- subset(all_sonar, !is.na(hour))



for(datei in dates) {
  for(shifti in 1:3) {
    for(periodi in 1:8) {
      for(min20i in 1:3) {  
        a <- with(all_fish, station=="Chena North" & date==datei & shift==shifti & shift_hr==periodi & min20==min20i)
        if(sum(a)>0) {
          CN_chin[idate,shifti,periodi,min20i] <- ifelse(sum(a)>1, median(rowSums(chinmat[, a])), median(chinmat[,a]))
          CN_chum[idate,shifti,periodi,min20i] <- ifelse(sum(a)>1, median(rowSums(chummat[, a])), median(chummat[,a]))
          vCN_chin[idate,shifti,periodi,min20i] <- ifelse(sum(a)>1, var(rowSums(chinmat[, a])), var(chinmat[,a]))
          vCN_chum[idate,shifti,periodi,min20i] <- ifelse(sum(a)>1, var(rowSums(chummat[, a])), var(chummat[,a]))
          CN_650up[idate,shifti,periodi,min20i] <- sum(all_fish$length[a]>=lthresh)
          CN_650down[idate,shifti,periodi,min20i] <- sum(all_fish$length[a]<lthresh)
        }
        a <- with(all_fish, station=="Chena South" & date==datei & shift==shifti & shift_hr==periodi & min20==min20i)
        if(sum(a)>0) {
          CS_chin[idate,shifti,periodi,min20i] <- ifelse(sum(a)>1, median(rowSums(chinmat[, a])), median(chinmat[,a]))
          CS_chum[idate,shifti,periodi,min20i] <- ifelse(sum(a)>1, median(rowSums(chummat[, a])), median(chummat[,a]))
          vCS_chin[idate,shifti,periodi,min20i] <- ifelse(sum(a)>1, var(rowSums(chinmat[, a])), var(chinmat[,a]))
          vCS_chum[idate,shifti,periodi,min20i] <- ifelse(sum(a)>1, var(rowSums(chummat[, a])), var(chummat[,a]))
          CS_650up[idate,shifti,periodi,min20i] <- sum(all_fish$length[a]>=lthresh)
          CS_650down[idate,shifti,periodi,min20i] <- sum(all_fish$length[a]<lthresh)
        }
        a <- with(all_fish, station=="Salcha North" & date==datei & shift==shifti & shift_hr==periodi & min20==min20i)
        if(sum(a)>0) {
          SN_chin[idate,shifti,periodi,min20i] <- ifelse(sum(a)>1, median(rowSums(chinmat[, a])), median(chinmat[,a]))
          SN_chum[idate,shifti,periodi,min20i] <- ifelse(sum(a)>1, median(rowSums(chummat[, a])), median(chummat[,a]))
          vSN_chin[idate,shifti,periodi,min20i] <- ifelse(sum(a)>1, var(rowSums(chinmat[, a])), var(chinmat[,a]))
          vSN_chum[idate,shifti,periodi,min20i] <- ifelse(sum(a)>1, var(rowSums(chummat[, a])), var(chummat[,a]))
          SN_650up[idate,shifti,periodi,min20i] <- sum(all_fish$length[a]>=lthresh)
          SN_650down[idate,shifti,periodi,min20i] <- sum(all_fish$length[a]<lthresh)
        }
        a <- with(all_fish, station=="Salcha South" & date==datei & shift==shifti & shift_hr==periodi & min20==min20i)
        if(sum(a)>0) {
          SS_chin[idate,shifti,periodi,min20i] <- ifelse(sum(a)>1, median(rowSums(chinmat[, a])), median(chinmat[,a]))
          SS_chum[idate,shifti,periodi,min20i] <- ifelse(sum(a)>1, median(rowSums(chummat[, a])), median(chummat[,a]))
          vSS_chin[idate,shifti,periodi,min20i] <- ifelse(sum(a)>1, var(rowSums(chinmat[, a])), var(chinmat[,a]))
          vSS_chum[idate,shifti,periodi,min20i] <- ifelse(sum(a)>1, var(rowSums(chummat[, a])), var(chummat[,a]))
          SS_650up[idate,shifti,periodi,min20i] <- sum(all_fish$length[a]>=lthresh)
          SS_650down[idate,shifti,periodi,min20i] <- sum(all_fish$length[a]<lthresh)
        }
        
        a <- with(all_sonar, station=="Chena North" & date==datei & shift==shifti & shift_hr==periodi & min20==min20i)
        if(sum(a)==0) {
          CN_chin[idate,shifti,periodi,min20i] <- NA
          CN_chum[idate,shifti,periodi,min20i] <- NA
          vCN_chin[idate,shifti,periodi,min20i] <- NA
          vCN_chum[idate,shifti,periodi,min20i] <- NA
          CN_650up[idate,shifti,periodi,min20i] <- NA
          CN_650down[idate,shifti,periodi,min20i] <- NA
        }
        a <- with(all_sonar, station=="Chena South" & date==datei & shift==shifti & shift_hr==periodi & min20==min20i)
        if(sum(a)==0) {
          CS_chin[idate,shifti,periodi,min20i] <- NA
          CS_chum[idate,shifti,periodi,min20i] <- NA
          vCS_chin[idate,shifti,periodi,min20i] <- NA
          vCS_chum[idate,shifti,periodi,min20i] <- NA
          CS_650up[idate,shifti,periodi,min20i] <- NA
          CS_650down[idate,shifti,periodi,min20i] <- NA
        }
        a <- with(all_sonar, station=="Salcha North" & date==datei & shift==shifti & shift_hr==periodi & min20==min20i)
        if(sum(a)==0) {
          SN_chin[idate,shifti,periodi,min20i] <- NA
          SN_chum[idate,shifti,periodi,min20i] <- NA
          vSN_chin[idate,shifti,periodi,min20i] <- NA
          vSN_chum[idate,shifti,periodi,min20i] <- NA
          SN_650up[idate,shifti,periodi,min20i] <- NA
          SN_650down[idate,shifti,periodi,min20i] <- NA
        }
        a <- with(all_sonar, station=="Salcha South" & date==datei & shift==shifti & shift_hr==periodi & min20==min20i)
        if(sum(a)==0) {
          SS_chin[idate,shifti,periodi,min20i] <- NA
          SS_chum[idate,shifti,periodi,min20i] <- NA
          vSS_chin[idate,shifti,periodi,min20i] <- NA
          vSS_chum[idate,shifti,periodi,min20i] <- NA
          SS_650up[idate,shifti,periodi,min20i] <- NA
          SS_650down[idate,shifti,periodi,min20i] <- NA
        }
      }
    }
  }
  idate <- idate+1
}


## Some weird checks, basically looking at cases where only one sonar was operating on a river but not both.

sum(!is.na(CN_chin))                 # successful periods for Chena North
sum(!is.na(CS_chin))                 # successful periods for Chena South
sum(!is.na(CN_chin)&!is.na(CS_chin)) # successful periods for both

sum(!is.na(SN_chin))
sum(!is.na(SS_chin))
sum(!is.na(SN_chin)&!is.na(SS_chin))


## Each row is a count day, the columns are the numbers of successful counts for North, South, and both.

data.frame(x1=apply(!is.na(CN_chin), 1, sum),
           x2=apply(!is.na(CS_chin), 1, sum),
           x3=apply(!is.na(CN_chin)&!is.na(CS_chin), 1, sum))

data.frame(x1=apply(!is.na(SN_chin), 1, sum),
           x2=apply(!is.na(SS_chin), 1, sum),
           x3=apply(!is.na(SN_chin)&!is.na(SS_chin), 1, sum))


## same thing, but just looking at the first (top of the hour) 20-min period

data.frame(x1=apply(!is.na(CN_chin[,,,1]), 1, sum),
           x2=apply(!is.na(CS_chin[,,,1]), 1, sum),
           x3=apply(!is.na(CN_chin[,,,1])&!is.na(CS_chin[,,,1]), 1, sum))

data.frame(x1=apply(!is.na(SN_chin[,,,1]), 1, sum),
           x2=apply(!is.na(SS_chin[,,,1]), 1, sum),
           x3=apply(!is.na(SN_chin[,,,1])&!is.na(SS_chin[,,,1]), 1, sum))


## total counted (not omitting NA values)  - i don't remember why this was of interest

data.frame(x1=apply((CN_chin), 1, sum),
           x2=apply((CS_chin), 1, sum),
           x3=apply((CN_chin)+(CS_chin), 1, sum))

data.frame(x1=apply((SN_chin), 1, sum),
           x2=apply((SS_chin), 1, sum),
           x3=apply((SN_chin)+(SS_chin), 1, sum))


## total counted, top of the hour

data.frame(x1=apply((CN_chin[,,,1]), 1, sum),
           x2=apply((CS_chin[,,,1]), 1, sum),
           x3=apply((CN_chin[,,,1])+(CS_chin[,,,1]), 1, sum))

data.frame(x1=apply((SN_chin[,,,1]), 1, sum),
           x2=apply((SS_chin[,,,1]), 1, sum),
           x3=apply((SN_chin[,,,1])+(SS_chin[,,,1]), 1, sum))


## top of the hour successful counts for Chena - don't remember why this was of interest

t(apply(CN_chin[,,,1], 1, c))
t(apply(CN_chin[,,,1]+CS_chin[,,,1], 1, c))


## I suspect this is a leftover from something else

# sum(CN,na.rm=T)
# sum(CS,na.rm=T)
# sum(SN,na.rm=T)
# sum(SS,na.rm=T)
# 
# mean(is.na(CN))
# mean(is.na(CS))
# mean(is.na(CN)|is.na(CS))
# mean(is.na(SN))
# mean(is.na(SS))
# mean(is.na(SN)|is.na(SS))
# 
# mean((is.na(CN)&!is.na(CS)) | (!is.na(CN)&is.na(CS)))
# mean((is.na(SN)&!is.na(SS)) | (!is.na(SN)&is.na(SS)))




## This function re-creates the variance calculations for the visual data (from the spreadsheet).
## This calculates the variance due to expansion from incomplete counts, as well as the expansion
## itself.  Since sonar counts are often incomplete, we needed a way to apply an equivalent 
## expansion to the sonar totals if necessary.

## The assumption is made that the variances due to expansion and due to species apportionment
## (in the case of sonar) are independent of one another, and can be summed.  Note that the variance
## due to species apportionment is propagated through the expansion.

expansion_sheet <- function(x, vx=NULL, worstcase=T) {
  Sydij <- apply(x, 1:2, sum, na.rm=T)         # count per shift
  # Sydi <- rowSums(Sydij)                       # count per day
  mdi <- apply(!is.na(x), 1:2, sum, na.rm=T)   # number of periods sampled per shift
  Sydij[mdi<4] <- 0 # or NA
  mdi[mdi<4] <- 0
  md <- rowSums(mdi)                           # number of periods sampled per day
  hdi <- mdi>0                                 # shifts with counts
  hd <- rowSums(hdi)                           # number of shifts with counts per day
  Md <- 72                                     # total possible periods per day
  Mdi <- 24                                    # total possible periods per shift
  Hd <- 3                                      # total possible shifts per day
  Ydi <- Mdi/mdi*Sydij                         # expanded count per shift
  Ydavg <- rowMeans(Ydi,na.rm=T)               # avg shift escapement per day
  Nd <- Ydavg*Hd                               # expanded daily escapement
  
  if(!is.null(vx)) {
    assign_var_raw <- apply(vx,1, sum, na.rm=T)
    mdi[mdi==0] <- NA  ### maybe???
    assign_var_expanded <- ((Hd/hd)^2)*rowSums(((Mdi/mdi)^2)*apply(vx, 1:2, sum, na.rm=T), na.rm=T)
  }
  
  RDS <- (x[,,-1,]-x[,,-8,])^2
  sumRDS <- apply(RDS, 1:2, sum, na.rm=T)
  s22di <- sumRDS/2/(mdi-1)
  s21d <- rowSums((Ydi-Ydavg)^2,na.rm=T)/(hd-1)
  f1d <- hd/Hd
  f2di <- mdi/Mdi
  V1 <- (1-f1d)*(Hd^2)*s21d/hd
  infcheck <- (1-f2di)*(Mdi^2)*s22di/mdi  # expansion sheet removes vals where mdi==0 in sum
  infcheck[is.infinite(infcheck)] <- NA
  V2 <- (1/f1d)*rowSums(infcheck, na.rm=T)
  VNd <- V1+V2
  
  ## -- this section was for applying the interpolation (with variance).
  ## -- decided to interpolate AFTER putting sonar & visual together.
  # if(worstcase) {
  #   cvmax <- max(sqrt(VNd)/Nd, na.rm=T)
  #   vworstcase <- (Nd*cvmax)^2
  #   VNd[is.na(VNd)] <- vworstcase[is.na(VNd)]
  # }
  
  # # this is where I should figure out how to interpolate
  # # interpolate where md==0
  # md0 <- md==0
  # md01 <- md02 <- F
  # n <- length(md0)
  # md01[-n] <- md0[-1]
  # md02[-1] <- md0[-n]
  # # wait... should interpolate AFTER putting sonar & visual together
  # # do need to take out Nd where hd==1 (i think)
  Nd[md==0 | hd==1] <- NA
  
  if(!is.null(vx)) out <- data.frame(count_raw=apply(x,1,sum,na.rm=T), count_expanded=Nd, var_expansion=VNd,
                                     nperiods1=mdi[,1],nperiods2=mdi[,2],nperiods3=mdi[,3],
                                     var_assign_raw=assign_var_raw, var_assign_expanded=assign_var_expanded,
                                     var_total=VNd+assign_var_expanded)
  if(is.null(vx)) out <- data.frame(count_raw=apply(x,1,sum,na.rm=T), count_expanded=Nd, var_expansion=VNd,
                                    nperiods1=mdi[,1],nperiods2=mdi[,2],nperiods3=mdi[,3])
  return(out)
}




## applying the expansion & expansion variance to the sonar output.
## Note: sonar stations are summed for each river, so one missing sonar results 
## in a no count (hence the checks above)

Cchin_expanded <- expansion_sheet(x=CN_chin+CS_chin, vx=vCN_chin+vCS_chin)
Cchum_expanded <- expansion_sheet(CN_chum+CS_chum, vx=vCN_chum+vCS_chum)
Schin_expanded <- expansion_sheet(SN_chin+SS_chin, vx=vSN_chin+vSS_chin)
Schum_expanded <- expansion_sheet(SN_chum+SS_chum, vx=vSN_chum+vSS_chum)

C650up_expanded <- expansion_sheet(x=CN_650up+CS_650up)
C650down_expanded <- expansion_sheet(CN_650down+CS_650down)
S650up_expanded <- expansion_sheet(SN_650up+SS_650up)
S650down_expanded <- expansion_sheet(SN_650down+SS_650down)







######################################################
#
#   expanding the visual data
#
######################################################




## Re-expressing the visual counts in the same array format as the sonar totals.
## Note: visual counts were only conducted for the top (first 20-min) block of 
## each hour, so visual data is only filled in for [,,,1]

Schin_vis <- Schum_vis <- Cchin_vis <- Cchum_vis <- NA*SN_chin
for(i in 1:3) {
  Cnr <- nrow(Chena_2025_Chin_vis)
  Snr <- nrow(Salcha_2025_Chin_vis)
  Cchin_vis[,i,,1] <- as.matrix(Chena_2025_Chin_vis[seq(i,Cnr,by=3),3:10])
  Cchum_vis[,i,,1] <- as.matrix(Chena_2025_Chum_vis[seq(i,Cnr,by=3),3:10])
  Schin_vis[,i,,1] <- as.matrix(Salcha_2025_Chin_vis[seq(i,Snr,by=3),3:10])
  Schum_vis[,i,,1] <- as.matrix(Salcha_2025_Chum_vis[seq(i,Snr,by=3),3:10])
}

## expandorizing
Cchin_vis_expanded <- expansion_sheet(x=Cchin_vis)
Cchum_vis_expanded <- expansion_sheet(x=Cchum_vis)
Schin_vis_expanded <- expansion_sheet(x=Schin_vis)
Schum_vis_expanded <- expansion_sheet(x=Schum_vis)

## compiling all sources of estimates into one data.frame for each species/river
Cchin_ests <- cbind(Cchin_vis_expanded, Cchin_expanded, C650up_expanded[,1:3])
Cchum_ests <- cbind(Cchum_vis_expanded, Cchum_expanded, C650down_expanded[,1:3])
Schin_ests <- cbind(Schin_vis_expanded, Schin_expanded, S650up_expanded[,1:3])
Schum_ests <- cbind(Schum_vis_expanded, Schum_expanded, S650down_expanded[,1:3])
colnames(Cchin_ests) <- colnames(Cchum_ests) <- colnames(Schin_ests) <- colnames(Schum_ests) <- 
  c(paste("vis",colnames(Cchin_vis_expanded),sep="_"), paste("sonar",colnames(Cchin_expanded),sep="_"), paste("650thresh",colnames(C650up_expanded[,1:3]),sep="_"))
rownames(Cchin_ests) <- rownames(Cchum_ests) <- rownames(Schin_ests) <- rownames(Schum_ests) <- dates




### looking for possible discrepancies between above expansion and what occurred
### within the spreadsheet
Cchin_sheet_raw <- read.csv("2025/flat_data/ChenaKing_expansionsheet.csv")
Cchum_sheet_raw <- read.csv("2025/flat_data/ChenaChum_expansionsheet.csv")
Schin_sheet_raw <- read.csv("2025/flat_data/SalchaKing_expansionsheet.csv")
Schum_sheet_raw <- read.csv("2025/flat_data/SalchaChum_expansionsheet.csv")

datesmasher <- function(x, year=2025) { # horrible kludge that turns 1-Jul into 2025-07-01
  x1 <- x
  x1[nchar(x1) < 6] <- paste0(0, x1[nchar(x1) < 6])
  dd <- substr(x1, 1, 2)
  mm <- substr(x1, 4, 6)
  mm1 <- mm
  mm1[mm1 %in% c("may","May")] <- "05"
  mm1[mm1 %in% c("jun","Jun")] <- "06"
  mm1[mm1 %in% c("jul","Jul")] <- "07"
  mm1[mm1 %in% c("aug","Aug")] <- "08"
  mm1[mm1 %in% c("sep","Sep")] <- "09"
  paste(year,mm1, dd, sep="-")
}

# keeping every third row
Cchin_sheet <- Cchin_sheet_raw[seq(3, nrow(Cchin_sheet_raw), by=3),] %>%
  select(c("Day","N.d.Yd.avg..Hd", "var.Nd.")) %>%
  rename(N=N.d.Yd.avg..Hd, V=var.Nd., date=Day) %>%
  mutate(date = datesmasher(date)) %>%
  mutate(V = as.numeric(V)) %>%    ## NA coercion warning is expected (#DIV/0! in Excel)   
  mutate(N = as.numeric(N))
Cchum_sheet <- Cchum_sheet_raw[seq(3, nrow(Cchin_sheet_raw), by=3),] %>%
  select(c("Day","N.d.Yd.avg..Hd", "var.Nd.")) %>%
  rename(N=N.d.Yd.avg..Hd, V=var.Nd., date=Day) %>%
  mutate(date = datesmasher(date)) %>%
  mutate(V = as.numeric(V)) %>%    ## NA coercion warning is expected (#DIV/0! in Excel)
  mutate(N = as.numeric(N))
Schin_sheet <- Schin_sheet_raw[seq(3, nrow(Cchin_sheet_raw), by=3),] %>%
  select(c("Day","N.d.Yd.avg..Hd", "var.Nd.")) %>%
  rename(N=N.d.Yd.avg..Hd, V=var.Nd., date=Day) %>%
  mutate(date = datesmasher(date)) %>%
  mutate(V = as.numeric(V)) %>%    ## NA coercion warning is expected (#DIV/0! in Excel)
  mutate(N = as.numeric(N))
Schum_sheet <- Schum_sheet_raw[seq(3, nrow(Cchin_sheet_raw), by=3),] %>%
  select(c("Day","N.d.Yd.avg..Hd", "var.Nd.")) %>%
  rename(N=N.d.Yd.avg..Hd, V=var.Nd., date=Day) %>%
  mutate(date = datesmasher(date)) %>%
  mutate(V = as.numeric(V)) %>%    ## NA coercion warning is expected (#DIV/0! in Excel)
  mutate(N = as.numeric(N))

range(Cchin_sheet$date)
range(row.names(Cchin_ests))
range(Cchum_sheet$date)
range(row.names(Cchum_ests))
range(Schin_sheet$date)
range(row.names(Schin_ests))
range(Schum_sheet$date)
range(row.names(Schum_ests))

checkthem <- function(ests, sheet) {
  x <- data.frame(date=sheet$date,
                  n_ests=ests$vis_count_expanded,
                  n_sheet=sheet$N,
                  v_ests=ests$vis_var_expansion,
                  v_sheet=sheet$V)
  logi <- ((!is.na(x$n_ests) & !is.na(x$n_sheet) & (abs(x$n_ests - x$n_sheet) > 1))) |
    ((!is.na(x$v_ests) & !is.na(x$v_sheet) & (abs(x$v_ests - x$v_sheet) > 1))) |
    (is.na(x$n_ests) & !is.na(x$n_sheet) & (x$n_sheet != 0)) |
    (is.na(x$v_ests) & !is.na(x$v_sheet) & (x$v_sheet != 0)) |
    (is.na(x$n_sheet) & !is.na(x$n_ests)) |
    (is.na(x$v_sheet) & !is.na(x$v_ests))
  
  x[logi,]
}
checkthem(ests=Cchin_ests, sheet=Cchin_sheet)

checkthem(ests=Cchum_ests, sheet=Cchum_sheet)
#          date n_ests n_sheet v_ests v_sheet
# 48 2025-08-09     NA     144    NaN      NA   - hd==1, count removed
# 50 2025-08-11     NA     108    NaN      NA   - hd==1, count removed

checkthem(ests=Schin_ests, sheet=Schin_sheet)
#          date n_ests n_sheet v_ests v_sheet
# 42 2025-08-03     NA      18    NaN      NA   - hd==1, count removed
# 44 2025-08-05     NA      27    NaN      NA   - hd==1, count removed

checkthem(ests=Schum_ests, sheet=Schum_sheet)
#          date n_ests n_sheet   v_ests v_sheet
# 25 2025-07-17     45      45 217.2857     171   - bug in expansion sheet xlsx!!  RDS columns for this region of dates
# 42 2025-08-03     NA     144      NaN      NA   - hd==1, count removed
# 44 2025-08-05     NA     279      NaN      NA   - hd==1, count removed







######################################################
#
#   plotting the visual vs sonar counts for both species
#
######################################################



## kludgy function for plotting all 3 sources of estimates (visual, sonar, 650mm threshold)

plot_ests <- function(x,...) {
  vis95lo <- x$vis_count_expanded - 2*sqrt(x$vis_var_expansion)
  vis95hi <- x$vis_count_expanded + 2*sqrt(x$vis_var_expansion)
  son95lo <- x$sonar_count_expanded - 2*sqrt(x$sonar_var_total)
  son95hi <- x$sonar_count_expanded + 2*sqrt(x$sonar_var_total)
  thr95lo <- x$`650thresh_count_expanded` - 2*sqrt(x$`650thresh_var_expansion`)
  thr95hi <- x$`650thresh_count_expanded` + 2*sqrt(x$`650thresh_var_expansion`)
  
  datex <- as.Date(rownames(x))
  plot(as.Date(rownames(x)), x$vis_count_expanded, ylim=c(0, max(vis95hi,son95hi,thr95hi,na.rm=T)), type='l', col=4, lwd=2, xlab="",ylab="",...=...)
  # lines(as.Date(rownames(x)), vis95hi, col=4, lwd=1)
  # lines(as.Date(rownames(x)), vis95lo, col=4, lwd=1)
  for(i in 2:length(datex)) polygon(c(datex[c(i-1,i)],rev(datex[c(i-1,i)])), c(vis95lo[c(i-1,i)],rev(vis95hi[c(i-1,i)])), border=NA, col=adjustcolor(4, alpha.f=.2))
  lines(as.Date(rownames(x)), x$sonar_count_expanded, col=2, lwd=2)
  # lines(as.Date(rownames(x)), son95hi, col=2, lwd=1)
  # lines(as.Date(rownames(x)), son95lo, col=2, lwd=1)
  for(i in 2:length(datex)) polygon(c(datex[c(i-1,i)],rev(datex[c(i-1,i)])), c(son95lo[c(i-1,i)],rev(son95hi[c(i-1,i)])), border=NA, col=adjustcolor(2, alpha.f=.2))
  lines(as.Date(rownames(x)), x$`650thresh_count_expanded`, col=6, lwd=2)
  # lines(as.Date(rownames(x)), son95hi, col=2, lwd=1)
  # lines(as.Date(rownames(x)), son95lo, col=2, lwd=1)
  for(i in 2:length(datex)) polygon(c(datex[c(i-1,i)],rev(datex[c(i-1,i)])), c(thr95lo[c(i-1,i)],rev(thr95hi[c(i-1,i)])), border=NA, col=adjustcolor(6, alpha.f=.2))
  legend("topleft",fill=adjustcolor(c(4,2,6),alpha.f=.3),legend=c("visual","sonar","650mm"))
}
par(mfrow=c(2,2))
plot_ests(x=Cchin_ests, main="Chena Chinook")
plot_ests(x=Schin_ests, main="Salcha Chinook")
plot_ests(x=Cchum_ests, main="Chena Chum")
plot_ests(x=Schum_ests, main="Salcha Chum")




# ## This was a check to look at the proportions from the 650mm threshold over time,
# ## as well as the amount of data.  2018 had a monstrous run of chums that played havoc
# ## with the threshold approach output.
# 
# par(mfrow=c(2,1))
# # mosaicplot(date~(length>=650), data=subset(all_fish, river=="Chena"), main="Chena")
# mosaicplot(date~(length>=650), data=subset(all_fish, river=="Salcha"), main="Salcha")
# # mosaicplot(date~(modlength>=650), data=subset(all_fish, river=="Chena"), main="Chena")
# mosaicplot(date~(modlength>=650), data=subset(all_fish, river=="Salcha"), main="Salcha")







######################################################
#
#   Applying the running average method and Hamachan model
#
######################################################


## Applying the running-average interpolation method where applicable, then the 
## Hamachan hierarchical run-timing model.  Historical note: I'm not sure if the 
## "Hamachan" model was actually created by Hamachan (and I don't think it is.)
## He uses it on the Kusko, that's how we knew about it - and we've referred to 
## it as the Hamachan model ever since

# first interpolate, then hamachan

runningavg_interp <- function(x) {
  avgcv <- mean(sqrt(x$vis_var_expansion)/x$vis_count_expanded, na.rm=T)
  
  ## original version
  # est <- ifelse(!is.na(x$vis_count_expanded), x$vis_count_expanded, x$sonar_count_expanded)
  
  ## 2025 version - took out sonar
  est <- x$vis_count_expanded
  
  
  # estvar <- ifelse(!is.na(x$vis_count_expanded), x$vis_var_expansion, x$sonar_var_expansion)
  interp_count <- interp_var <- rep(NA,nrow(x))
  for(i in 2:(length(est)-1)) {
    runcheck <- is.na(est[(i-1):(i+1)])
    if(all(runcheck==c(F,T,F))) {
      interp_count[i] <- mean(est[(i-1):(i+1)], na.rm=T)
      interp_var[i] <- (avgcv*interp_count[i])^2
    }
  }
  for(i in 3:(length(est)-2)) {
    runcheck <- is.na(est[(i-2):(i+2)])
    if(all(runcheck==c(F,T,T,F,F)) | all(runcheck==c(F,F,T,T,F))) {
      interp_count[i] <- mean(est[(i-2):(i+2)], na.rm=T)
      interp_var[i] <- (avgcv*interp_count[i])^2
    }
  }
  x1 <- cbind(x,interp_count,interp_var)
}
Cchin_ests1 <- runningavg_interp(x=Cchin_ests)
Schin_ests1 <- runningavg_interp(x=Schin_ests)
Cchum_ests1 <- runningavg_interp(x=Cchum_ests)
Schum_ests1 <- runningavg_interp(x=Schum_ests)


# Hamachan all the things!!


## These datasets get updated every year

## IMPORTANT: all these counts start June 23
Cchin_histo <- read.csv("2025/flat_data/ChenaKing_historic_runtiming.csv", skip=4, nrows=54)
Cchum_histo <- read.csv("2025/flat_data/ChenaChum_historic_runtiming.csv", skip=3, nrows=54)
Schin_histo <- read.csv("2025/flat_data/SalchaKing_historic_runtiming.csv", skip=4, nrows=84)
Schum_histo <- read.csv("2025/flat_data/SalchaChum_historic_runtiming.csv", skip=4, nrows=84)


# ## fixorizing inputs
# Schin_histo <- Schin_histo[,colSums(!is.na(Schin_histo))>0]  # removing empty columns
# Schum_histo <- Schum_histo[,colSums(!is.na(Schum_histo))>0]
# Cchin_histo <- Cchin_histo[,colSums(!is.na(Cchin_histo))>0]  # removing empty columns
# Cchum_histo <- Cchum_histo[,colSums(!is.na(Cchum_histo))>0]

Cchin_histo_counts <- Cchin_histo[,seq(2,ncol(Cchin_histo),by=2)]
Cchum_histo_counts <- Cchum_histo[,seq(2,ncol(Cchum_histo),by=2)]
Schin_histo_counts <- Schin_histo[,seq(2,ncol(Schin_histo),by=2)]
Schum_histo_counts <- Schum_histo[,seq(2,ncol(Schum_histo),by=2)]
Cchin_histo_counttype <- Cchin_histo[,seq(3,ncol(Cchin_histo),by=2)]
Cchum_histo_counttype <- Cchum_histo[,seq(3,ncol(Cchum_histo),by=2)]
Schin_histo_counttype <- Schin_histo[,seq(3,ncol(Schin_histo),by=2)]
Schum_histo_counttype <- Schum_histo[,seq(3,ncol(Schum_histo),by=2)]

## censoring data types "not allowed".  Haven't really messed with this much.
## v=visual, s=sonar, 0=interpolated somehow
notallowed <- c(0)
for(i in 1:length(notallowed)) {  # still works, but would have been more efficient with %in%
  Cchin_histo_counts[Cchin_histo_counttype == notallowed[i]] <- NA
  Cchum_histo_counts[Cchum_histo_counttype == notallowed[i]] <- NA
  Schin_histo_counts[Schin_histo_counttype == notallowed[i]] <- NA
  Schum_histo_counts[Schum_histo_counttype == notallowed[i]] <- NA
}

colnames(Cchin_histo_counts) <- paste0("count",1993:2024)
colnames(Cchum_histo_counts) <- paste0("count",1993:2024)
colnames(Schin_histo_counts) <- paste0("count",1993:2024)
colnames(Schum_histo_counts) <- paste0("count",1993:2024)
# colnames(Schin_histo_counts) <- paste0("count",(1993:2023)[!(1993:2023 %in% c(2018, 2020))])
# colnames(Schum_histo_counts) <- paste0("count",(1993:2023)[!(1993:2023 %in% c(2018, 2020))])




## setting up to use current year's available daily estimates in the Hamachan model

getests <- function(x) ifelse(!is.na(x$vis_count_expanded), x$vis_count_expanded, x$sonar_count_expanded)

# match these with dates
Cchin_histo_counts$count2025 <- NA
Cchum_histo_counts$count2025 <- NA
Schin_histo_counts$count2025 <- NA
Schum_histo_counts$count2025 <- NA

# IMPORTANT: the 1:nrow() bit relies on _ests1 and _histo_counts starting on Jun 23
# Cchin_histo_counts$count2025 <- getests(Cchin_ests1[1:nrow(Cchin_histo_counts),])
# Cchum_histo_counts$count2025 <- getests(Cchum_ests1[1:nrow(Cchum_histo_counts),])
# Schin_histo_counts$count2025[1:nrow(Schin_ests1)] <- getests(Schin_ests1)
# Schum_histo_counts$count2025[1:nrow(Schin_ests1)] <- getests(Schum_ests1)



### NEW FOR 2025:
### Since we don't trust the sonar data, ONLY USE VISUAL FOR HAMACHAN INPUT
Cchin_histo_counts$count2025 <- Cchin_ests1$vis_count_expanded[1:nrow(Cchin_histo_counts)]
Cchum_histo_counts$count2025 <- Cchum_ests1$vis_count_expanded[1:nrow(Cchum_histo_counts)]
Schin_histo_counts$count2025[1:nrow(Schin_ests1)] <- Schin_ests1$vis_count_expanded
Schum_histo_counts$count2025[1:nrow(Schin_ests1)] <- Schum_ests1$vis_count_expanded



# # The previous model was intended to run both rivers together, to account for 
# # the fact that run timing is likely related between the two.
# # I don't think it's doing so correctly, and decided to run each river
# # independently.
# hiermod1_jags <- tempfile()
# cat('model {
#     
#   for(j in 1:nyrs) {
#     for(i in 1:ndays){
#     y1[i,j] ~ dnorm(theta1[i,j], tausq1[j])
#       #    y1[i,j] ~ dpois(theta1[i,j])    
#       # Assume that run timing distribution takes log normal distribution 
#       theta1[i,j] <- a1[j]*exp(-0.5*pow(log(x[i]/mu1[j])/b1[j],2))
#       # Assume that run timing distribution takes Extreme value distribution 
#         # theta1[i,j] <- a1[j]*exp(-exp(-(x[i]-mu1[j])/b1[j])-(x[i]-mu1[j])/b1[j]+1)
#       # Assume that run timing distribution takes log-logistic distribution 
#         # theta1[i,j] <- (a1[j]*(b1[j]/mu1[j])*pow((x[i]/mu1[j]),b1[j]-1))/pow(1+pow((x[i]/mu1[j]),b1[j]),2)
#       
#       y2[i,j] ~ dnorm(theta2[i,j], tausq2[j])
#       #    y2[i,j] ~ dpois(theta2[i,j])    
#       # Assume that run timing distribution takes log normal distribution 
#       theta2[i,j] <- a2[j]*exp(-0.5*pow(log(x[i]/mu2[j])/b2[j],2))
#       # Assume that run timing distribution takes Extreme value distribution 
#       #   theta2[i,j] <- a2[j]*exp(-exp(-(x[i]-mu2[j])/b2[j])-(x[i]-mu2[j])/b2[j]+1)
#       # Assume that run timing distribution takes log-logistic distribution 
#       #   theta2[i,j] <- (a2[j]*(b2[j]/mu2[j])*pow((x[i]/mu2[j]),b2[j]-1))/pow(1+pow((x[i]/mu2[j]),b2[j]),2)   
#     }
#   }
#     # a[] indicates the maximum height (amplitude) of the function a>0
#     # mu[] indicates the function peaks when x = mu mu>0 : Peak timing
#     # b[] indicates peak width of the function b>0 standard deviation
#     
#     # Priors
#   for(i in 1:nyrs) {
#     # Normal distribution Positive only 
#     #  a: is independent not hierarchical 
#     a1[i] ~ dnorm(0,0.00001)T(0,)
#     b1[i] ~ dnorm(b01,b01.prec)T(0.16,)
#     # b1[i] <- b2[i]
#     # mu1[i] ~ dnorm(mu01,mu01.prec)T(0,)
#     # mu1[i] ~ dnorm(mu2[i],prec.mu)
#     mu1[i] <- mu2[i] + eps[i]
#     eps[i] ~ dnorm(mueps,preceps)T(-mu2[i],)
#     
#     a2[i] ~ dnorm(0,0.00001)T(0,)
#     b2[i] ~ dnorm(b02,b02.prec)T(0.16,)
#     mu2[i] ~ dnorm(mu02,mu02.prec)T(0,)   
#   }  
#   mueps ~ dnorm(0,0.01)
#   preceps <- pow(sigeps,-2)
#   sigeps ~ dunif(0,4)
#   prec.mu <- pow(sig.mu,-2)
#   sig.mu ~ dunif(0,2)
#     
#   b01 ~ dnorm(0.5,0.001)T(0.16,)
#   mu01 ~ dnorm(25,0.001)T(0,)
#   b01.prec <-1/b01.ssq 
#   b01.ssq <- b01.sigma*b01.sigma
#   b01.sigma ~ dunif(0,100)  
#   mu01.prec <-1/mu01.ssq 
#   mu01.ssq <- mu01.sigma*mu01.sigma
#   mu01.sigma ~ dunif(0,100) 
#     
#   b02 ~ dnorm(0.5,0.001)T(0.16,)
#   mu02 ~ dnorm(25,0.001)T(0,)
#   b02.prec <-1/b02.ssq 
#   b02.ssq <- b02.sigma*b02.sigma
#   b02.sigma ~ dunif(0,100)  
#   mu02.prec <-1/mu02.ssq 
#   mu02.ssq <- mu02.sigma*mu02.sigma
#   mu02.sigma ~ dunif(0,100)  
#     
#     ## This assumes that variance of each year is independent.     
#   for(i in 1:nyrs) {    
#     tausq1[i] <- pow(sigma1[i],-2)
#     sigma1[i] ~ dunif(0,100) 
#     
#     tausq2[i] <- pow(sigma2[i],-2)
#     sigma2[i] ~ dunif(0,100) 
#   }
#     
#     # Backestimate escapement 
#   for(j in 1:nyrs){
#     for(i in 1:ndays){ 
#       y1est[i,j] <- y1[i,j]
#       y2est[i,j] <- y2[i,j]
#     }
#   }
#     # missing <- sum(y1est[1:20,12])
#     
# }', file=hiermod1_jags)


hiermod2_jags <- tempfile()
cat('model {
    
  for(j in 1:nyrs) {
    for(i in 1:ndays){
    y1[i,j] ~ dnorm(theta1[i,j], tausq1[j])
    y1pp[i,j] ~ dnorm(theta1[i,j], tausq1[j])
      #    y1[i,j] ~ dpois(theta1[i,j])    
      # Assume that run timing distribution takes log normal distribution 
      theta1[i,j] <- a1[j]*exp(-0.5*pow(log(x[i]/mu1[j])/b1[j],2))
      # Assume that run timing distribution takes Extreme value distribution 
        # theta1[i,j] <- a1[j]*exp(-exp(-(x[i]-mu1[j])/b1[j])-(x[i]-mu1[j])/b1[j]+1)
      # Assume that run timing distribution takes log-logistic distribution 
        # theta1[i,j] <- (a1[j]*(b1[j]/mu1[j])*pow((x[i]/mu1[j]),b1[j]-1))/pow(1+pow((x[i]/mu1[j]),b1[j]),2)
    }
  }
    # a[] indicates the maximum height (amplitude) of the function a>0
    # mu[] indicates the function peaks when x = mu mu>0 : Peak timing
    # b[] indicates peak width of the function b>0 standard deviation
    
    # Priors
  for(i in 1:nyrs) {
    # Normal distribution Positive only 
    #  a: is independent not hierarchical 
    a1[i] ~ dnorm(0,0.05)T(0,)            # was dnorm(0,0.00001), 0.05 worked okay
    b1[i] ~ dnorm(b01,b01.prec)T(0.16,)
    # b1[i] <- b2[i]
    # mu1[i] ~ dnorm(mu01,mu01.prec)T(0,)
    # mu1[i] ~ dnorm(mu2[i],prec.mu)
    mu1[i]  ~ dnorm(mu01,mu01.prec)T(0,)   
  }  
  
  prec.mu <- pow(sig.mu,-2)
  sig.mu ~ dunif(0,2)
    
  b01 ~ dnorm(0.5,0.001)T(0.16,)
  mu01 ~ dnorm(25,0.001)T(0,)
  b01.prec <-1/b01.ssq 
  b01.ssq <- b01.sigma*b01.sigma
  b01.sigma ~ dunif(0,100)  
  mu01.prec <-1/mu01.ssq 
  mu01.ssq <- mu01.sigma*mu01.sigma
  mu01.sigma ~ dunif(0,100) 
    
    ## This assumes that variance of each year is independent.     
  for(i in 1:nyrs) {    
    tausq1[i] <- pow(sigma1[i],-2)
    sigma1[i] ~ dunif(0,100) 
  }
    
    # Backestimate escapement 
  for(j in 1:nyrs){
    for(i in 1:ndays){ 
      y1est[i,j] <- y1[i,j]
    }
  }
    # missing <- sum(y1est[1:20,12])
    
}', file=hiermod2_jags)

# runHamachan <- function(y1,y2,n.iter=5000,msg="",tryitonce=F,...) {
#   y1[y1<0]<-0
#   y2[y2<0]<-0
#   hiermod1_jags_data <- list(y1=log(y1+1),
#                              y2=log(y2+1),
#                              nyrs=dim(y1)[2],
#                              ndays=dim(y1)[1],
#                              x=1:nrow(y1))
#   # n.iter <- 5000   # 5000 took 1.4 min in parallel
#   didit <- F
#   tries <- 1
#   while(!didit) {
#     t.start <- Sys.time()
#     print(paste(msg,"try",tries))
#     print(t.start)
#     if(tryitonce) {
#       hiermod1_jags_out <- jagsUI::jags(model.file=hiermod1_jags,
#                                         data=hiermod1_jags_data, 
#                                         parameters.to.save=c("y1est","y2est","eps","b1","b2","mu1","mu2","sigma1","sigma2","a1","a2"), 
#                                         n.chains=ncores, parallel=T, n.iter=n.iter, n.burnin=n.iter/2, n.adapt=n.iter/10, n.thin=n.iter/1000,
#                                         ...=...)
#       didit <- T
#     }
#     else hiermod1_jags_out <- tryCatch(jagsUI::jags(model.file=hiermod1_jags,
#                                                     data=hiermod1_jags_data, 
#                                                     parameters.to.save=c("y1est","y2est","eps","b1","b2","mu1","mu2","sigma1","sigma2","a1","a2"), 
#                                                     n.chains=ncores, parallel=T, n.iter=n.iter, n.burnin=n.iter/2, n.adapt=n.iter/10, n.thin=n.iter/1000,
#                                                     ...=...),
#                                        error=function(x) NA)
#     if(length(hiermod1_jags_out)>1) didit<-T
#     tries <- tries+1
#     time <- Sys.time()-t.start
#     print(time)
#   }
#   return(hiermod1_jags_out)
# }


# an alternate version that runs one river/species combo at a time (more stable)
runHamachan <- function(y1,n.iter=5000,msg="",tryitonce=F,...) {
  y1[y1<0]<-0
  hiermod2_jags_data <- list(y1=log(y1+1),
                             nyrs=dim(y1)[2],
                             ndays=dim(y1)[1],
                             x=1:nrow(y1))
  # n.iter <- 5000   # 5000 took 1.4 min in parallel
  didit <- F
  tries <- 1
  while(!didit) {
    t.start <- Sys.time()
    print(paste(msg,"try",tries))
    print(t.start)
    if(tryitonce) {
      hiermod2_jags_out <- jagsUI::jags(model.file=hiermod2_jags,
                                        data=hiermod2_jags_data, 
                                        parameters.to.save=c("y1est","b1","mu1","sigma1","a1","theta1",
                                                             "y1pp",
                                                             "b01", "b01.sigma", "mu01", "mu01.sigma"), 
                                        n.chains=ncores, parallel=T, n.iter=n.iter, n.burnin=n.iter/2, n.adapt=n.iter/10, n.thin=n.iter/1000,
                                        ...=...)
      didit <- T
    }
    else hiermod2_jags_out <- tryCatch(jagsUI::jags(model.file=hiermod2_jags,
                                                    data=hiermod2_jags_data, 
                                                    parameters.to.save=c("y1est","b1","mu1","sigma1","a1","theta1",
                                                                         "y1pp",
                                                                         "b01", "b01.sigma", "mu01", "mu01.sigma"), 
                                                    n.chains=ncores, parallel=T, n.iter=n.iter, n.burnin=n.iter/2, n.adapt=n.iter/10, n.thin=n.iter/1000,
                                                    ...=...),
                                       error=function(x) NA)
    if(length(hiermod2_jags_out)>1) didit<-T
    tries <- tries+1
    time <- Sys.time()-t.start
    print(time)
  }
  return(hiermod2_jags_out)
}



## The Hamachan model can be temperamental.  It very often errors out with
## "invalid parent values" in mid run.  The runHamachan function above provides
## a wrapper for running the thing, and keeps trying until it succeeds.



# The section below (commented out) was not needed with the simplified model.

# ## This is kludgy.  In 2018 it took forever to get the model to run without erroring, so
# ## when it finally did, I harvested some of the posterior values to use as inits in case
# ## I had to run it again.  I saved them to an Rdata file, but here they are as hard-coded.
# ## Note that there are 26 values associated with each parameter, and 27 are needed - this
# ## will need to be updated in future years if this is done again.
# 
# # load(file="2018 materials/forinits.Rdata")
# mns <- list(#eps=-3.07521,
#             b1=c(0.4993585,0.4874376,0.7624875,0.9734876,0.458732,0.5421083,0.3211041,0.3740339,0.1929754,0.3908579,0.4122211,0.6864661,0.3759515,0.4443365,0.6278043,0.6584322,0.4933222,0.3012637,0.4994659,0.4840655,0.3097202,0.3172026,0.544756,0.479573,0.809854,0.3268056),
#             b2=c(0.5594515,0.4664597,0.6634599,1.082397,0.5895152,0.5607642,0.3615209,0.358652,0.2936067,0.4280528,0.335822,0.5988643,0.4539034,0.3820309,0.3282549,0.446211,0.4376694,0.3169996,0.4750557,0.546952,0.3782434,0.488768,0.5489337,0.4331722,0.5893685,0.4820904),
#             mu2=c(35.56875,37.19195,41.08907,27.05414,39.08196,43.53637,46.77643,41.9007,35.49535,41.63567,32.75191,41.18205,44.93722,39.49298,43.7797,48.54853,41.94573,42.10001,44.21258,47.85188,40.09372,38.20914,39.13038,29.63719,39.12832,45.70052),
#             sigma1=c(0.8410581,1.264184,0.7762708,0.835336,0.9054144,1.357112,0.7919183,0.5859112,0.6912784,0.2445944,0.8591711,0.7059833,1.340392,0.4699086,1.00802,1.290016,0.8248737,0.6178315,50.92243,0.7304966,1.097464,0.7134912,1.225191,0.4489991,1.13808,0.5346976),
#             sigma2=c(0.888907,0.842229,0.6036151,0.3796727,0.9886522,0.6087939,1.781811,0.6405888,1.012119,0.7453564,0.5691745,0.7107642,0.7832727,0.6425774,0.7383351,0.8215943,1.199256,0.7138716,0.2709813,0.4223435,0.9294938,50.15124,0.7383826,1.037929,0.9035162,0.6739997),
#             a1=c(5.678893,6.044671,5.454089,6.6836,6.516682,5.65599,6.91382,5.254823,6.601376,6.042483,4.67915,6.95134,7.479791,7.83172,5.62572,4.954238,6.795865,6.500094,251.0988,6.714561,7.791792,7.194905,6.140953,6.247392,6.94229,6.948424),
#             a2=c(5.601007,7.825206,7.197368,8.656482,7.682589,6.999347,6.786969,7.355442,7.302795,7.20858,11.44176,7.645368,9.355948,9.153506,7.464748,7.274597,7.793973,7.524785,8.475906,7.653075,8.592395,252.2626,6.458762,5.435573,7.444079,8.06277))
# sds <- list(#eps=0.6064488,
#             b1=c(0.03866983,0.05414206,0.1101233,0.2230508,0.04498995,0.08051076,0.02874224,0.02671431,0.01669528,0.08299616,0.1113003,0.03757654,0.07164957,0.02906789,0.06328428,0.09191997,0.04247819,0.01936726,0.2175975,0.02859066,0.02608488,0.02872582,0.06360887,0.09261977,0.1054313,0.02396206),
#             b2=c(0.04406355,0.02940756,0.04173194,0.2389003,0.04811137,0.03958778,0.03664379,0.02415809,0.02918349,0.07694738,0.0413929,0.01919289,0.01752968,0.01351291,0.03374409,0.03677125,0.03907535,0.01450575,0.03457909,0.0331126,0.02070704,0.1902916,0.07356101,0.07883926,0.07228649,0.02609204),
#             mu2=c(1.23545,0.9162987,1.62253,2.214778,1.830528,2.145404,1.478285,0.796168,0.8225886,4.291614,4.227743,0.5955652,0.692352,0.4171637,2.279478,1.397698,1.591942,0.5588188,1.132288,1.067957,0.7360707,1.227842,1.973257,4.206209,3.072517,1.576285),
#             sigma1=c(0.1074857,0.1677873,0.1550886,0.1841203,0.1130642,0.1972234,0.1077742,0.07409995,0.1081477,0.0722943,0.1455415,0.09164432,0.2895339,0.07996181,0.1318207,0.197919,0.1073783,0.08084683,28.96905,0.111376,0.1724392,0.1212499,0.1515441,0.09321352,0.1888806,0.07542445),
#             sigma2=c(0.1147809,0.1076489,0.07199613,0.1104888,0.118231,0.07495933,0.2034865,0.08360964,0.1903357,0.172397,0.09825959,0.06060466,0.07586093,0.08290737,0.1009956,0.1252546,0.1560355,0.08326016,0.06001122,0.05435268,0.1184784,28.97692,0.1264625,0.1770651,0.1179773,0.0822787),
#             a1=c(0.2125329,0.3143807,0.2596297,0.2396886,0.2503697,0.3794018,0.2603522,0.1553136,0.2669334,0.565404,0.4807141,0.1953144,0.5105327,0.1313666,0.3082988,0.6275289,0.2175612,0.1813053,194.9607,0.2629037,0.3407902,0.2268901,0.2987485,0.5399707,0.3201379,0.2338063),
#             a2=c(0.2086162,0.2075684,0.1281544,0.1332426,0.2559887,0.1800073,0.4950331,0.171989,0.7686177,0.7071601,4.143269,0.1584075,0.1747124,0.1650843,0.3554756,0.7650475,0.2988157,0.1939959,0.08604632,0.09677732,0.2370959,188.1118,0.222736,1.311115,0.2914468,0.277075))
# 
# nr <- ncol(Cchin_histo_counts)  # this used to be the 27
# haminits1 <- function() list(#eps=rnorm(1,mns$eps,sds$eps),
#                              b1=rnorm(nr,c(mns$b1,median(mns$b1)),c(sds$b1,median(sds$b1))),
#                              b2=rnorm(nr,c(mns$b2,median(mns$b2)),c(sds$b2,median(sds$b2))),
#                              mu2=rnorm(nr,c(mns$mu2,median(mns$mu2)),c(sds$mu2,median(sds$mu2))),
#                              sigma1=rnorm(nr,c(mns$sigma1,median(mns$sigma1)),c(sds$sigma1,median(sds$sigma1))),
#                              sigma2=rnorm(nr,c(mns$sigma2,median(mns$sigma2)),c(sds$sigma2,median(sds$sigma2))),
#                              a1=rnorm(nr,c(mns$a1,median(mns$a1)),c(sds$a1,median(sds$a1))),
#                              a2=rnorm(nr,c(mns$a2,median(mns$a2)),c(sds$a2,median(sds$a2))))
# # save(mns,sds,haminits,file="forinits.Rdata")


ncores <- 6


## had a bit of an issue getting the input data formatted correctly.  This is a way to do ONE test run
## rather than an infinite loop when the issue is with the data, not the MCMC!
# chin_hieroutTEST <- runHamachan(y1=Cchin_histo_counts, y2=Schin_histo_counts, n.iter=1000, tryitonce=T, inits=haminits1) #
# chum_hieroutTEST <- runHamachan(y1=Cchum_histo_counts, y2=Schum_histo_counts, n.iter=1000, tryitonce=T, inits=haminits1) #
test_model <- FALSE
if(test_model) {
  Cchin_hieroutTEST <- runHamachan(y1=Cchin_histo_counts, n.iter=1000, tryitonce=T) #, inits=haminits1
  Cchum_hieroutTEST <- runHamachan(y1=Cchum_histo_counts, n.iter=1000, tryitonce=T) # , inits=haminits1
  Schin_hieroutTEST <- runHamachan(y1=Schin_histo_counts, n.iter=1000, tryitonce=T) #, inits=haminits1
  Schum_hieroutTEST <- runHamachan(y1=Schum_histo_counts, n.iter=1000, tryitonce=T) # , inits=haminits1
}

## Doing it for real - 50k takes 15 minutes if it succeeds, 100k in 30

# 100k in about 25 min if it succeeds - 2000k in 8 hrs
# niter <- 2000*1000   # 100k still doesn't impressively converge
niter <- 30*1000   

# chin_hierout <- runHamachan(y1=Cchin_histo_counts, y2=Schin_histo_counts, n.iter=niter, msg="firstmod -", inits=haminits1) #
# chum_hierout <- runHamachan(y1=Cchum_histo_counts, y2=Schum_histo_counts, n.iter=niter, msg="secondmod -", inits=haminits1) #

run_model
if(run_model) {
  {
    Cchin_hierout <- runHamachan(y1=Cchin_histo_counts, n.iter=niter, msg="Cchin -") #, inits=haminits1
    Cchum_hierout <- runHamachan(y1=Cchum_histo_counts, n.iter=niter, msg="Cchum -") #, inits=haminits1
    Schin_hierout <- runHamachan(y1=Schin_histo_counts, n.iter=niter, msg="Schin -") #, inits=haminits1
    Schum_hierout <- runHamachan(y1=Schum_histo_counts, n.iter=niter, msg="Schum -") #, inits=haminits1
  }
  
}  else {
  load(file="2025/Rdata/hier_runtiming2025.Rdata")
}


#### the diagnostic & plotting section following won't work unless the 
#### models have been run.  Wrapped the full section in a logical.

run_model
if(run_model) {
  par(mfrow=c(2,2))
  plotRhats(Cchin_hierout)
  plotRhats(Cchum_hierout)
  plotRhats(Schin_hierout)
  plotRhats(Schum_hierout)
  
  traceworstRhat(Cchin_hierout, parmfrow=c(3,3))
  traceworstRhat(Cchum_hierout, parmfrow=c(3,3))  # looks overparameterized honestly
  traceworstRhat(Schin_hierout, parmfrow=c(3,3))
  traceworstRhat(Schum_hierout, parmfrow=c(3,3))
  
  tracedens_jags(Cchin_hierout, p=c("b1[33]", "mu1[33]", "sigma1[33]", "a1[33]"), parmfrow=c(2,2))
  tracedens_jags(Cchum_hierout, p=c("b1[33]", "mu1[33]", "sigma1[33]", "a1[33]"), parmfrow=c(2,2))
  tracedens_jags(Schin_hierout, p=c("b1[33]", "mu1[33]", "sigma1[33]", "a1[33]"), parmfrow=c(2,2))
  tracedens_jags(Schum_hierout, p=c("b1[33]", "mu1[33]", "sigma1[33]", "a1[33]"), parmfrow=c(2,2))
  
  tracedens_jags(Cchin_hierout, p=c(paste0("theta1[", which.max(Cchin_hierout$Rhat$theta1[,33]),",33]"),
                                    paste0("y1est[", which.max(Cchin_hierout$Rhat$y1est[,33]),",33]")))
  tracedens_jags(Cchum_hierout, p=c(paste0("theta1[", which.max(Cchum_hierout$Rhat$theta1[,33]),",33]"),
                                    paste0("y1est[", which.max(Cchum_hierout$Rhat$y1est[,33]),",33]")))
  tracedens_jags(Schin_hierout, p=c(paste0("theta1[", which.max(Schin_hierout$Rhat$theta1[,33]),",33]"),
                                    paste0("y1est[", which.max(Schin_hierout$Rhat$y1est[,33]),",33]")))
  tracedens_jags(Schum_hierout, p=c(paste0("theta1[", which.max(Schum_hierout$Rhat$theta1[,33]),",33]"),
                                    paste0("y1est[", which.max(Schum_hierout$Rhat$y1est[,33]),",33]")))
  
  tracedens_jags(Cchin_hierout, p=c(paste0("y1est[",which.max(apply(Cchin_hierout$sims.list$y1est[,,33],2,max)), ",33]")))
  tracedens_jags(Cchum_hierout, p=c(paste0("y1est[",which.max(apply(Cchum_hierout$sims.list$y1est[,,33],2,max)), ",33]")))
  tracedens_jags(Schin_hierout, p=c(paste0("y1est[",which.max(apply(Schin_hierout$sims.list$y1est[,,33],2,max)), ",33]")))
  tracedens_jags(Schum_hierout, p=c(paste0("y1est[",which.max(apply(Schum_hierout$sims.list$y1est[,,33],2,max)), ",33]")))
  
  caterpillar(Cchin_hierout, p="a1")
  caterpillar(Cchum_hierout, p="a1")
  caterpillar(Schin_hierout, p="a1")
  caterpillar(Schum_hierout, p="a1")
  
  tracethemall <- function(x) {
    numrows <- nbyname(x)$y1est[1]
    for(i in 1:numrows) {
      tracedens_jags(x, p=paste0("y1est[",i,",33]"))
    }
  }
  
  ### careful - this makes a LOOOOOOT of plots!
  par(mfrow=c(3,3))
  tracethemall(x=Cchin_hierout)
  tracethemall(x=Cchum_hierout)
  tracethemall(x=Schin_hierout)
  tracethemall(x=Schum_hierout)
  
  
  
  # # does not seem to work
  # par(mfrow=c(2,2))
  # qq_postpred(Cchin_hierout, p="y1pp", y=as.matrix(Cchin_histo_counts))
  # qq_postpred(Cchum_hierout, p="y1pp", y=Cchum_histo_counts)
  # qq_postpred(Schin_hierout, p="y1pp", y=Schin_histo_counts)
  # qq_postpred(Schum_hierout, p="y1pp", y=Schum_histo_counts)
  
  
  ## posterior predictive plots for all years overlayed
  par(mfrow=c(2,2))
  qq_postpred(ypp=Cchin_hierout$sims.list$y1pp[,,1], y=log(Cchin_histo_counts[,1]))
  for(j in 1:ncol(Cchin_histo_counts)) {
    qq_postpred(ypp=Cchin_hierout$sims.list$y1pp[,,j], y=log(Cchin_histo_counts[,j]), add=T)
  }
  qq_postpred(ypp=Cchum_hierout$sims.list$y1pp[,,1], y=log(Cchum_histo_counts[,1]))
  for(j in 1:ncol(Cchum_histo_counts)) {
    qq_postpred(ypp=Cchum_hierout$sims.list$y1pp[,,j], y=log(Cchum_histo_counts[,j]), add=T)
  }
  qq_postpred(ypp=Schin_hierout$sims.list$y1pp[,,1], y=log(Schin_histo_counts[,1]))
  for(j in 1:ncol(Schin_histo_counts)) {
    qq_postpred(ypp=Schin_hierout$sims.list$y1pp[,,j], y=log(Schin_histo_counts[,j]), add=T)
  }
  qq_postpred(ypp=Schum_hierout$sims.list$y1pp[,,1], y=log(Schum_histo_counts[,1]))
  for(j in 1:ncol(Schum_histo_counts)) {
    qq_postpred(ypp=Schum_hierout$sims.list$y1pp[,,j], y=log(Schum_histo_counts[,j]), add=T)
  }
  
  ## posterior predictive plots for 2025 
  par(mfrow=c(2,2))
  # qq_postpred(ypp=Cchin_hierout$sims.list$y1pp[,,1], y=log(Cchin_histo_counts[,1]))
  for(j in ncol(Cchin_histo_counts)) {
    qq_postpred(ypp=Cchin_hierout$sims.list$y1pp[,,j], y=log(Cchin_histo_counts[,j]))
  }
  # qq_postpred(ypp=Cchum_hierout$sims.list$y1pp[,,1], y=log(Cchum_histo_counts[,1]))
  for(j in ncol(Cchum_histo_counts)) {
    qq_postpred(ypp=Cchum_hierout$sims.list$y1pp[,,j], y=log(Cchum_histo_counts[,j]))
  }
  # qq_postpred(ypp=Schin_hierout$sims.list$y1pp[,,1], y=log(Schin_histo_counts[,1]))
  for(j in ncol(Schin_histo_counts)) {
    qq_postpred(ypp=Schin_hierout$sims.list$y1pp[,,j], y=log(Schin_histo_counts[,j]))
  }
  # qq_postpred(ypp=Schum_hierout$sims.list$y1pp[,,1], y=log(Schum_histo_counts[,1]))
  for(j in ncol(Schum_histo_counts)) {
    qq_postpred(ypp=Schum_hierout$sims.list$y1pp[,,j], y=log(Schum_histo_counts[,j]))
  }
  
  # are all these equal??
  ncol(Cchin_histo_counts)
  ncol(Cchum_histo_counts)
  ncol(Schin_histo_counts)
  ncol(Schum_histo_counts)
  
  # if so, define a variable for the index of the last year (the one we care about)
  nyr <- ncol(Cchin_histo_counts)
  
  par(mfrow=c(2,2))
  envelope(Cchin_hierout$sims.list$y1est[,,nyr])
  points(log(Cchin_histo_counts[,nyr]))
  # envelope(exp(Cchin_hierout$sims.list$y1est[,,nyr]), log="y")
  # points(Cchin_histo_counts[,nyr])
  envelope(Cchum_hierout$sims.list$y1est[,,nyr])
  points(log(Cchum_histo_counts[,nyr]))
  envelope(Schin_hierout$sims.list$y1est[,,nyr])
  points(log(Schin_histo_counts[,nyr]))
  envelope(Schum_hierout$sims.list$y1est[,,nyr])
  points(log(Schum_histo_counts[,nyr]))
  
  
  # plotting the estimated run (posterior median) for all years
  
  plotlines <- function(x, xlab="", ylab="", main="", ...) {  # x is a matrix
    plot(NA, xlim=c(1, nrow(x)), ylim=c(1, max(x, na.rm=TRUE)), 
         xlab=xlab, ylab=ylab, main=main, ...=...)
    cols <- adjustcolor(rainbow(ncol(x)), green.f=.8, red.f=.8, blue.f=.8, alpha.f=.5)
    for(j in 1:ncol(x)) {
      lines(x[,j], col=cols[j])
    }
  }
  
  par(mfrow=c(2,2))
  plotlines(exp(Cchin_hierout$q50$y1est[,apply(Cchin_histo_counts, 2, \(x) !all(is.na(x)))]),
            main="Chena King", ylab="escapement", log="y")
  lines(exp(Cchin_hierout$q50$y1est[,nyr]), lwd=2)
  points(Cchin_histo_counts[,nyr], pch=16)
  legend("topleft", lwd=2, legend=2025)
  
  plotlines(exp(Cchum_hierout$q50$y1est[,apply(Cchum_histo_counts, 2, \(x) !all(is.na(x)))]),
            main="Chena Chum", ylab="escapement", log="y")
  lines(exp(Cchum_hierout$q50$y1est[,nyr]), lwd=2)
  points((Cchum_histo_counts[,nyr]), pch=16)
  legend("topleft", lwd=2, legend=2025)
  
  plotlines(exp(Schin_hierout$q50$y1est[,apply(Schin_histo_counts, 2, \(x) !all(is.na(x)))]),
            main="Salcha King", ylab="escapement", log="y")
  lines(exp(Schin_hierout$q50$y1est[,nyr]), lwd=2)
  points((Schin_histo_counts[,nyr]), pch=16)
  legend("topleft", lwd=2, legend=2025)
  
  plotlines(exp(Schum_hierout$q50$y1est[,apply(Schum_histo_counts, 2, \(x) !all(is.na(x)))]),
            main="Salcha Chum", ylab="escapement", log="y")
  lines(exp(Schum_hierout$q50$y1est[,nyr]), lwd=2)
  points((Schum_histo_counts[,nyr]), pch=16)
  legend("topleft", lwd=2, legend=2025)
  
  
  
  
  ## defining and saving simplified matrices for each River x Species
  ## note: backtransformed to the natural scale
  
  Cchin_ham <- exp(Cchin_hierout$sims.list$y1est[,,nyr])-1
  Cchum_ham <- exp(Cchum_hierout$sims.list$y1est[,,nyr])-1
  Schin_ham <- exp(Schin_hierout$sims.list$y1est[,,nyr])-1
  Schum_ham <- exp(Schum_hierout$sims.list$y1est[,,nyr])-1
  
  save_output
  if(save_output) {
    save(Cchin_ham, Cchum_ham, Schin_ham, Schum_ham, 
         file="2025/Rdata/hier_runtiming2025.Rdata")
  } else {
    load(file="2025/Rdata/hier_runtiming2025.Rdata")   # think about this construct
  }
  
}

### taking out the most extreme values of the MCMC for each date

# saving the unaltered state of each matrix
Cchin_ham0 <- Cchin_ham
Cchum_ham0 <- Cchum_ham
Schin_ham0 <- Schin_ham
Schum_ham0 <- Schum_ham

# possible "theoretical" values for the variances
# note: these don't work if the model has not been run, but that's unimportant.
# just ignore all error messages below!
lognV <- function(mu, sig) {
  (exp(sig^2)-1)*(exp((2*mu) + (sig^2)))
}
Cchin_vtheo <- lognV(mu=Cchin_hierout$q50$theta1[,nyr],
                     sig=sqrt((Cchin_hierout$q50$sigma1[nyr]^2) +
                                (Cchin_hierout$sd$theta1[,nyr]^2)))
Cchum_vtheo <- lognV(mu=Cchum_hierout$q50$theta1[,nyr],
                     sig=sqrt((Cchum_hierout$q50$sigma1[nyr]^2) +
                                (Cchum_hierout$sd$theta1[,nyr]^2)))
Schin_vtheo <- lognV(mu=Schin_hierout$q50$theta1[,nyr],
                     sig=sqrt((Schin_hierout$q50$sigma1[nyr]^2) +
                                (Schin_hierout$sd$theta1[,nyr]^2)))
Schum_vtheo <- lognV(mu=Schum_hierout$q50$theta1[,nyr],
                     sig=sqrt((Schum_hierout$q50$sigma1[nyr]^2) +
                                (Schum_hierout$sd$theta1[,nyr]^2)))

# defining a function to censor the most extreme MCMC samples associated with
# each date.  The hope was that variance would stabilize after removing just a few
censorizor <- function(x, n=1) {
  # x[-c(apply(x, 2, which.max), apply(x, 2, which.min)),]
  
  # # taking out JUST extreme vals, assuming columns are independent
  # x1 <- apply(x, 2, \(y) y[!(rank(y, ties.method="first") %in% c(1:n, (length(y)-(0:(n-1)))))])
  
  # taking out all MCMC samples associated with extreme vals
  takethese <- unique(as.numeric(apply(x, 2, \(y) which(rank(y, ties.method="first") %in% c(1:n, (length(y)-(0:(n-1))))))))
  x1 <- x[-takethese,]
  
  if(n==0) {
    return(x)
  } else {
    return(x1)
  }
}

# trying values of n (num extreme values) to take out
# - actually only need to sum the date range of interest
rownames(Cchin_ests)[44] # aug 5 is the last date of interest

ns <- 0:50 # trial values of n
Cchin_totsd <- Cchum_totsd <- Schin_totsd <- Schum_totsd <- NA*ns
for(i in seq_along(ns)) {
  # # assuming daily estimates are independent
  # Cchin_totsd[i] <- sqrt(sum(apply(censorizor(Cchin_ham0[,1:44], n=ns[i]), 2, var)))
  # Cchum_totsd[i] <- sqrt(sum(apply(censorizor(Cchum_ham0[,1:44], n=ns[i]), 2, var)))
  # Schin_totsd[i] <- sqrt(sum(apply(censorizor(Schin_ham0[,1:44], n=ns[i]), 2, var)))
  # Schum_totsd[i] <- sqrt(sum(apply(censorizor(Schum_ham0[,1:44], n=ns[i]), 2, var)))
  
  # assuming daily estimates have covariance (only makes sense for method 2 above)
  Cchin_totsd[i] <- sd(rowSums(censorizor(Cchin_ham0[,1:44], n=ns[i])))
  Cchum_totsd[i] <- sd(rowSums(censorizor(Cchum_ham0[,1:44], n=ns[i])))
  Schin_totsd[i] <- sd(rowSums(censorizor(Schin_ham0[,1:44], n=ns[i])))
  Schum_totsd[i] <- sd(rowSums(censorizor(Schum_ham0[,1:44], n=ns[i])))
}

# plotting on the natural scale
par(mfrow=c(2,2))
plot(ns, Cchin_totsd)  
abline(h=sqrt(sum(Cchin_vtheo[1:44])), lty=3)
plot(ns, Cchum_totsd)
abline(h=sqrt(sum(Cchum_vtheo[1:44])), lty=3)
plot(ns, Schin_totsd)
abline(h=sqrt(sum(Schin_vtheo[1:44])), lty=3)
plot(ns, Schum_totsd)
abline(h=sqrt(sum(Schum_vtheo[1:44])), lty=3)

# plotting on the log scale
plot(ns, Cchin_totsd, log="y")
abline(h=sqrt(sum(Cchin_vtheo[1:44])), lty=3)
plot(ns, Cchum_totsd, log="y")
abline(h=sqrt(sum(Cchum_vtheo[1:44])), lty=3)
plot(ns, Schin_totsd, log="y")
abline(h=sqrt(sum(Schin_vtheo[1:44])), lty=3)
plot(ns, Schum_totsd, log="y")
abline(h=sqrt(sum(Schum_vtheo[1:44])), lty=3)

head(Cchin_totsd)


nn <- 1  # how many to actually censor!!!
Cchin_ham <- censorizor(x=Cchin_ham0, n=nn)
Cchum_ham <- censorizor(Cchum_ham0, n=nn)
Schin_ham <- censorizor(Schin_ham0, n=nn)
Schum_ham <- censorizor(Schum_ham0, n=nn)

par(mfrow=c(3,3))
for(j in 1:ncol(Cchin_ham)) plot(Cchin_ham[,j])
for(j in 1:ncol(Cchin_ham)) plot(Cchum_ham[,j])
for(j in 1:ncol(Cchin_ham)) plot(Schin_ham[,j])
for(j in 1:ncol(Cchin_ham)) plot(Schum_ham[,j])

# Cchin_ests2 <- Cchin_ests1
# Cchum_ests2 <- Cchum_ests1
# Schin_ests2 <- Schin_ests1
# Schum_ests2 <- Schum_ests1



## function to add point estimates & variance for Hamachan

hierinterp <- function(x, ham, hamstart=as.Date("2025-06-23")) {
  x1a <- matrix(nrow=hamstart-min(as.Date(rownames(x))), ncol=ncol(x))
  x1b <- matrix(nrow=dim(ham)[2]-nrow(x)-(hamstart-min(as.Date(rownames(x)))), ncol=ncol(x))
  colnames(x1a) <- colnames(x1b) <- colnames(x)
  if(nrow(x1a)>0) rownames(x1a) <- as.character(hamstart-(nrow(x1a):1))
  if(nrow(x1b)>0) rownames(x1b) <- as.character(max(as.Date(rownames(x)))+(1:nrow(x1b)))
  x1 <- rbind(x1a,x,x1b)
  # nr <- nrow(x)           # this only works because they start on the same date!!
  x1$hierinterp <- apply(ham, 2, median)#[1:nr]
  x1$hierinterp_var <- apply(ham, 2, var)#[1:nr]
  
  ## original version
  # x1$hierinterp[!is.na(x1$vis_count_expanded) | !is.na(x1$sonar_count_expanded)] <- NA
  # x1$hierinterp_var[!is.na(x1$vis_count_expanded) | !is.na(x1$sonar_count_expanded)] <- NA
  
  ## 2025 version
  x1$hierinterp[!is.na(x1$vis_count_expanded)] <- NA
  x1$hierinterp_var[!is.na(x1$vis_count_expanded)] <- NA
  return(x1)
}
nc <- min(nrow(Cchin_ests1), ncol(Cchin_ham))
ns <- min(nrow(Schin_ests1), ncol(Schin_ham))
Cchin_ests2 <- hierinterp(x=Cchin_ests1[1:nc,], ham=Cchin_ham[,1:nc])
Cchum_ests2 <- hierinterp(x=Cchum_ests1[1:nc,], ham=Cchum_ham[,1:nc])
Schin_ests2 <- hierinterp(x=Schin_ests1[1:ns,], ham=Schin_ham[,1:ns])
Schum_ests2 <- hierinterp(x=Schum_ests1[1:ns,], ham=Schum_ham[,1:ns])




#################################
#
#  7. Plotting everything!
#
#################################


## all estimates

plotallthethings <- function(x, ...) {
  ests <- x[,c(2,8,19,21)]#
  ses <- sqrt(as.matrix(x[,c(3,9,20,22)]))#
  lo <- ests-ses
  hi <- ests+ses
  dates <- as.Date(rownames(x))
  cols <- c(4,2,5,3)
  # plot(NA, xlim=range(dates), ylim=c(0,max(hi,na.rm=T)), ...=...)
  plot(dates, ests[,1], ylim=c(0,max(hi,na.rm=T)), col="white", ...=...)
  for(i in 1:4) { #4
    segments(x0=dates+.2*i, y0=lo[,i], y1=hi[,i], col=cols[i])
    points(x=dates+.2*i, y=ests[,i], col=cols[i], pch=16)
    lines(x=dates+.2*i, y=ests[,i], col=cols[i], pch=16)
  }
}
par(mfrow=c(2,2))
plotallthethings(x=Cchin_ests2, main="Chena Chinook")
legend("topleft", col=c(4,2,5,3), pch=16, lty=1, 
       legend=c("Visual","Sonar","Running Avg", "Hier Mod"))
plotallthethings(x=Cchum_ests2, main="Chena Chum")
plotallthethings(x=Schin_ests2, main="Salcha Chinook")
plotallthethings(x=Schum_ests2, main="Salcha Chum")


## all daily Chinook proportions

plotalltheprops <- function(x1,x2, ...) {
  ests <- x1[,c(2,8,19,21)]/(x2[,c(2,8,19,21)]+x1[,c(2,8,19,21)])
  dates <- as.Date(rownames(x1))
  cols <- c(4,2,6,3)
  # plot(NA, xlim=range(dates), ylim=c(0,max(hi,na.rm=T)), ...=...)
  plot(dates, ests[,1], ylim=c(0,1), col="white", ...=...)
  for(i in 1:4) {
    # segments(x0=dates+.2*i, y0=lo[,i], y1=hi[,i], col=cols[i])
    points(x=dates+.2*i, y=ests[,i], col=cols[i], pch=16)
    lines(x=dates+.2*i, y=ests[,i], col=cols[i], pch=16)
  }
}
par(mfrow=c(2,1))
plotalltheprops(Cchin_ests2, Cchum_ests2, main="Chena prop Chinook")
plotalltheprops(Schin_ests2, Schum_ests2, main="Salcha prop Chinook")







#################################
#
#  8. Compiling all estimates
#
#################################


## "final" estimate for each day.  Visual, then sonar, then runavg interpolated, then Hamachan.

makeests <- function(x) {
  # ## original version
  # x$DailyEst <- ifelse(!is.na(x$vis_count_expanded), x$vis_count_expanded,
  #                      ifelse(!is.na(x$sonar_count_expanded), x$sonar_count_expanded,
  #                             ifelse(!is.na(x$interp_count), x$interp_count, x$hierinterp)))
  # x$DailyVar <- ifelse(!is.na(x$vis_count_expanded), x$vis_var_expansion,
  #                      ifelse(!is.na(x$sonar_count_expanded), x$sonar_var_total,
  #                             ifelse(!is.na(x$interp_count), x$interp_count, x$hierinterp_var)))
  # x$DailyMethod <- ifelse(!is.na(x$vis_count_expanded), "Visual",
  #                         ifelse(!is.na(x$sonar_count_expanded), "Sonar",
  #                                ifelse(!is.na(x$interp_count), "Running Avg Interpolation", "Hier Run-timing Mod")))
  
  ## 2025 version - TAKING OUT SONAR
  x$DailyEst <- ifelse(!is.na(x$vis_count_expanded), x$vis_count_expanded,
                       ifelse(!is.na(x$interp_count), x$interp_count, x$hierinterp))
  x$DailyVar <- ifelse(!is.na(x$vis_count_expanded), x$vis_var_expansion,
                       ifelse(!is.na(x$interp_count), x$interp_count, x$hierinterp_var))
  x$DailyMethod <- ifelse(!is.na(x$vis_count_expanded), "Visual",
                          ifelse(!is.na(x$interp_count), "Running Avg Interpolation", "Hier Run-timing Mod"))
  nc <- ncol(x)
  x <- x[,c((nc-2):nc,1:(nc-3))]
  return(x)
}
Cchin_final <- makeests(x=Cchin_ests2)
Cchum_final <- makeests(x=Cchum_ests2)
Schin_final <- makeests(x=Schin_ests2)
Schum_final <- makeests(x=Schum_ests2)



## "final" total, according to a user-input date range.

maketot <- function(x, from, to) {
  daterange <- (as.Date(rownames(x)) >= as.Date(from)) & (as.Date(rownames(x)) <= as.Date(to))
  return(c(sum(x$DailyEst[daterange]), sqrt(sum(x$DailyVar[daterange]))))
}
from="2025-06-23"
to="2025-08-05"
tots <- rbind(maketot(x=Cchin_final, from=from, to=to),
              maketot(x=Cchum_final, from=from, to=to),
              maketot(x=Schin_final, from=from, to=to),
              maketot(x=Schum_final, from=from, to=to))
tots1 <- data.frame(River=c("Chena","Chena","Salcha","Salcha"),
                    Species=c("Chinook","Chum","Chinook","Chum"),
                    Estimate=tots[,1],
                    SE=tots[,2],
                    StartDate=from,
                    EndDate=to)

makeprops <- function(x, from, to) {
  daterange <- (as.Date(rownames(x)) >= as.Date(from)) & (as.Date(rownames(x)) <= as.Date(to))
  x1 <- x[daterange, ]
  sums <- tapply(x1$DailyEst,
                 factor(x1$DailyMethod,
                        levels=c("Visual", "Sonar", "Running Avg Interpolation", "Hier Run-timing Mod")),
                 sum, na.rm=TRUE)
  sums[is.na(sums)] <- 0
  props <- paste(round(100*sums/sum(x1$DailyEst, na.rm=TRUE),1),"%")
  names(props) <- names(sums)
  return(props)
}
allprops <- rbind(makeprops(x=Cchin_final, from=from, to=to),
                  makeprops(x=Cchum_final, from=from, to=to),
                  makeprops(x=Schin_final, from=from, to=to),
                  makeprops(x=Schum_final, from=from, to=to))
tots2 <- cbind(tots1, allprops)
tots2


# need to incorporate the additional summation variance due to covariance 
# in the hamachan model!!
inflationizer <- function(est_summary, ham, from, to) {
  daterange <- which((as.Date(rownames(est_summary)) >= as.Date(from)) & 
                       (as.Date(rownames(est_summary)) <= as.Date(to)) &
                       (est_summary$DailyMethod == "Hier Run-timing Mod"))
  daterange_orig <- which((as.Date(rownames(est_summary)) >= as.Date(from)) & 
                            (as.Date(rownames(est_summary)) <= as.Date(to)))
  
  vsum_raw <- sum(est_summary$DailyVar[daterange])  # raw sum of var for hamachan dates
  vsum_withcov <- var(rowSums(ham[,daterange]))     # total var for hamachan dates
  
  vsum_all <- sum(est_summary$DailyVar[daterange_orig]) # raw sum of var for all dates
  return(sqrt(vsum_all - vsum_raw + vsum_withcov))      # adding in var due to covariance
}
tots3 <- tots2
tots3$SE <- c(inflationizer(est_summary=Cchin_final, ham=Cchin_ham, from=from, to=to),
              inflationizer(est_summary=Cchum_final, ham=Cchum_ham, from=from, to=to),
              inflationizer(est_summary=Schin_final, ham=Schin_ham, from=from, to=to),
              inflationizer(est_summary=Schum_final, ham=Schum_ham, from=from, to=to))
tots3
## ^^ THIS METHOD WAS NOT USED, BUT IS RETAINED AS A CHECK ^^




#### Defining global covariance matrix for daily counts
#### This is the most correct way to handle total variance of summation!!

dates <- as.Date(c(paste0("2025-06-",23:30),
                   paste0("2025-07-",1:31),
                   paste0("2025-08-",1:5)))  # I don't think this is used anymore

## calculating covariance for hamachan
## NOTE: the seq_along in the indices only works because the start date is the same (06-23)
Cchin_vcov_hierinterp <- cov(Cchin_ham)#[seq_along(dates), seq_along(dates)]
Cchum_vcov_hierinterp <- cov(Cchum_ham)#[seq_along(dates), seq_along(dates)]
Schin_vcov_hierinterp <- cov(Schin_ham)#[seq_along(dates), seq_along(dates)]
Schum_vcov_hierinterp <- cov(Schum_ham)#[seq_along(dates), seq_along(dates)]

# ## calculating covariance for mixture model
# # first sum by date
# S_chinmat <- chinmat[, all_fish$river=="Salcha"]
# S_chummat <- chinmat[, all_fish$river=="Salcha"]
# # C_chinmat <- chinmat[, all_fish$river=="Chena"]
# # C_chummat <- chinmat[, all_fish$river=="Chena"]
# 
# 
# Cdates <- rownames(Cchin_final)
# Sdates <- rownames(Schin_final)
# S_chin_byday <- t(apply(S_chinmat, 1,
#                         \(x) tapply(X=x, INDEX=factor(all_fish$date, levels=Sdates), FUN=sum)))
# S_chum_byday <- t(apply(S_chummat, 1,
#                         \(x) tapply(X=x, INDEX=factor(all_fish$date, levels=Sdates), FUN=sum)))
# # C_chin_byday <- t(apply(C_chinmat, 1,
# #                         \(x) tapply(X=x, INDEX=factor(all_fish$date, levels=Cdates), FUN=sum)))
# # C_chum_byday <- t(apply(C_chummat, 1,
# #                         \(x) tapply(X=x, INDEX=factor(all_fish$date, levels=Cdates), FUN=sum)))
# 
# # then calculate correlation matrix
# Schin_cor <- cor(S_chin_byday)
# Schum_cor <- cor(S_chum_byday)
# # Cchin_cor <- cor(C_chin_byday)
# # Cchum_cor <- cor(C_chum_byday)
# 
# # then calculate vcov matrix from correlation
# # Schin_vcov_mixture <- outer(sqrt(Schin_final$DailyVar[seq_along(dates)]),
# #                             sqrt(Schin_final$DailyVar[seq_along(dates)])) * Schin_cor
# # Schum_vcov_mixture <- outer(sqrt(Schum_final$DailyVar[seq_along(dates)]),
# #                             sqrt(Schum_final$DailyVar[seq_along(dates)])) * Schum_cor
# Schin_vcov_mixture <- outer(sqrt(Schin_final$DailyVar),
#                             sqrt(Schin_final$DailyVar)) * Schin_cor
# Schum_vcov_mixture <- outer(sqrt(Schum_final$DailyVar),
#                             sqrt(Schum_final$DailyVar)) * Schum_cor
# # Cchin_vcov_mixture <- outer(sqrt(Cchin_final$DailyVar),
# #                             sqrt(Cchin_final$DailyVar)) * Cchin_cor
# # Cchum_vcov_mixture <- outer(sqrt(Cchum_final$DailyVar),
# #                             sqrt(Cchum_final$DailyVar)) * Cchum_cor

## filling in the pieces for the total covariance matrix
# make_vcov <- function(ests, vcov_hierinterp=NULL, vcov_mixture=NULL, dd = seq_along(dates)) {
#   # be careful about dd in future years if this happens
#   tots_vcov <- matrix(0, nrow=length(dd), ncol=length(dd))
#   diag(tots_vcov) <- ests$DailyVar[dd]
#   
#   tots_vcov[ests$DailyMethod[dd]=="Hier Run-timing Mod", ests$DailyMethod[dd]=="Hier Run-timing Mod"] <-
#     vcov_hierinterp[ests$DailyMethod[dd]=="Hier Run-timing Mod", ests$DailyMethod[dd]=="Hier Run-timing Mod"]
#   
#   tots_vcov[ests$DailyMethod[dd]=="Sonar", ests$DailyMethod[dd]=="Sonar"] <-
#     vcov_mixture[ests$DailyMethod[dd]=="Sonar", ests$DailyMethod[dd]=="Sonar"]
#   
#   return(tots_vcov)
# }
make_vcov <- function(ests, vcov_hierinterp=NULL, vcov_mixture=NULL) {
  # be careful about aligning all input matrices!!!!!!
  # currently they all start at 06-23, but this might not be the case in the future
  tots_vcov <- matrix(0, nrow=nrow(ests), ncol=nrow(ests))
  diag(tots_vcov) <- ests$DailyVar
  
  tots_vcov[which(ests$DailyMethod=="Hier Run-timing Mod"), which(ests$DailyMethod=="Hier Run-timing Mod")] <-
    vcov_hierinterp[which(ests$DailyMethod=="Hier Run-timing Mod"), which(ests$DailyMethod=="Hier Run-timing Mod")]
  
  tots_vcov[which(ests$DailyMethod=="Sonar"), which(ests$DailyMethod=="Sonar")] <-
    vcov_mixture[which(ests$DailyMethod=="Sonar"), which(ests$DailyMethod=="Sonar")]
  
  rownames(tots_vcov) <- colnames(tots_vcov) <-  rownames(ests)
  return(tots_vcov)
}
Cchin_vcov <- make_vcov(ests=Cchin_final, vcov_hierinterp=Cchin_vcov_hierinterp)
Cchum_vcov <- make_vcov(ests=Cchum_final, vcov_hierinterp=Cchum_vcov_hierinterp)
Schin_vcov <- make_vcov(ests=Schin_final, vcov_hierinterp=Schin_vcov_hierinterp)#, vcov_mixture=Schin_vcov_mixture)
Schum_vcov <- make_vcov(ests=Schum_final, vcov_hierinterp=Schum_vcov_hierinterp)#, vcov_mixture=Schum_vcov_mixture)

tots1$SE
tots2$SE
tots3$SE

Cwhich <- which(as.Date(rownames(Cchin_vcov)) >= as.Date(from) & as.Date(rownames(Cchin_vcov)) <= as.Date(to))
Swhich <- which(as.Date(rownames(Schin_vcov)) >= as.Date(from) & as.Date(rownames(Schin_vcov)) <= as.Date(to))
tots4 <- tots3
tots4$SE <- c(sqrt(sum(Cchin_vcov[Cwhich, Cwhich])),
              sqrt(sum(Cchum_vcov[Cwhich, Cwhich])),
              sqrt(sum(Schin_vcov[Swhich, Swhich])),
              sqrt(sum(Schum_vcov[Swhich, Swhich])))

tots4$SE
tots4

# library(xlsx)
# write.xlsx(tots1, file="ChenaSalcha2018_summary.xlsx", sheetName="SummaryTotals")
# write.xlsx(Cchin_final, file="ChenaSalcha2018_summary.xlsx", sheetName="ChenaChinook", append=T)
# write.xlsx(Cchum_final, file="ChenaSalcha2018_summary.xlsx", sheetName="ChenaChum", append=T)
# write.xlsx(Schin_final, file="ChenaSalcha2018_summary.xlsx", sheetName="SalchaChinook", append=T)
# write.xlsx(Schum_final, file="ChenaSalcha2018_summary.xlsx", sheetName="SalchaChum", append=T)

save_output
if(save_output) {
  write.csv(tots4, file="2025/output/ChenaSalcha_SummaryTotals_2025.csv")
  write.csv(Cchin_final, file="2025/output/ChenaSalcha_ChenaChinook_2025.csv")
  write.csv(Cchum_final, file="2025/output/ChenaSalcha_ChenaChum_2025.csv")
  write.csv(Schin_final, file="2025/output/ChenaSalcha_SalchaChinook_2025.csv")
  write.csv(Schum_final, file="2025/output/ChenaSalcha_SalchaChum_2025.csv")
  write.csv(Cchin_vcov, file="2025/output/ChenaSalcha_ChenaChinook_vcov_2025.csv")
  write.csv(Cchum_vcov, file="2025/output/ChenaSalcha_ChenaChum_vcov_2025.csv")
  write.csv(Schin_vcov, file="2025/output/ChenaSalcha_SalchaChinook_vcov_2025.csv")
  write.csv(Schum_vcov, file="2025/output/ChenaSalcha_SalchaChum_vcov_2025.csv")
}
