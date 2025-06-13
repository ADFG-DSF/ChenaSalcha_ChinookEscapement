# compare mixture mod output with visual counts
# output plots
# expand sonar stuff as needed (ugh)

# estimates!

# setwd("~/2014/Analyses/Chena and Delta Clearwater/2018 analysis")
setwd("C:/Users/mbtyers/Desktop/laptop work/ChenaSalcha2019")


load(file="df_2019.Rdata")
load(file="CSpriors2019.Rdata")
load(file="sonardata2019.Rdata")
load(file="vis_2019.Rdata")


#######################
#
# 6. Produce estimates
#
#######################


## This is a monstrous kludge, but comment or uncomment this line depending on
## which model will be used.

# df_2019 <- df_2019_alt



all_fish <- subset(all_sonar, length>=400)

# function to pull a subset of a data.frame by column names
pull_post <- function(x, p) x[,substr(names(x),1,nchar(p))==p]

# data.frame of posterior species 
specmat <- pull_post(df_2019, "species")
# specmat_alt <- pull_post(df_2019_alt, "species")

# modeled length
all_fish$modlength <- apply(pull_post(df_2019, "L.mm"), 2, median) + mean(all_fish$length)
# all_fish$modlength_alt <- apply(pull_post(df_2019_alt, "L.mm"), 2, median) + mean(all_fish$length)


# slightly kludgy thing for post-truncation based on input length...
## in past years, 450 seemed to work best.  Not so obvious this year!
trunc <- 450
trunc_subset <- (all_fish$length >= trunc)
specmat <- specmat[,trunc_subset]
# specmat_alt <- specmat_alt[,trunc_subset]
all_fish <- all_fish[trunc_subset,]


# simplifying to TRUE or FALSE for each species
chinmat <- specmat==1
chummat <- specmat==2
# chinmat_alt <- specmat_alt==1
# chummat_alt <- specmat_alt==2



## Formatting posterior totals as a set of arrays, in which each cell corresponds to a 20-min
## counting block.  Array dimensions correspond to date, 8-hr shift, hour period, and 20-min block.
## Arrays are generated for point estimates (median) and variance, for each species and each
## sonar station.

## The 650 stuff is left over from a comparison we did - totals of fish larger/smaller than a 
## 650mm threshold (or whatever we wanted the shreshold to be).

dates <- sort(unique(paste(2019, substr(as.character(Salcha_2019_Chin_vis$Day),6,10),sep="-")))
# stations <- sort(unique(all_sonar$station))
CN_650down <- CS_650down <- SN_650down <- SS_650down <- CN_650up <- CS_650up <- SN_650up <- SS_650up <- 
  CN_650down <- CS_650down <- SN_650down <- SS_650down <- CN_650up <- CS_650up <- SN_650up <- SS_650up <- 
  CN_chum <- CS_chum <- SN_chum <- SS_chum <- CN_chin <- CS_chin <- SN_chin <- SS_chin <- 
  vCN_chum <- vCS_chum <- vSN_chum <- vSS_chum <- vCN_chin <- vCS_chin <- vSN_chin <- vSS_chin <- array(0, dim=c(length(dates),3,8,3))

idate <- 1
# all_fish$length <- all_fish$modlength    ########## don't do this

lthresh <- 650#650

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
## due to species apportionment is propegated through the expansion.

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
  V2 <- (1/f1d)*rowSums((1-f2di)*(Mdi^2)*s22di/mdi, na.rm=T)
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



## Re-expressing the visual counts in the same array format as the sonar totals.
## Note: visual counts were only conducted for the top (first 20-min) block of 
## each hour, so visual data is only filled in for [,,,1]

Schin_vis <- Schum_vis <- Cchin_vis <- Cchum_vis <- NA*SN_chin
for(i in 1:3) {
  Cnr <- nrow(Chena_2019_Chin_vis)
  Snr <- nrow(Salcha_2019_Chin_vis)
  Cchin_vis[,i,,1] <- as.matrix(Chena_2019_Chin_vis[seq(i,Cnr,by=3),3:10])
  Cchum_vis[,i,,1] <- as.matrix(Chena_2019_Chum_vis[seq(i,Cnr,by=3),3:10])
  Schin_vis[,i,,1] <- as.matrix(Salcha_2019_Chin_vis[seq(i,Snr,by=3),3:10])
  Schum_vis[,i,,1] <- as.matrix(Salcha_2019_Chum_vis[seq(i,Snr,by=3),3:10])
}

## expandorizing
Cchin_vis_expanded <- expansion_sheet(Cchin_vis)
Cchum_vis_expanded <- expansion_sheet(Cchum_vis)
Schin_vis_expanded <- expansion_sheet(Schin_vis)
Schum_vis_expanded <- expansion_sheet(Schum_vis)

## compiling all sources of estimates into one data.frame for each species/river
Cchin_ests <- cbind(Cchin_vis_expanded, Cchin_expanded, C650up_expanded[,1:3])
Cchum_ests <- cbind(Cchum_vis_expanded, Cchum_expanded, C650down_expanded[,1:3])
Schin_ests <- cbind(Schin_vis_expanded, Schin_expanded, S650up_expanded[,1:3])
Schum_ests <- cbind(Schum_vis_expanded, Schum_expanded, S650down_expanded[,1:3])
colnames(Cchin_ests) <- colnames(Cchum_ests) <- colnames(Schin_ests) <- colnames(Schum_ests) <- 
  c(paste("vis",colnames(Cchin_vis_expanded),sep="_"), paste("sonar",colnames(Cchin_expanded),sep="_"), paste("650thresh",colnames(C650up_expanded[,1:3]),sep="_"))
rownames(Cchin_ests) <- rownames(Cchum_ests) <- rownames(Schin_ests) <- rownames(Schum_ests) <- dates



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



## This was a check to look at the proportions from the 650mm threshold over time,
## as well as the amount of data.  2018 had a monstrous run of chums that played havoc
## with the threshold approach output.

par(mfrow=c(2,1))
mosaicplot(date~(length>=650), data=subset(all_fish, river=="Chena"), main="Chena")
mosaicplot(date~(length>=650), data=subset(all_fish, river=="Salcha"), main="Salcha")
mosaicplot(date~(modlength>=650), data=subset(all_fish, river=="Chena"), main="Chena")
mosaicplot(date~(modlength>=650), data=subset(all_fish, river=="Salcha"), main="Salcha")



## Applying the running-average interpolation method where applicable, then the 
## Hamachan hierarchical run-timing model.  Historical note: I'm not sure if the 
## "Hamachan" model was actually created by Hamachan (and I don't think it is.)
## He uses it on the Kusko, that's how we knew about it - and we've referred to 
## it as the Hamachan model ever since

# first interpolate, then hamachan

runningavg_interp <- function(x) {
  avgcv <- mean(sqrt(x$vis_var_expansion)/x$vis_count_expanded, na.rm=T)
  est <- ifelse(!is.na(x$vis_count_expanded), x$vis_count_expanded, x$sonar_count_expanded)
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

# setwd("C:/Users/mbtyers/Documents/2014/Analyses/Chena and Delta Clearwater/2018 analysis/")


## These datasets get updated every year

## IMPORTANT: all these counts start June 23
library(readxl)
Cchin_histo <- as.data.frame(read_xlsx("2018 materials/historicdata_for_hierruntiming_18.xlsx", "Chena Historic Chinook", "A6:BA90", col_names=T))
Cchum_histo <- as.data.frame(read_xlsx("2018 materials/historicdata_for_hierruntiming_18.xlsx", "Chena Historic Chum", "A5:BA89", col_names=T))
Schin_histo <- as.data.frame(read_xlsx("2018 materials/historicdata_for_hierruntiming_18.xlsx", "Salcha Historic Chinook", "A5:BZ89", col_names=T))
Schum_histo <- as.data.frame(read_xlsx("2018 materials/historicdata_for_hierruntiming_18.xlsx", "Salcha Historic Chum", "A5:BZ89", col_names=T))


## fixorizing inputs
Schin_histo <- Schin_histo[,colSums(!is.na(Schin_histo))>0]
Schum_histo <- Schum_histo[,colSums(!is.na(Schum_histo))>0]

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
for(i in 1:length(notallowed)) {
  Cchin_histo_counts[Cchin_histo_counttype == notallowed[i]] <- NA
  Cchum_histo_counts[Cchum_histo_counttype == notallowed[i]] <- NA
  Schin_histo_counts[Schin_histo_counttype == notallowed[i]] <- NA
  Schum_histo_counts[Schum_histo_counttype == notallowed[i]] <- NA
}

colnames(Cchin_histo_counts) <- paste0("count",1993:2018)
colnames(Cchum_histo_counts) <- paste0("count",1993:2018)
colnames(Schin_histo_counts) <- paste0("count",1993:2017)
colnames(Schum_histo_counts) <- paste0("count",1993:2017)



## setting up to use current year's available daily estimates in the Hamachan model

getests <- function(x) ifelse(!is.na(x$vis_count_expanded), x$vis_count_expanded, x$sonar_count_expanded)

# match these with dates
Cchin_histo_counts$count2019 <- NA
Cchum_histo_counts$count2019 <- NA

Schin_histo_counts$count2018 <- NA
Schum_histo_counts$count2018 <- NA
Schin_histo_counts$count2019 <- NA
Schum_histo_counts$count2019 <- NA

Cchin_histo_counts$count2019[1:nrow(Cchin_ests1)] <- getests(Cchin_ests1)
Cchum_histo_counts$count2019[1:nrow(Cchum_ests1)] <- getests(Cchum_ests1)
Schin_histo_counts$count2019[1:nrow(Schin_ests1)] <- getests(Schin_ests1)
Schum_histo_counts$count2019[1:nrow(Schin_ests1)] <- getests(Schum_ests1)

cat('model {
    
  for(j in 1:nyrs) {
    for(i in 1:ndays){
    y1[i,j] ~ dnorm(theta1[i,j], tausq1[j])
      #    y1[i,j] ~ dpois(theta1[i,j])    
      # Assume that run timing distribution takes log normal distribution 
      theta1[i,j] <- a1[j]*exp(-0.5*pow(log(x[i]/mu1[j])/b1[j],2))
      # Assume that run timing distribution takes Extreme value distribution 
        # theta1[i,j] <- a1[j]*exp(-exp(-(x[i]-mu1[j])/b1[j])-(x[i]-mu1[j])/b1[j]+1)
      # Assume that run timing distribution takes log-logistic distribution 
        # theta1[i,j] <- (a1[j]*(b1[j]/mu1[j])*pow((x[i]/mu1[j]),b1[j]-1))/pow(1+pow((x[i]/mu1[j]),b1[j]),2)
      
      y2[i,j] ~ dnorm(theta2[i,j], tausq2[j])
      #    y2[i,j] ~ dpois(theta2[i,j])    
      # Assume that run timing distribution takes log normal distribution 
      theta2[i,j] <- a2[j]*exp(-0.5*pow(log(x[i]/mu2[j])/b2[j],2))
      # Assume that run timing distribution takes Extreme value distribution 
      #   theta2[i,j] <- a2[j]*exp(-exp(-(x[i]-mu2[j])/b2[j])-(x[i]-mu2[j])/b2[j]+1)
      # Assume that run timing distribution takes log-logistic distribution 
      #   theta2[i,j] <- (a2[j]*(b2[j]/mu2[j])*pow((x[i]/mu2[j]),b2[j]-1))/pow(1+pow((x[i]/mu2[j]),b2[j]),2)   
    }
  }
    # a[] indicates the maximum height (amplitude) of the function a>0
    # mu[] indicates the function peaks when x = mu mu>0 : Peak timing
    # b[] indicates peak width of the function b>0 standard deviation
    
    # Priors
  for(i in 1:nyrs) {
    # Normal distribution Positive only 
    #  a: is independent not hierarchical 
    a1[i] ~ dnorm(0,0.00001)T(0,)
    b1[i] ~ dnorm(b01,b01.prec)T(0.16,)
    # b1[i] <- b2[i]
    # mu1[i] ~ dnorm(mu01,mu01.prec)T(0,)
    # mu1[i] ~ dnorm(mu2[i],prec.mu)
    mu1[i] <- mu2[i] + eps
    #eps[i] ~ dnorm(mueps,prec.mueps)
    
    a2[i] ~ dnorm(0,0.00001)T(0,)
    b2[i] ~ dnorm(b02,b02.prec)T(0.16,)
    mu2[i] ~ dnorm(mu02,mu02.prec)T(0,)   
  }  
  eps ~ dnorm(0,0.01)
    #   prec.mueps <- pow(sig.mueps,-2)
    #   sig.mueps ~ dunif(0,4)
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
    
  b02 ~ dnorm(0.5,0.001)T(0.16,)
  mu02 ~ dnorm(25,0.001)T(0,)
  b02.prec <-1/b02.ssq 
  b02.ssq <- b02.sigma*b02.sigma
  b02.sigma ~ dunif(0,100)  
  mu02.prec <-1/mu02.ssq 
  mu02.ssq <- mu02.sigma*mu02.sigma
  mu02.sigma ~ dunif(0,100)  
    
    ## This assumes that variance of each year is independent.     
  for(i in 1:nyrs) {    
    tausq1[i] <- pow(sigma1[i],-2)
    sigma1[i] ~ dunif(0,100) 
    
    tausq2[i] <- pow(sigma2[i],-2)
    sigma2[i] ~ dunif(0,100) 
  }
    
    # Backestimate escapement 
  for(j in 1:nyrs){
    for(i in 1:ndays){ 
      y1est[i,j] <- y1[i,j]
      y2est[i,j] <- y2[i,j]
    }
  }
    # missing <- sum(y1est[1:20,12])
    
}', file="hiermod1.jags")

runHamachan <- function(y1,y2,n.iter=5000,msg="",tryitonce=F,...) {
  y1[y1<0]<-0
  y2[y2<0]<-0
  hiermod1.jags.data <- list(y1=log(y1+1),
                             y2=log(y2+1),
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
      hiermod1.jags.out <- jagsUI::jags(model.file="hiermod1.jags",
                                                             data=hiermod1.jags.data, 
                                                             parameters.to.save=c("y1est","y2est","eps","b1","b2","mu2","sigma1","sigma2","a1","a2"), 
                                                             n.chains=ncores, parallel=T, n.iter=n.iter, n.burnin=n.iter/2, n.adapt=n.iter/10, n.thin=n.iter/1000,
                                                             ...=...)
      didit <- T
    }
    else hiermod1.jags.out <- tryCatch(jagsUI::jags(model.file="hiermod1.jags",
                                               data=hiermod1.jags.data, 
                                               parameters.to.save=c("y1est","y2est","eps","b1","b2","mu2","sigma1","sigma2","a1","a2"), 
                                               n.chains=ncores, parallel=T, n.iter=n.iter, n.burnin=n.iter/2, n.adapt=n.iter/10, n.thin=n.iter/1000,
                                               ...=...),
                                  error=function(x) NA)
    if(length(hiermod1.jags.out)>1) didit<-T
    tries <- tries+1
    time <- Sys.time()-t.start
    print(time)
  }
  return(hiermod1.jags.out)
}



## The Hamachan model can be tempermental.  It very often errors out with 
## "invalid parent values" in mid run.  The runHamachan function above provides
## a wrapper for running the thing, and keeps trying until it succeeds.



## This is kludgy.  In 2018 it took forever to get the model to run without erroring, so 
## when it finally did, I harvested some of the posteror values to use as inits in case
## I had to run it again.  I saved them to an Rdata file, but here they are as hard-coded.
## Note that there are 26 values associated with each parameter, and 27 are needed - this
## will need to be updated in future years if this is done again.

# load(file="2018 materials/forinits.Rdata")
mns <- list(eps=-3.07521,
            b1=c(0.4993585,0.4874376,0.7624875,0.9734876,0.458732,0.5421083,0.3211041,0.3740339,0.1929754,0.3908579,0.4122211,0.6864661,0.3759515,0.4443365,0.6278043,0.6584322,0.4933222,0.3012637,0.4994659,0.4840655,0.3097202,0.3172026,0.544756,0.479573,0.809854,0.3268056),
            b2=c(0.5594515,0.4664597,0.6634599,1.082397,0.5895152,0.5607642,0.3615209,0.358652,0.2936067,0.4280528,0.335822,0.5988643,0.4539034,0.3820309,0.3282549,0.446211,0.4376694,0.3169996,0.4750557,0.546952,0.3782434,0.488768,0.5489337,0.4331722,0.5893685,0.4820904),
            mu2=c(35.56875,37.19195,41.08907,27.05414,39.08196,43.53637,46.77643,41.9007,35.49535,41.63567,32.75191,41.18205,44.93722,39.49298,43.7797,48.54853,41.94573,42.10001,44.21258,47.85188,40.09372,38.20914,39.13038,29.63719,39.12832,45.70052),
            sigma1=c(0.8410581,1.264184,0.7762708,0.835336,0.9054144,1.357112,0.7919183,0.5859112,0.6912784,0.2445944,0.8591711,0.7059833,1.340392,0.4699086,1.00802,1.290016,0.8248737,0.6178315,50.92243,0.7304966,1.097464,0.7134912,1.225191,0.4489991,1.13808,0.5346976),
            sigma2=c(0.888907,0.842229,0.6036151,0.3796727,0.9886522,0.6087939,1.781811,0.6405888,1.012119,0.7453564,0.5691745,0.7107642,0.7832727,0.6425774,0.7383351,0.8215943,1.199256,0.7138716,0.2709813,0.4223435,0.9294938,50.15124,0.7383826,1.037929,0.9035162,0.6739997),
            a1=c(5.678893,6.044671,5.454089,6.6836,6.516682,5.65599,6.91382,5.254823,6.601376,6.042483,4.67915,6.95134,7.479791,7.83172,5.62572,4.954238,6.795865,6.500094,251.0988,6.714561,7.791792,7.194905,6.140953,6.247392,6.94229,6.948424),
            a2=c(5.601007,7.825206,7.197368,8.656482,7.682589,6.999347,6.786969,7.355442,7.302795,7.20858,11.44176,7.645368,9.355948,9.153506,7.464748,7.274597,7.793973,7.524785,8.475906,7.653075,8.592395,252.2626,6.458762,5.435573,7.444079,8.06277))
sds <- list(eps=0.6064488,
            b1=c(0.03866983,0.05414206,0.1101233,0.2230508,0.04498995,0.08051076,0.02874224,0.02671431,0.01669528,0.08299616,0.1113003,0.03757654,0.07164957,0.02906789,0.06328428,0.09191997,0.04247819,0.01936726,0.2175975,0.02859066,0.02608488,0.02872582,0.06360887,0.09261977,0.1054313,0.02396206),
            b2=c(0.04406355,0.02940756,0.04173194,0.2389003,0.04811137,0.03958778,0.03664379,0.02415809,0.02918349,0.07694738,0.0413929,0.01919289,0.01752968,0.01351291,0.03374409,0.03677125,0.03907535,0.01450575,0.03457909,0.0331126,0.02070704,0.1902916,0.07356101,0.07883926,0.07228649,0.02609204),
            mu2=c(1.23545,0.9162987,1.62253,2.214778,1.830528,2.145404,1.478285,0.796168,0.8225886,4.291614,4.227743,0.5955652,0.692352,0.4171637,2.279478,1.397698,1.591942,0.5588188,1.132288,1.067957,0.7360707,1.227842,1.973257,4.206209,3.072517,1.576285),
            sigma1=c(0.1074857,0.1677873,0.1550886,0.1841203,0.1130642,0.1972234,0.1077742,0.07409995,0.1081477,0.0722943,0.1455415,0.09164432,0.2895339,0.07996181,0.1318207,0.197919,0.1073783,0.08084683,28.96905,0.111376,0.1724392,0.1212499,0.1515441,0.09321352,0.1888806,0.07542445),
            sigma2=c(0.1147809,0.1076489,0.07199613,0.1104888,0.118231,0.07495933,0.2034865,0.08360964,0.1903357,0.172397,0.09825959,0.06060466,0.07586093,0.08290737,0.1009956,0.1252546,0.1560355,0.08326016,0.06001122,0.05435268,0.1184784,28.97692,0.1264625,0.1770651,0.1179773,0.0822787),
            a1=c(0.2125329,0.3143807,0.2596297,0.2396886,0.2503697,0.3794018,0.2603522,0.1553136,0.2669334,0.565404,0.4807141,0.1953144,0.5105327,0.1313666,0.3082988,0.6275289,0.2175612,0.1813053,194.9607,0.2629037,0.3407902,0.2268901,0.2987485,0.5399707,0.3201379,0.2338063),
            a2=c(0.2086162,0.2075684,0.1281544,0.1332426,0.2559887,0.1800073,0.4950331,0.171989,0.7686177,0.7071601,4.143269,0.1584075,0.1747124,0.1650843,0.3554756,0.7650475,0.2988157,0.1939959,0.08604632,0.09677732,0.2370959,188.1118,0.222736,1.311115,0.2914468,0.277075))

haminits1 <- function() list(eps=rnorm(1,mns$eps,sds$eps),
                             b1=rnorm(27,c(mns$b1,median(mns$b1)),c(sds$b1,median(sds$b1))),
                             b2=rnorm(27,c(mns$b2,median(mns$b2)),c(sds$b2,median(sds$b2))),
                             mu2=rnorm(27,c(mns$mu2,median(mns$mu2)),c(sds$mu2,median(sds$mu2))),
                             sigma1=rnorm(27,c(mns$sigma1,median(mns$sigma1)),c(sds$sigma1,median(sds$sigma1))),
                             sigma2=rnorm(27,c(mns$sigma2,median(mns$sigma2)),c(sds$sigma2,median(sds$sigma2))),
                             a1=rnorm(27,c(mns$a1,median(mns$a1)),c(sds$a1,median(sds$a1))),
                             a2=rnorm(27,c(mns$a2,median(mns$a2)),c(sds$a2,median(sds$a2))))
# save(mns,sds,haminits,file="forinits.Rdata")


ncores <- 3


## had a bit of an issue getting the input data formatted correctly.  This is a way to do ONE test run
## rather than an infinite loop when the issue is with the data, not the MCMC!
chin_hieroutTEST <- runHamachan(y1=Cchin_histo_counts, y2=Schin_histo_counts, n.iter=1000, tryitonce=T, inits=haminits1) #
chum_hieroutTEST <- runHamachan(y1=Cchum_histo_counts, y2=Schum_histo_counts, n.iter=1000, tryitonce=T, inits=haminits1) # 



## Doing it for real - 50k takes 15 minutes if it succeeds, 100k in 30

chin_hierout <- runHamachan(y1=Cchin_histo_counts, y2=Schin_histo_counts, n.iter=200000, msg="firstmod -", inits=haminits1) #
chum_hierout <- runHamachan(y1=Cchum_histo_counts, y2=Schum_histo_counts, n.iter=200000, msg="secondmod -", inits=haminits1) #
# chin_hierout_alt <- runHamachan(y1=Cchin_histo_counts, y2=Schin_histo_counts, n.iter=200000, msg="firstmod -", inits=haminits1) #
# chum_hierout_alt <- runHamachan(y1=Cchum_histo_counts, y2=Schum_histo_counts, n.iter=200000, msg="secondmod -", inits=haminits1) # 

save(chum_hierout, chin_hierout, file="hamachanout19.Rdata")
# save(chum_hierout_alt, chin_hierout_alt, file="hamachanout19_alt.Rdata")

# save(chum_hierout, chin_hierout, chum_hierout_alt, chin_hierout_alt, file="hamachanout19.Rdata")

# load(file="hamachanout19.Rdata")

# load(file="hamachanout19_alt.Rdata")
## chin_hierout <- chin_hierout_alt  # careful with this!!
## chum_hierout <- chum_hierout_alt




## Playing with diagnostics

# par(mfrow=c(2,2))
# hist(unlist(chin_hierout$Rhat))
# hist(unlist(chin_hierout$n.eff))     # n.eff gets swamped with cases where y1est or y2est are fixed
# hist(unlist(chum_hierout$Rhat))
# hist(unlist(chum_hierout$n.eff))

par(mfrow=c(2,2))
hist(chin_hierout$n.eff$y1est[is.na(Cchin_histo_counts)])
hist(chin_hierout$n.eff$y2est[is.na(Schin_histo_counts)])
hist(chum_hierout$n.eff$y1est[is.na(Cchum_histo_counts)])
hist(chum_hierout$n.eff$y2est[is.na(Schum_histo_counts)])

sapply(chin_hierout$Rhat, function(x) sum(x>1.1, na.rm=T))
sapply(chin_hierout$n.eff, function(x) mean(x<500, na.rm=T))
sum(chin_hierout$n.eff$y1est[is.na(Cchin_histo_counts)]<500)
sum(chin_hierout$n.eff$y2est[is.na(Schin_histo_counts)]<500)

sapply(chum_hierout$Rhat, function(x) sum(x>1.1, na.rm=T))
sapply(chum_hierout$n.eff, function(x) mean(x<500, na.rm=T))
sum(chum_hierout$n.eff$y1est[is.na(Cchum_histo_counts)]<500)
sum(chum_hierout$n.eff$y2est[is.na(Schum_histo_counts)]<500)

weirdtrace2 <- function(x, nline=NULL, n=NULL, lwd=1, main="") {             
  if(is.null(nline)) nline <- length(x)/n
  if(is.null(n)) n <- length(x)/nline
  cols <- adjustcolor(rainbow(nline),red.f=.9,blue.f=.9,green.f=.9,alpha.f=.6)
  plot(NA,xlim=c(0,n),ylim=range(x,na.rm=T),main=main)
  # lwd <- c(1,1,1,1,1,1,1,1,1,2,2,2)
  for(i in 1:nline) {
    lines(1:n, x[(n*(i-1)+1):(n*i)], col=cols[i],lwd=lwd)
  }
}
trace_df <- function(df,nline=ncores,...) {
  for(i in 1:ncol(df)) {
    weirdtrace2(df[,i],main=names(df)[i],nline=nline,...=...)
  }
}

par(mfrow=c(4,2))
nyrs <- ncol(Cchin_histo_counts)
trace_df(chin_hierout$sims.list$y1est[,is.na(Cchin_histo_counts[,nyrs]),nyrs])
par(mfrow=c(4,2))
trace_df(chum_hierout$sims.list$y1est[,is.na(Cchum_histo_counts[,nyrs]),nyrs])
par(mfrow=c(4,2))
trace_df(chin_hierout$sims.list$y2est[,is.na(Schin_histo_counts[,nyrs]),nyrs])
par(mfrow=c(4,2))
trace_df(chum_hierout$sims.list$y2est[,is.na(Schum_histo_counts[,nyrs]),nyrs])




## backtransform, extract median & sd, shoot Hamachan results back to Cchin_ests1, etc
nyrs <- ncol(Cchin_histo_counts)
Cchin_ham <- exp(chin_hierout$sims.list$y1est[,,nyrs])-1
Cchum_ham <- exp(chum_hierout$sims.list$y1est[,,nyrs])-1
Schin_ham <- exp(chin_hierout$sims.list$y2est[,,nyrs])-1
Schum_ham <- exp(chum_hierout$sims.list$y2est[,,nyrs])-1

Cchin_ests2 <- Cchin_ests1
Cchum_ests2 <- Cchum_ests1
Schin_ests2 <- Schin_ests1
Schum_ests2 <- Schum_ests1



## function to add point estimates & variance for Hamachan

hierinterp <- function(x, ham, hamstart=as.Date("2019-06-23")) {
  x1a <- matrix(nrow=hamstart-min(as.Date(rownames(x))), ncol=ncol(x))
  x1b <- matrix(nrow=dim(ham)[2]-nrow(x)-(hamstart-min(as.Date(rownames(x)))), ncol=ncol(x))
  colnames(x1a) <- colnames(x1b) <- colnames(x)
  if(nrow(x1a)>0) rownames(x1a) <- as.character(hamstart-(nrow(x1a):1))
  if(nrow(x1b)>0) rownames(x1b) <- as.character(max(as.Date(rownames(x)))+(1:nrow(x1b)))
  x1 <- rbind(x1a,x,x1b)
  # nr <- nrow(x)           # this only works because they start on the same date!!
  x1$hierinterp <- apply(ham, 2, median)#[1:nr]
  x1$hierinterp_var <- apply(ham, 2, var)#[1:nr]
  x1$hierinterp[!is.na(x1$vis_count_expanded) | !is.na(x1$sonar_count_expanded)] <- NA
  x1$hierinterp_var[!is.na(x1$vis_count_expanded) | !is.na(x1$sonar_count_expanded)] <- NA
  return(x1)
}
Cchin_ests2 <- hierinterp(x=Cchin_ests1, ham=Cchin_ham)
Cchum_ests2 <- hierinterp(x=Cchum_ests1, ham=Cchum_ham)
Schin_ests2 <- hierinterp(x=Schin_ests1, ham=Schin_ham)
Schum_ests2 <- hierinterp(x=Schum_ests1, ham=Schum_ham)




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
  cols <- c(4,2,6,3)
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

# ...figure out if we're truncating at 400 or 450 for 2018 analysis
# ... perhaps investigate whether to NOT mixmodel by sex



## This plot could possibly be moved to following the mixture model.  This looks at the
## modeled probability that each sonar target is a Chinook, with time on the x-axis
## and length on the y-axis.

modspec <- 2-colMeans(specmat)
library(MTfuncs)
dotsplot.rgb(all_fish$date[all_fish$river=="Chena"]+.3*runif(sum(all_fish$river=="Chena"),-1,1),
             all_fish$modlength[all_fish$river=="Chena"],
             modspec[all_fish$river=="Chena"],
             main="Chena",legend.title="prob(Chinook)",ylim=c(400,1200))
dotsplot.rgb(all_fish$date[all_fish$river=="Salcha"]+.3*runif(sum(all_fish$river=="Salcha"),-1,1),
             all_fish$modlength[all_fish$river=="Salcha"],
             modspec[all_fish$river=="Salcha"],
             main="Salcha",legend.title="prob(Chinook)",ylim=c(400,1200))



#################################
#
#  8. Compiling all estimates
#
#################################


## "final" estimate for each day.  Visual, then sonar, then runavg interpolated, then Hamachan.

makeests <- function(x) {
  x$DailyEst <- ifelse(!is.na(x$vis_count_expanded), x$vis_count_expanded,
                       ifelse(!is.na(x$sonar_count_expanded), x$sonar_count_expanded,
                              ifelse(!is.na(x$interp_count), x$interp_count, x$hierinterp)))
  x$DailyVar <- ifelse(!is.na(x$vis_count_expanded), x$vis_var_expansion,
                       ifelse(!is.na(x$sonar_count_expanded), x$sonar_var_total,
                              ifelse(!is.na(x$interp_count), x$interp_count, x$hierinterp_var)))
  x$DailyMethod <- ifelse(!is.na(x$vis_count_expanded), "Visual",
                          ifelse(!is.na(x$sonar_count_expanded), "Sonar",
                                 ifelse(!is.na(x$interp_count), "Running Avg Interpolation", "Hier Run-timing Mod")))
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
from="2019-06-23"
to="2019-08-10"
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

# library(xlsx)
# write.xlsx(tots1, file="ChenaSalcha2018_summary.xlsx", sheetName="SummaryTotals")
# write.xlsx(Cchin_final, file="ChenaSalcha2018_summary.xlsx", sheetName="ChenaChinook", append=T)
# write.xlsx(Cchum_final, file="ChenaSalcha2018_summary.xlsx", sheetName="ChenaChum", append=T)
# write.xlsx(Schin_final, file="ChenaSalcha2018_summary.xlsx", sheetName="SalchaChinook", append=T)
# write.xlsx(Schum_final, file="ChenaSalcha2018_summary.xlsx", sheetName="SalchaChum", append=T)


write.csv(tots1, file="ChenaSalcha_SummaryTotals_2019.csv")
write.csv(Cchin_final, file="ChenaSalcha_ChenaChinook_2019.csv")
write.csv(Cchum_final, file="ChenaSalcha_ChenaChum_2019.csv")
write.csv(Schin_final, file="ChenaSalcha_SalchaChinook_2019.csv")
write.csv(Schum_final, file="ChenaSalcha_SalchaChum_2019.csv")

# write.csv(tots1, file="ChenaSalcha_SummaryTotals_2019_alt.csv")
# write.csv(Cchin_final, file="ChenaSalcha_ChenaChinook_2019_alt.csv")
# write.csv(Cchum_final, file="ChenaSalcha_ChenaChum_2019_alt.csv")
# write.csv(Schin_final, file="ChenaSalcha_SalchaChinook_2019_alt.csv")
# write.csv(Schum_final, file="ChenaSalcha_SalchaChum_2019_alt.csv")

save(tots1,Cchin_final,Cchum_final,Schin_final,Schum_final, file="finalests.Rdata")
