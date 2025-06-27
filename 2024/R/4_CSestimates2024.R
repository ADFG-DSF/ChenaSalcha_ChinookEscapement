load(file="2024/Rdata/CSpriors2024.Rdata")
load(file="2024/Rdata/sonardata2024.Rdata")
load(file="2024/Rdata/vis_2024.Rdata")
load(file="2024/Rdata/mixmodel2024.Rdata")


# slightly kludgy thing for post-truncation based on input length...
## in past years, 450 seemed to work best.  Not so obvious this year!
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

dates <- sort(unique(paste(2024, substr(as.character(Salcha_2024_Chin_vis$Day),6,10),sep="-")))
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
  Cnr <- nrow(Chena_2024_Chin_vis)
  Snr <- nrow(Salcha_2024_Chin_vis)
  Cchin_vis[,i,,1] <- as.matrix(Chena_2024_Chin_vis[seq(i,Cnr,by=3),3:10])
  Cchum_vis[,i,,1] <- as.matrix(Chena_2024_Chum_vis[seq(i,Cnr,by=3),3:10])
  Schin_vis[,i,,1] <- as.matrix(Salcha_2024_Chin_vis[seq(i,Snr,by=3),3:10])
  Schum_vis[,i,,1] <- as.matrix(Salcha_2024_Chum_vis[seq(i,Snr,by=3),3:10])
}

## expandorizing
Cchin_vis_expanded <- expansion_sheet(x=Cchin_vis)
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




### looking for possible discrepancies between above expansion and what occurred
### within the spreadsheet
Cchin_sheet_raw <- read.csv("2024/flat_data/ChenaKing_expansionsheet.csv")
Cchum_sheet_raw <- read.csv("2024/flat_data/ChenaChum_expansionsheet.csv")
Schin_sheet_raw <- read.csv("2024/flat_data/SalchaKing_expansionsheet.csv")
Schum_sheet_raw <- read.csv("2024/flat_data/SalchaChum_expansionsheet.csv")

datesmasher <- function(x, year=2024) { # horrible kludge that turns 1-Jul into 2024-07-01
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
  mutate(V = as.numeric(V)) %>%
  mutate(N = as.numeric(N))
Cchum_sheet <- Cchum_sheet_raw[seq(3, nrow(Cchin_sheet_raw), by=3),] %>%
  select(c("Day","N.d.Yd.avg..Hd", "var.Nd.")) %>%
  rename(N=N.d.Yd.avg..Hd, V=var.Nd., date=Day) %>%
  mutate(date = datesmasher(date)) %>%
  mutate(V = as.numeric(V)) %>%
  mutate(N = as.numeric(N))
Schin_sheet <- Schin_sheet_raw[seq(3, nrow(Cchin_sheet_raw), by=3),] %>%
  select(c("Day","N.d.Yd.avg..Hd", "var.Nd.")) %>%
  rename(N=N.d.Yd.avg..Hd, V=var.Nd., date=Day) %>%
  mutate(date = datesmasher(date)) %>%
  mutate(V = as.numeric(V)) %>%
  mutate(N = as.numeric(N))
Schum_sheet <- Schum_sheet_raw[seq(3, nrow(Cchin_sheet_raw), by=3),] %>%
  select(c("Day","N.d.Yd.avg..Hd", "var.Nd.")) %>%
  rename(N=N.d.Yd.avg..Hd, V=var.Nd., date=Day) %>%
  mutate(date = datesmasher(date)) %>%
  mutate(V = as.numeric(V)) %>%
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
checkthem(ests=Schin_ests, sheet=Schin_sheet)
checkthem(ests=Schum_ests, sheet=Schum_sheet)


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