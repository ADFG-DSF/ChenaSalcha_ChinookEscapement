# Since sonars are no longer used in the Chena & Salcha as of 2026, much of the
# established codebase for previous years will no longer be used (cool, makes life easier!)
# As such, all code will be slimmed down as possible and combined into a single script.
# 
# First, the visual counts are read, formatted as appropriate, and cleaned.
# A few imputations are performed to make sure presence of visual counts are 
# consistent with clarity values, and some summary checks are performed.
# 
# Then, the expansion and expansion variance calculations are performed in R,
# and compared to results from the expansion spreadsheet calculations - the purpose
# of this being to flag data issues as well as verify all calculations.




#################################
#
#   Reading the visual count data -- no longer comparing to sonar
#
#################################


save_output <- FALSE   ## whether to write output to an external file
# save_output <- TRUE




library(tidyverse)

datesmasher <- function(x, year=2026) { # horrible kludge that turns 1-Jul into 2026-07-01
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

## reading visual counts (full expansion sheet this time)

Chena_2026_Chin_vis <- read_csv("2026/flat_data/ChenaKing_expansionsheet.csv") %>%
  filter(!is.na(Day)) %>%
  mutate(Day=datesmasher(Day)) %>%
  as.data.frame
Chena_2026_Chum_vis <- read_csv("2026/flat_data/ChenaChum_expansionsheet.csv") %>%
  filter(!is.na(Day)) %>%
  mutate(Day=datesmasher(Day)) %>%
  as.data.frame

Salcha_2026_Chin_vis <- read_csv("2026/flat_data/SalchaKing_expansionsheet.csv") %>%
  filter(!is.na(Day)) %>%
  mutate(Day=datesmasher(Day)) %>%
  as.data.frame
Salcha_2026_Chum_vis <- read_csv("2026/flat_data/SalchaChum_expansionsheet.csv") %>%
  filter(!is.na(Day)) %>%
  mutate(Day=datesmasher(Day)) %>%
  as.data.frame

Chena_2026_Clarity_vis <- read_csv("2026/flat_data/ChenaClarity_vis20min.csv") %>%
  filter(!is.na(Day)) %>%
  mutate(Day=datesmasher(Day)) %>%
  as.data.frame
Salcha_2026_Clarity_vis <- read_csv("2026/flat_data/SalchaClarity_vis20min.csv") %>%
  filter(!is.na(Day)) %>%
  mutate(Day=datesmasher(Day)) %>%
  as.data.frame

# looking at the raw data files
summary(Chena_2026_Chin_vis)
summary(Chena_2026_Chum_vis)
summary(Chena_2026_Clarity_vis)
summary(Salcha_2026_Chin_vis)
summary(Salcha_2026_Chum_vis)
summary(Salcha_2026_Clarity_vis)

summary(Chena_2026_Chin_vis[,1:10])
summary(Chena_2026_Chum_vis[,1:10])
summary(Chena_2026_Clarity_vis[,1:10])
summary(Salcha_2026_Chin_vis[,1:10])
summary(Salcha_2026_Chum_vis[,1:10])
summary(Salcha_2026_Clarity_vis[,1:10])

# # this was only needed once, but easy enough to fix for everyone
# # - one of the count columns was formatted as character
## ---------------- NOT AN ISSUE IN 2026
# for(j in 3:10) {
#   Chena_2026_Chin_vis[,j] <- as.numeric(Chena_2026_Chin_vis[,j])
#   Chena_2026_Chum_vis[,j] <- as.numeric(Chena_2026_Chum_vis[,j])
#   Chena_2026_Clarity_vis[,j] <- as.numeric(Chena_2026_Clarity_vis[,j])
#   Salcha_2026_Chin_vis[,j] <- as.numeric(Salcha_2026_Chin_vis[,j])
#   Salcha_2026_Chum_vis[,j] <- as.numeric(Salcha_2026_Chum_vis[,j])
#   Salcha_2026_Clarity_vis[,j] <- as.numeric(Salcha_2026_Clarity_vis[,j])
# }

# checking to see if counts are present where the shouldn't be
# - bad visibility or recorded no-count (clarity of -1, 4, or 5)
table(as.matrix(is.na(Chena_2026_Chin_vis[,3:10])),
      as.matrix(Chena_2026_Clarity_vis[,3:10]))
table(as.matrix(is.na(Chena_2026_Chum_vis[,3:10])),
      as.matrix(Chena_2026_Clarity_vis[,3:10]))
table(as.matrix(is.na(Salcha_2026_Chin_vis[,3:10])),
      as.matrix(Salcha_2026_Clarity_vis[,3:10]))
table(as.matrix(is.na(Salcha_2026_Chum_vis[,3:10])),
      as.matrix(Salcha_2026_Clarity_vis[,3:10]))

# what ARE the counts we would be missing if we turn these into NA
table(as.matrix(Chena_2026_Chin_vis[,3:10])[as.matrix(Chena_2026_Clarity_vis[,3:10]) %in% c(-1,4,5)], useNA = "ifany")
table(as.matrix(Chena_2026_Chum_vis[,3:10])[as.matrix(Chena_2026_Clarity_vis[,3:10]) %in% c(-1,4,5)], useNA = "ifany")
table(as.matrix(Salcha_2026_Chin_vis[,3:10])[as.matrix(Salcha_2026_Clarity_vis[,3:10]) %in% c(-1,4,5)], useNA = "ifany")
table(as.matrix(Salcha_2026_Chum_vis[,3:10])[as.matrix(Salcha_2026_Clarity_vis[,3:10]) %in% c(-1,4,5)], useNA = "ifany")

# note: some no-counts for clarity values of 1, 2, or 3 (probably not a problem)


# investigating another data problem I randomly found..
# - clarity values entered as counts
Cchin_check <- Cchum_check <- Schin_check <- Schum_check <- NA
checker <- function(x1,x2) (!is.na(x1) & !is.na(x2)) & x1==x2
summer <- function(x) {
  runsum <- 0
  sumvec <- rep(NA, ncol(x))
  for(i in 1:ncol(x)) {
    if(x[1,i]) {
      sumvec[i] <- runsum + 1
      runsum <- runsum + 1
    } else {
      runsum <- 0
      sumvec[i] <- 0
    }
  }
  max(sumvec)
}
for(i in 1:nrow(Chena_2026_Chin_vis)) {
  # Cchin_check[i] <- sum(!is.na(Chena_2026_Chin_vis[1,3:10]) & 
  #                         !is.na(Chena_2026_Clarity_vis[1,3:10]) & 
  #                         Chena_2026_Chin_vis[1,3:10] == 
  #                         Chena_2026_Clarity_vis[1,3:10])
  Cchin_check[i] <- summer(checker(Chena_2026_Chin_vis[i, 3:10], Chena_2026_Clarity_vis[i,3:10]))
  Cchum_check[i] <- summer(checker(Chena_2026_Chum_vis[i, 3:10], Chena_2026_Clarity_vis[i,3:10]))
  Schin_check[i] <- summer(checker(Salcha_2026_Chin_vis[i, 3:10], Salcha_2026_Clarity_vis[i,3:10]))
  Schum_check[i] <- summer(checker(Salcha_2026_Chum_vis[i, 3:10], Salcha_2026_Clarity_vis[i,3:10]))
}

cbind(Cchin_check, Chena_2026_Chin_vis[, 3:10], 
      Chena_2026_Chin_vis[, 3:10] == Chena_2026_Clarity_vis[,3:10])[Cchin_check>0,]
cbind(Cchum_check, Chena_2026_Chum_vis[, 3:10], 
      Chena_2026_Chum_vis[, 3:10] == Chena_2026_Clarity_vis[,3:10])[Cchum_check>0,]
cbind(Schin_check, Salcha_2026_Chin_vis[, 3:10], 
      Salcha_2026_Chin_vis[, 3:10] == Salcha_2026_Clarity_vis[,3:10])[Schin_check>0,]
cbind(Schum_check, Salcha_2026_Chum_vis[, 3:10], 
      Salcha_2026_Chum_vis[, 3:10] == Salcha_2026_Clarity_vis[,3:10])[Schum_check>0,]

table(Cchin_check)
table(Cchum_check)
table(Schin_check)
table(Schum_check)

## ------ DOES NOT SEEM TO BE A PROBLEM IN 2026



# fixing basic imputation stuff
for(j in 3:10) {
  ## data fix: counts when clarity is 4, 5 or -1 should be NA
  Chena_2026_Chin_vis[,j][Chena_2026_Clarity_vis[,j] %in% c(-1,4,5)] <- NA
  Chena_2026_Chum_vis[,j][Chena_2026_Clarity_vis[,j] %in% c(-1,4,5)] <- NA
  Salcha_2026_Chin_vis[,j][Salcha_2026_Clarity_vis[,j] %in% c(-1,4,5)] <- NA
  Salcha_2026_Chum_vis[,j][Salcha_2026_Clarity_vis[,j] %in% c(-1,4,5)] <- NA
  
  ## data fix: counts when clarity is 1-3 should be zero, not NA
  #### maybe safer to impute from other species??
  Chena_2026_Chin_vis[,j][Chena_2026_Clarity_vis[,j] %in% 1:3 &
                            is.na(Chena_2026_Chin_vis[,j])] <- 0
  Chena_2026_Chum_vis[,j][Chena_2026_Clarity_vis[,j] %in% 1:3 &
                            is.na(Chena_2026_Chum_vis[,j])] <- 0
  Salcha_2026_Chin_vis[,j][Salcha_2026_Clarity_vis[,j] %in% 1:3 &
                             is.na(Salcha_2026_Chin_vis[,j])] <- 0
  Salcha_2026_Chum_vis[,j][Salcha_2026_Clarity_vis[,j] %in% 1:3 &
                             is.na(Salcha_2026_Chum_vis[,j])] <- 0
}

# checking to make sure fixes worked
table(as.matrix(is.na(Chena_2026_Chin_vis[,3:10])),
      as.matrix(Chena_2026_Clarity_vis[,3:10]))
table(as.matrix(is.na(Chena_2026_Chum_vis[,3:10])),
      as.matrix(Chena_2026_Clarity_vis[,3:10]))
table(as.matrix(is.na(Salcha_2026_Chin_vis[,3:10])),
      as.matrix(Salcha_2026_Clarity_vis[,3:10]))
table(as.matrix(is.na(Salcha_2026_Chum_vis[,3:10])),
      as.matrix(Salcha_2026_Clarity_vis[,3:10]))




# #### saving output
## --------- NO LONGER NEEDED IN 2026, ONLY A SINGLE SCRIPT IS USED
# save_output
# if(save_output) {
#   save(Chena_2025_Chin_vis, Chena_2025_Chum_vis, 
#        Salcha_2025_Chin_vis, Salcha_2025_Chum_vis,
#        file="2025/Rdata/vis_2025.Rdata")
# }


# save_output <- TRUE  # FALSE  # whether to write output to external files
save_output <- FALSE

# run_model <- TRUE  # whether to (re)run the interpolation model
run_model <- FALSE






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



######################################################
#
#   expanding the visual data
#
######################################################




## Re-expressing the visual counts in the same array format as the sonar totals.
## Note: visual counts were only conducted for the top (first 20-min) block of 
## each hour, so visual data is only filled in for [,,,1]

dates <- sort(unique(paste(2026, substr(as.character(Salcha_2026_Chin_vis$Day),6,10),sep="-")))
# # stations <- sort(unique(all_sonar$station))
# CN_650down <- CS_650down <- SN_650down <- SS_650down <- CN_650up <- CS_650up <- SN_650up <- SS_650up <- 
#   CN_650down <- CS_650down <- SN_650down <- SS_650down <- CN_650up <- CS_650up <- SN_650up <- SS_650up <- 
#   CN_chum <- CS_chum <- SN_chum <- SS_chum <- CN_chin <- CS_chin <- SN_chin <- SS_chin <- 
#   vCN_chum <- vCS_chum <- vSN_chum <- vSS_chum <- vCN_chin <- vCS_chin <- vSN_chin <- vSS_chin <- array(0, dim=c(length(dates),3,8,3))


Schin_vis <- Schum_vis <- Cchin_vis <- Cchum_vis <- array(NA, dim=c(length(dates),3,8,3))
for(i in 1:3) {
  Cnr <- nrow(Chena_2026_Chin_vis)
  Snr <- nrow(Salcha_2026_Chin_vis)
  Cchin_vis[,i,,1] <- as.matrix(Chena_2026_Chin_vis[seq(i,Cnr,by=3),3:10])
  Cchum_vis[,i,,1] <- as.matrix(Chena_2026_Chum_vis[seq(i,Cnr,by=3),3:10])
  Schin_vis[,i,,1] <- as.matrix(Salcha_2026_Chin_vis[seq(i,Snr,by=3),3:10])
  Schum_vis[,i,,1] <- as.matrix(Salcha_2026_Chum_vis[seq(i,Snr,by=3),3:10])
}

## expandorizing
Cchin_vis_expanded <- expansion_sheet(x=Cchin_vis)
Cchum_vis_expanded <- expansion_sheet(x=Cchum_vis)
Schin_vis_expanded <- expansion_sheet(x=Schin_vis)
Schum_vis_expanded <- expansion_sheet(x=Schum_vis)

## compiling all sources of estimates into one data.frame for each species/river
Cchin_ests <- cbind(Cchin_vis_expanded)#, Cchin_expanded, C650up_expanded[,1:3])
Cchum_ests <- cbind(Cchum_vis_expanded)#, Cchum_expanded, C650down_expanded[,1:3])
Schin_ests <- cbind(Schin_vis_expanded)#, Schin_expanded, S650up_expanded[,1:3])
Schum_ests <- cbind(Schum_vis_expanded)#, Schum_expanded, S650down_expanded[,1:3])
colnames(Cchin_ests) <- colnames(Cchum_ests) <- colnames(Schin_ests) <- colnames(Schum_ests) <- 
  c(paste("vis",colnames(Cchin_vis_expanded),sep="_"))#, paste("sonar",colnames(Cchin_expanded),sep="_"), paste("650thresh",colnames(C650up_expanded[,1:3]),sep="_"))
rownames(Cchin_ests) <- rownames(Cchum_ests) <- rownames(Schin_ests) <- rownames(Schum_ests) <- dates




#################################
#
# Verifying expansion calculations vs the expansion sheets
# 
# Typically this flags data issues rather than calculation errors, but it's
# nice when the calculations themselves match up!
#
#################################


## - NO LONGER IMPORTING VISUAL COUNTS & EXPANSION SHEETS SEPARATELY

Cchin_sheet <- Chena_2026_Chin_vis[seq(3, nrow(Chena_2026_Chin_vis), by=3),] %>%
  select(c("Day","N^d=Yd(avg)*Hd", "var(Nd)")) %>%
  rename(N=`N^d=Yd(avg)*Hd`, V=`var(Nd)`, date=Day) %>%
  # mutate(date = datesmasher(date)) %>%
  mutate(V = as.numeric(V)) %>%    ## NA coercion warning is expected (#DIV/0! in Excel)   
  mutate(N = as.numeric(N))
Cchum_sheet <- Chena_2026_Chum_vis[seq(3, nrow(Chena_2026_Chum_vis), by=3),] %>%
  select(c("Day","N^d=Yd(avg)*Hd", "var(Nd)")) %>%
  rename(N=`N^d=Yd(avg)*Hd`, V=`var(Nd)`, date=Day) %>%
  # mutate(date = datesmasher(date)) %>%
  mutate(V = as.numeric(V)) %>%    ## NA coercion warning is expected (#DIV/0! in Excel)
  mutate(N = as.numeric(N))
Schin_sheet <- Salcha_2026_Chin_vis[seq(3, nrow(Salcha_2026_Chin_vis), by=3),] %>%
  select(c("Day","N^d=Yd(avg)*Hd", "var(Nd)")) %>%
  rename(N=`N^d=Yd(avg)*Hd`, V=`var(Nd)`, date=Day) %>%
  # mutate(date = datesmasher(date)) %>%
  mutate(V = as.numeric(V)) %>%    ## NA coercion warning is expected (#DIV/0! in Excel)
  mutate(N = as.numeric(N))
Schum_sheet <- Salcha_2026_Chum_vis[seq(3, nrow(Salcha_2026_Chum_vis), by=3),] %>%
  select(c("Day","N^d=Yd(avg)*Hd", "var(Nd)")) %>%
  rename(N=`N^d=Yd(avg)*Hd`, V=`var(Nd)`, date=Day) %>%
  # mutate(date = datesmasher(date)) %>%
  mutate(V = as.numeric(V)) %>%    ## NA coercion warning is expected (#DIV/0! in Excel)
  mutate(N = as.numeric(N))


# are all date ranges consistent between sheets?? THIS IS IMPORTANT!!!!

range(Cchin_sheet$date)
range(row.names(Cchin_ests))
range(Cchum_sheet$date)
range(row.names(Cchum_ests))
range(Schin_sheet$date)
range(row.names(Schin_ests))
range(Schum_sheet$date)
range(row.names(Schum_ests))


# A quick funtion to check and summarize any discrepancies

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
#          date n_ests n_sheet v_ests v_sheet
# 10 2026-07-02     NA      12    NaN      NA   - hd==1, count removed
# 35 2026-07-27     NA      51    NaN      NA   - hd==1, count removed

checkthem(ests=Cchum_ests, sheet=Cchum_sheet)
#          date n_ests n_sheet   v_ests v_sheet
# 35 2026-07-27     NA      41      NaN      NA   - hd==1, count removed
# 46 2026-08-07    465     465 6490.286    4392   - Bug in expansion sheet!! Cell AE139 is hard-coded "0"

checkthem(ests=Schin_ests, sheet=Schin_sheet)
#          date   n_ests n_sheet   v_ests  v_sheet
# 18 2026-07-10       NA      36      NaN       NA   - hd==1, count removed
# 43 2026-08-04 24.42857      26 68.00000 58.57143   - no-count in sheet (Aug 4, shift 1, hour 5) imputed as zero - actual no-count?
# 47 2026-08-08  7.00000       6 21.25714 13.71429   - 2 hours taken out for water clarity

checkthem(ests=Schum_ests, sheet=Schum_sheet)
#          date   n_ests n_sheet   v_ests  v_sheet
# 43 2026-08-04 313.7143     336 2677.429 3185.429   - no-count in sheet (Aug 4, shift 1, hour 5) imputed as zero - actual no-count?






######################################################
#
#   Applying the running average method and Hamachan model
#
######################################################


## Applying the running-average interpolation method where applicable, then the 
## Hamachan hierarchical run-timing model.  Historical note: I'm not sure if the 
## "Hamachan" model was actually created by Hamachan (and I don't think it is.)
## He used it on the Kusko, that's how we knew about it - and we've referred to 
## it as the Hamachan model ever since

# first interpolate, then hamachan

runningavg_interp <- function(x) {
  
  # 2026 addition - there was a date with zero count but nonzero variance!
  cv_raw <- sqrt(x$vis_var_expansion)/x$vis_count_expanded
  avgcv <- mean(cv_raw[is.finite(cv_raw)], na.rm=T)
  
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


##### These datasets get updated every year
## Note: in 2026, I appended columns from 
##    2025/output/ChenaSalcha_ChenaChinook_2025 (etc) sheets 
## to 
##    2025/flat_data/ChenaKing_historic_runtiming (etc) sheets 

## IMPORTANT: all these counts start June 23
Cchin_histo <- read.csv("2026/flat_data/ChenaKing_historic_runtiming.csv", skip=4, nrows=54)
Cchum_histo <- read.csv("2026/flat_data/ChenaChum_historic_runtiming.csv", skip=3, nrows=54)
Schin_histo <- read.csv("2026/flat_data/SalchaKing_historic_runtiming.csv", skip=4, nrows=84)
Schum_histo <- read.csv("2026/flat_data/SalchaChum_historic_runtiming.csv", skip=4, nrows=84)


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

## note: historical data ends in 2025 for year 2026, this is expected
colnames(Cchin_histo_counts) <- paste0("count",1993:2025)
colnames(Cchum_histo_counts) <- paste0("count",1993:2025)
colnames(Schin_histo_counts) <- paste0("count",1993:2025)
colnames(Schum_histo_counts) <- paste0("count",1993:2025)
# colnames(Schin_histo_counts) <- paste0("count",(1993:2023)[!(1993:2023 %in% c(2018, 2020))])
# colnames(Schum_histo_counts) <- paste0("count",(1993:2023)[!(1993:2023 %in% c(2018, 2020))])




## setting up to use current year's available daily estimates in the Hamachan model

getests <- function(x) ifelse(!is.na(x$vis_count_expanded), x$vis_count_expanded, x$sonar_count_expanded)

# match these with dates
Cchin_histo_counts$count2026 <- NA
Cchum_histo_counts$count2026 <- NA
Schin_histo_counts$count2026 <- NA
Schum_histo_counts$count2026 <- NA

# IMPORTANT: the 1:nrow() bit relies on _ests1 and _histo_counts starting on Jun 23
# Cchin_histo_counts$count2025 <- getests(Cchin_ests1[1:nrow(Cchin_histo_counts),])
# Cchum_histo_counts$count2025 <- getests(Cchum_ests1[1:nrow(Cchum_histo_counts),])
# Schin_histo_counts$count2025[1:nrow(Schin_ests1)] <- getests(Schin_ests1)
# Schum_histo_counts$count2025[1:nrow(Schin_ests1)] <- getests(Schum_ests1)



###  FOR 2026:
### Since there is no sonar in 2026, ONLY USE VISUAL FOR HAMACHAN INPUT
Cchin_histo_counts$count2026 <- Cchin_ests1$vis_count_expanded[1:nrow(Cchin_histo_counts)]
Cchum_histo_counts$count2026 <- Cchum_ests1$vis_count_expanded[1:nrow(Cchum_histo_counts)]
Schin_histo_counts$count2026[1:nrow(Schin_ests1)] <- Schin_ests1$vis_count_expanded
Schum_histo_counts$count2026[1:nrow(Schin_ests1)] <- Schum_ests1$vis_count_expanded



# # The previous model was intended to run both rivers together, to account for 
# # the fact that run timing is likely related between the two.
# # I don't think it's doing so correctly, and decided to run each river
# # independently.
## NOTE: the previous model is not even included here.

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




### CONTROLS FOR HAMACHAN MODEL:
run_model <- TRUE
test_model <- FALSE

ncores <- 10
niter <- 10*1000  # < 1 min per model on desktop (10 cores)
# niter <- 100*1000  # ~ 10 min per model on desktop (10 cores)


## had a bit of an issue getting the input data formatted correctly.  This is a way to do ONE test run
## rather than an infinite loop when the issue is with the data, not the MCMC!
# chin_hieroutTEST <- runHamachan(y1=Cchin_histo_counts, y2=Schin_histo_counts, n.iter=1000, tryitonce=T, inits=haminits1) #
# chum_hieroutTEST <- runHamachan(y1=Cchum_histo_counts, y2=Schum_histo_counts, n.iter=1000, tryitonce=T, inits=haminits1) #
test_model
if(test_model) {
  Cchin_hieroutTEST <- runHamachan(y1=Cchin_histo_counts, n.iter=1000, tryitonce=T) #, inits=haminits1
  Cchum_hieroutTEST <- runHamachan(y1=Cchum_histo_counts, n.iter=1000, tryitonce=T) # , inits=haminits1
  Schin_hieroutTEST <- runHamachan(y1=Schin_histo_counts, n.iter=1000, tryitonce=T) #, inits=haminits1
  Schum_hieroutTEST <- runHamachan(y1=Schum_histo_counts, n.iter=1000, tryitonce=T) # , inits=haminits1
}
## all currently works





## Doing it for real 

run_model
if(run_model) {
  {
    Cchin_hierout <- runHamachan(y1=Cchin_histo_counts, n.iter=niter, msg="Cchin -") #, inits=haminits1
    Cchum_hierout <- runHamachan(y1=Cchum_histo_counts, n.iter=niter, msg="Cchum -") #, inits=haminits1
    Schin_hierout <- runHamachan(y1=Schin_histo_counts, n.iter=niter, msg="Schin -") #, inits=haminits1
    Schum_hierout <- runHamachan(y1=Schum_histo_counts, n.iter=niter, msg="Schum -") #, inits=haminits1
  }
  
}  else {
  load(file="2026/Rdata/hier_runtiming2026.Rdata")
}


#### the diagnostic & plotting section following won't work unless the 
#### models have been run.  Wrapped the full section in a logical.

library(jagshelper)

run_model
if(run_model) {
  par(mfrow=c(2,2))
  plotRhats(Cchin_hierout)
  plotRhats(Cchum_hierout)
  plotRhats(Schin_hierout)
  plotRhats(Schum_hierout)
  
  traceworstRhat(Cchin_hierout, parmfrow=c(3,3))
  traceworstRhat(Cchum_hierout, parmfrow=c(3,3))  
  traceworstRhat(Schin_hierout, parmfrow=c(3,3))
  traceworstRhat(Schum_hierout, parmfrow=c(3,3))
  
  tracedens_jags(Cchin_hierout, p=c("b1[34]", "mu1[34]", "sigma1[34]", "a1[34]"), parmfrow=c(2,2))
  tracedens_jags(Cchum_hierout, p=c("b1[34]", "mu1[34]", "sigma1[34]", "a1[34]"), parmfrow=c(2,2))
  tracedens_jags(Schin_hierout, p=c("b1[34]", "mu1[34]", "sigma1[34]", "a1[34]"), parmfrow=c(2,2))
  tracedens_jags(Schum_hierout, p=c("b1[34]", "mu1[34]", "sigma1[34]", "a1[34]"), parmfrow=c(2,2))
  
  tracedens_jags(Cchin_hierout, p=c(paste0("theta1[", which.max(Cchin_hierout$Rhat$theta1[,34]),",34]"),
                                    paste0("y1est[", which.max(Cchin_hierout$Rhat$y1est[,34]),",34]")))
  tracedens_jags(Cchum_hierout, p=c(paste0("theta1[", which.max(Cchum_hierout$Rhat$theta1[,34]),",34]"),
                                    paste0("y1est[", which.max(Cchum_hierout$Rhat$y1est[,34]),",34]")))
  tracedens_jags(Schin_hierout, p=c(paste0("theta1[", which.max(Schin_hierout$Rhat$theta1[,34]),",34]"),
                                    paste0("y1est[", which.max(Schin_hierout$Rhat$y1est[,34]),",34]")))
  tracedens_jags(Schum_hierout, p=c(paste0("theta1[", which.max(Schum_hierout$Rhat$theta1[,34]),",34]"),
                                    paste0("y1est[", which.max(Schum_hierout$Rhat$y1est[,34]),",34]")))
  
  tracedens_jags(Cchin_hierout, p=c(paste0("y1est[",which.max(apply(Cchin_hierout$sims.list$y1est[,,34],2,max)), ",34]")))
  tracedens_jags(Cchum_hierout, p=c(paste0("y1est[",which.max(apply(Cchum_hierout$sims.list$y1est[,,34],2,max)), ",34]")))
  tracedens_jags(Schin_hierout, p=c(paste0("y1est[",which.max(apply(Schin_hierout$sims.list$y1est[,,34],2,max)), ",34]")))
  tracedens_jags(Schum_hierout, p=c(paste0("y1est[",which.max(apply(Schum_hierout$sims.list$y1est[,,34],2,max)), ",34]")))
  
  tracedens_jags(Cchin_hierout, p=c(paste0("theta1[",which.max(apply(Cchin_hierout$sims.list$theta1[,,34],2,max)), ",34]")))
  tracedens_jags(Cchum_hierout, p=c(paste0("theta1[",which.max(apply(Cchum_hierout$sims.list$theta1[,,34],2,max)), ",34]")))
  tracedens_jags(Schin_hierout, p=c(paste0("theta1[",which.max(apply(Schin_hierout$sims.list$theta1[,,34],2,max)), ",34]")))
  tracedens_jags(Schum_hierout, p=c(paste0("theta1[",which.max(apply(Schum_hierout$sims.list$theta1[,,34],2,max)), ",34]")))
  
  caterpillar(Cchin_hierout, p="a1")
  caterpillar(Cchum_hierout, p="a1")
  caterpillar(Schin_hierout, p="a1")
  caterpillar(Schum_hierout, p="a1")
  
  caterpillar(Cchin_hierout, p="b1")
  caterpillar(Cchum_hierout, p="b1")
  caterpillar(Schin_hierout, p="b1")
  caterpillar(Schum_hierout, p="b1")
  
  caterpillar(Cchin_hierout, p="sigma1")
  caterpillar(Cchum_hierout, p="sigma1")
  caterpillar(Schin_hierout, p="sigma1")
  caterpillar(Schum_hierout, p="sigma1")
  
  # tracethemall <- function(x) {
  #   numrows <- nbyname(x)$y1est[1]
  #   for(i in 1:numrows) {
  #     tracedens_jags(x, p=paste0("y1est[",i,",33]"))
  #   }
  # }
  # 
  # ### careful - this makes a LOOOOOOT of plots!
  # par(mfrow=c(3,3))
  # tracethemall(x=Cchin_hierout)
  # tracethemall(x=Cchum_hierout)
  # tracethemall(x=Schin_hierout)
  # tracethemall(x=Schum_hierout)
  
  
  
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
  
  ## posterior predictive plots for 2026 
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
  legend("topleft", lwd=2, legend=2026)
  
  plotlines(exp(Cchum_hierout$q50$y1est[,apply(Cchum_histo_counts, 2, \(x) !all(is.na(x)))]),
            main="Chena Chum", ylab="escapement", log="y")
  lines(exp(Cchum_hierout$q50$y1est[,nyr]), lwd=2)
  points((Cchum_histo_counts[,nyr]), pch=16)
  legend("topleft", lwd=2, legend=2026)
  
  plotlines(exp(Schin_hierout$q50$y1est[,apply(Schin_histo_counts, 2, \(x) !all(is.na(x)))]),
            main="Salcha King", ylab="escapement", log="y")
  lines(exp(Schin_hierout$q50$y1est[,nyr]), lwd=2)
  points((Schin_histo_counts[,nyr]), pch=16)
  legend("topleft", lwd=2, legend=2026)
  
  plotlines(exp(Schum_hierout$q50$y1est[,apply(Schum_histo_counts, 2, \(x) !all(is.na(x)))]),
            main="Salcha Chum", ylab="escapement", log="y")
  lines(exp(Schum_hierout$q50$y1est[,nyr]), lwd=2)
  points((Schum_histo_counts[,nyr]), pch=16)
  legend("topleft", lwd=2, legend=2026)
  
  
  
  
  ## defining and saving simplified matrices for each River x Species
  ## note: backtransformed to the natural scale
  
  Cchin_ham <- exp(Cchin_hierout$sims.list$y1est[,,nyr])-1
  Cchum_ham <- exp(Cchum_hierout$sims.list$y1est[,,nyr])-1
  Schin_ham <- exp(Schin_hierout$sims.list$y1est[,,nyr])-1
  Schum_ham <- exp(Schum_hierout$sims.list$y1est[,,nyr])-1
  
  Cchin_allests <- exp(Cchin_hierout$q50$y1est)#-1
  Cchum_allests <- exp(Cchum_hierout$q50$y1est)#-1
  Schin_allests <- exp(Schin_hierout$q50$y1est)#-1
  Schum_allests <- exp(Schum_hierout$q50$y1est)#-1
  
  save_output
  if(save_output) {
    save(Cchin_ham, Cchum_ham, Schin_ham, Schum_ham, 
         Cchin_allests, Cchum_allests, Schin_allests, Schum_allests, 
         file="2026/Rdata/hier_runtiming2026.Rdata")
  } else {
    load(file="2026/Rdata/hier_runtiming2026.Rdata")   # think about this construct
  }
  
}

plotlines <- function(x, xlab="", ylab="", main="", ...) {  # x is a matrix
  plot(NA, xlim=c(1, nrow(x)), ylim=c(1, max(x, na.rm=TRUE)), 
       xlab=xlab, ylab=ylab, main=main, ...=...)
  cols <- adjustcolor(rainbow(ncol(x)), green.f=.8, red.f=.8, blue.f=.8, alpha.f=.5)
  for(j in 1:ncol(x)) {
    lines(x[,j], col=cols[j])
  }
}

lastday <- 54
rownames(Cchin_ests)[lastday]
nyr <- ncol(Cchin_allests)

par(mfrow=c(2,2))
par(mfrow=c(2,2))

plotlines(Cchin_allests,
          main="Chena King", ylab="escapement", log="y")
lines(Cchin_allests[,nyr], lwd=2)
points(Cchin_histo_counts[,nyr], pch=16)
legend("topleft", lwd=2, legend=2026)
abline(v=lastday, lty=3)

plotlines(Cchum_allests,
          main="Chena Chum", ylab="escapement", log="y")
lines(Cchum_allests[,nyr], lwd=2)
points(Cchum_histo_counts[,nyr], pch=16)
legend("topleft", lwd=2, legend=2026)
abline(v=lastday, lty=3)

plotlines(Schin_allests,
          main="Salcha King", ylab="escapement", log="y")
lines(Schin_allests[,nyr], lwd=2)
points(Schin_histo_counts[,nyr], pch=16)
legend("topleft", lwd=2, legend=2026)
abline(v=lastday, lty=3)

plotlines(Schum_allests,
          main="Salcha Chum", ylab="escapement", log="y")
lines(Schum_allests[,nyr], lwd=2)
points(Schum_histo_counts[,nyr], pch=16)
legend("topleft", lwd=2, legend=2026)
abline(v=lastday, lty=3)







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
lastday <- 54
rownames(Cchin_ests)[lastday] # aug 15 is the last date of interest (in 2025)


ns <- 0:50 # trial values of n
Cchin_totsd <- Cchum_totsd <- Schin_totsd <- Schum_totsd <- NA*ns
for(i in seq_along(ns)) {
  # # assuming daily estimates are independent
  # Cchin_totsd[i] <- sqrt(sum(apply(censorizor(Cchin_ham0[,1:lastday], n=ns[i]), 2, var)))
  # Cchum_totsd[i] <- sqrt(sum(apply(censorizor(Cchum_ham0[,1:lastday], n=ns[i]), 2, var)))
  # Schin_totsd[i] <- sqrt(sum(apply(censorizor(Schin_ham0[,1:lastday], n=ns[i]), 2, var)))
  # Schum_totsd[i] <- sqrt(sum(apply(censorizor(Schum_ham0[,1:lastday], n=ns[i]), 2, var)))
  
  # assuming daily estimates have covariance (only makes sense for method 2 above)
  Cchin_totsd[i] <- sd(rowSums(censorizor(Cchin_ham0[,1:lastday], n=ns[i])))
  Cchum_totsd[i] <- sd(rowSums(censorizor(Cchum_ham0[,1:lastday], n=ns[i])))
  Schin_totsd[i] <- sd(rowSums(censorizor(Schin_ham0[,1:lastday], n=ns[i])))
  Schum_totsd[i] <- sd(rowSums(censorizor(Schum_ham0[,1:lastday], n=ns[i])))
}

# plotting on the natural scale
par(mfrow=c(2,2))
plot(ns, Cchin_totsd)  
abline(h=sqrt(sum(Cchin_vtheo[1:lastday])), lty=3)
plot(ns, Cchum_totsd)
abline(h=sqrt(sum(Cchum_vtheo[1:lastday])), lty=3)
plot(ns, Schin_totsd)
abline(h=sqrt(sum(Schin_vtheo[1:lastday])), lty=3)
plot(ns, Schum_totsd)
abline(h=sqrt(sum(Schum_vtheo[1:lastday])), lty=3)

# plotting on the log scale
plot(ns, Cchin_totsd, log="y")
abline(h=sqrt(sum(Cchin_vtheo[1:lastday])), lty=3)
plot(ns, Cchum_totsd, log="y")
abline(h=sqrt(sum(Cchum_vtheo[1:lastday])), lty=3)
plot(ns, Schin_totsd, log="y")
abline(h=sqrt(sum(Schin_vtheo[1:lastday])), lty=3)
plot(ns, Schum_totsd, log="y")
abline(h=sqrt(sum(Schum_vtheo[1:lastday])), lty=3)

head(Cchin_totsd)

## in 2026: NO NEED TO CENSOR!
# nn <- 1  # how many to actually censor!!!
# Cchin_ham <- censorizor(x=Cchin_ham0, n=nn)
# Cchum_ham <- censorizor(Cchum_ham0, n=nn)
# Schin_ham <- censorizor(Schin_ham0, n=nn)
# Schum_ham <- censorizor(Schum_ham0, n=nn)


# ### this is a time-consuming graphical check that is only worth running
# ### if you want to look at all individual years
#
# par(mfrow=c(3,3))
# for(j in 1:ncol(Cchin_ham)) plot(Cchin_ham[,j])
# for(j in 1:ncol(Cchin_ham)) plot(Cchum_ham[,j])
# for(j in 1:ncol(Cchin_ham)) plot(Schin_ham[,j])
# for(j in 1:ncol(Cchin_ham)) plot(Schum_ham[,j])


# actual CI on the natural scale (none of that +/- 2*SE silliness)
par(mfrow=c(2,2))
caterpillar(Cchin_ham[,1:lastday])
caterpillar(Cchum_ham[,1:lastday])
caterpillar(Schin_ham[,1:lastday])
caterpillar(Schum_ham[,1:lastday])




## function to add point estimates & variance for Hamachan

hierinterp <- function(x, ham, hamstart=as.Date("2026-06-23")) {  ### HAHA remember to change the year
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


######## section needs updating in 2026 (or removing)


## all estimates

plotallthethings <- function(x, ...) {
  # ests <- x[,c(2,8,19,21)]#
  # ses <- sqrt(as.matrix(x[,c(3,9,20,22)]))#
  ests <- x[,c(2,7,9)]#
  ses <- sqrt(as.matrix(x[,c(3,8,10)]))#
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
legend("topleft", col=c(4,5,3), pch=16, lty=1, 
       legend=c("Visual","Running Avg", "Hier Mod"))
plotallthethings(x=Cchum_ests2, main="Chena Chum")
plotallthethings(x=Schin_ests2, main="Salcha Chinook")
plotallthethings(x=Schum_ests2, main="Salcha Chum")


## all daily Chinook proportions

plotalltheprops <- function(x1,x2, ...) {
  # ests <- x1[,c(2,8,19,21)]/(x2[,c(2,8,19,21)]+x1[,c(2,8,19,21)])
  ests <- x1[,c(2,7,9)]/(x2[,c(2,7,9)]+x1[,c(2,7,9)])
  dates <- as.Date(rownames(x1))
  cols <- c(4,6,3)
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
  
  ## 2026 version - TAKING OUT SONAR
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

#### INPUTS FOR DATE RANGE
Cfrom="2026-06-23"  ### note: these are defaulty and need to be added from Tracy
Cto="2026-08-15"
Sfrom="2026-06-23"
Sto="2026-08-15"


tots <- rbind(maketot(x=Cchin_final, from=Cfrom, to=Cto),
              maketot(x=Cchum_final, from=Cfrom, to=Cto),
              maketot(x=Schin_final, from=Sfrom, to=Sto),
              maketot(x=Schum_final, from=Sfrom, to=Sto))
tots1 <- data.frame(River=c("Chena","Chena","Salcha","Salcha"),
                    Species=c("Chinook","Chum","Chinook","Chum"),
                    Estimate=tots[,1],
                    SE=tots[,2],
                    StartDate=c(Cfrom, Cfrom, Sfrom, Sfrom),
                    EndDate=c(Cto, Cto, Sto, Sto))

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
allprops <- rbind(makeprops(x=Cchin_final, from=Cfrom, to=Cto),
                  makeprops(x=Cchum_final, from=Cfrom, to=Cto),
                  makeprops(x=Schin_final, from=Sfrom, to=Sto),
                  makeprops(x=Schum_final, from=Sfrom, to=Sto))
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
tots3$SE <- c(inflationizer(est_summary=Cchin_final, ham=Cchin_ham, from=Cfrom, to=Cto),
              inflationizer(est_summary=Cchum_final, ham=Cchum_ham, from=Cfrom, to=Cto),
              inflationizer(est_summary=Schin_final, ham=Schin_ham, from=Sfrom, to=Sto),
              inflationizer(est_summary=Schum_final, ham=Schum_ham, from=Sfrom, to=Sto))
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

Cwhich <- which(as.Date(rownames(Cchin_vcov)) >= as.Date(Cfrom) & 
                  as.Date(rownames(Cchin_vcov)) <= as.Date(Cto))
Swhich <- which(as.Date(rownames(Schin_vcov)) >= as.Date(Sfrom) & 
                  as.Date(rownames(Schin_vcov)) <= as.Date(Sto))
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


# plot cumulative SE wrt end date - starting at earliest date
# plot cumulative CV wrt end date - starting at earliest date

Cchin_cumSE <- NA
Cchum_cumSE <- NA
Cchin_cumN <- NA
Cchum_cumN <- NA
whichstart <- which(rownames(Cchin_vcov) == Cfrom)
whichend <- which(rownames(Cchin_vcov) == Cto)

for(i in whichstart:nrow(Cchin_vcov)) {
  Cchin_cumSE[i] <- sqrt(sum(Cchin_vcov[whichstart:i, whichstart:i]))
  Cchum_cumSE[i] <- sqrt(sum(Cchum_vcov[whichstart:i, whichstart:i]))
  Cchin_cumN[i] <- sum(Cchin_final$DailyEst[whichstart:i])
  Cchum_cumN[i] <- sum(Cchum_final$DailyEst[whichstart:i])
}

Schin_cumSE <- NA
Schum_cumSE <- NA
Schin_cumN <- NA
Schum_cumN <- NA
whichstart <- which(rownames(Schin_vcov) == Sfrom)
whichend <- which(rownames(Schin_vcov) == Sto)

for(i in whichstart:nrow(Schin_vcov)) {
  Schin_cumSE[i] <- sqrt(sum(Schin_vcov[whichstart:i, whichstart:i]))
  Schum_cumSE[i] <- sqrt(sum(Schum_vcov[whichstart:i, whichstart:i]))
  Schin_cumN[i] <- sum(Schin_final$DailyEst[whichstart:i])
  Schum_cumN[i] <- sum(Schum_final$DailyEst[whichstart:i])
}

par(mfrow=c(2,2))
plot(as.Date(rownames(Cchin_final)), Cchin_cumSE, type="b",
     xlab="", ylab="Cumulative SE", main="Chena Chinook - SE")
plot(as.Date(rownames(Cchin_final)), Cchum_cumSE, type="b",
     xlab="", ylab="Cumulative SE", main="Chena Chum - SE")
plot(as.Date(rownames(Schin_final)), Schin_cumSE, type="b",
     xlab="", ylab="Cumulative SE", main="Salcha Chinook - SE")
plot(as.Date(rownames(Schin_final)), Schum_cumSE, type="b",
     xlab="", ylab="Cumulative SE", main="Salcha Chum - SE")

par(mfrow=c(2,2))
plot(as.Date(rownames(Cchin_final)), Cchin_cumSE/Cchin_cumN,
     xlab="", ylab="Cumulative CV", main="Chena Chinook - CV",
     type="b", ylim=c(0, 0.5))
abline(h=0, lty=3)
plot(as.Date(rownames(Cchin_final)), Cchum_cumSE/Cchum_cumN,
     xlab="", ylab="Cumulative CV", main="Chena Chum - CV",
     type="b", ylim=c(0, 0.5))
abline(h=0, lty=3)
plot(as.Date(rownames(Schin_final)), Schin_cumSE/Schin_cumN,
     xlab="", ylab="Cumulative CV", main="Salcha Chinook - CV",
     type="b", ylim=c(0, 0.5))
abline(h=0, lty=3)
plot(as.Date(rownames(Schin_final)), Schum_cumSE/Schum_cumN,
     xlab="", ylab="Cumulative CV", main="Salcha Chum - CV",
     type="b", ylim=c(0, 0.5))
abline(h=0, lty=3)

par(mfrow=c(2,2))
plot(as.Date(rownames(Cchin_final)), Cchin_cumN + 2*Cchin_cumSE, lty=3, type='l',
     xlab="", ylab="Cumulative N", main="Chena Chinook - N")
lines(as.Date(rownames(Cchin_final)), Cchin_cumN - 2*Cchin_cumSE, lty=3)
points(as.Date(rownames(Cchin_final)), Cchin_cumN, lty=3)
plot(as.Date(rownames(Cchum_final)), Cchum_cumN + 2*Cchum_cumSE, lty=3, type='l',
     xlab="", ylab="Cumulative N", main="Chena Chum - N")
lines(as.Date(rownames(Cchum_final)), Cchum_cumN - 2*Cchum_cumSE, lty=3)
points(as.Date(rownames(Cchum_final)), Cchum_cumN, lty=3)
plot(as.Date(rownames(Schin_final)), Schin_cumN + 2*Schin_cumSE, lty=3, type='l',
     xlab="", ylab="Cumulative N", main="Salcha Chinook - N")
lines(as.Date(rownames(Schin_final)), Schin_cumN - 2*Schin_cumSE, lty=3)
points(as.Date(rownames(Schin_final)), Schin_cumN, lty=3)
plot(as.Date(rownames(Schum_final)), Schum_cumN + 2*Schum_cumSE, lty=3, type='l',
     xlab="", ylab="Cumulative N", main="Salcha Chum - N")
lines(as.Date(rownames(Schum_final)), Schum_cumN - 2*Schum_cumSE, lty=3)
points(as.Date(rownames(Schum_final)), Schum_cumN, lty=3)
