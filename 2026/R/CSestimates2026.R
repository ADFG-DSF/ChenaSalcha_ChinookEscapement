#################################
#
#   5. Reading the visual count data -- no longer comparing to sonar
#
#################################


save_output <- FALSE   ## whether to write output to an external file
# save_output <- TRUE




library(tidyverse)

datesmasher <- function(x, year=2026) { # horrible kludge that turns 1-Jul into 2024-07-01
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
## - NOT AN ISSUE IN 2026
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


Schin_vis <- Schum_vis <- Cchin_vis <- Cchum_vis <- array(0, dim=c(length(dates),3,8,3))
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




### looking for possible discrepancies between above expansion and what occurred    --- this needs the current import stuff
### within the spreadsheet
Cchin_sheet_raw <- read.csv("2026/flat_data/ChenaKing_expansionsheet.csv")
Cchum_sheet_raw <- read.csv("2026/flat_data/ChenaChum_expansionsheet.csv")
Schin_sheet_raw <- read.csv("2026/flat_data/SalchaKing_expansionsheet.csv")
Schum_sheet_raw <- read.csv("2026/flat_data/SalchaChum_expansionsheet.csv")

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
# no problems!

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