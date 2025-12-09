#################################
#
#   5. Reading the visual count data and comparing to sonar
#
#################################


save_output <- FALSE   ## whether to write output to an external file
# save_output <- TRUE




library(tidyverse)

datesmasher <- function(x, year=2025) { # horrible kludge that turns 1-Jul into 2024-07-01
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

Chena_2025_Chin_vis <- read.csv("2025/flat_data/ChenaKing_vis20min.csv") %>%
  mutate(Day=datesmasher(Day))
Chena_2025_Chum_vis <- read.csv("2025/flat_data/ChenaChum_vis20min.csv") %>%
  mutate(Day=datesmasher(Day))

Salcha_2025_Chin_vis <- read.csv("2025/flat_data/SalchaKing_vis20min.csv") %>%
  mutate(Day=datesmasher(Day))
Salcha_2025_Chum_vis <- read.csv("2025/flat_data/SalchaChum_vis20min.csv") %>%
  mutate(Day=datesmasher(Day))

Chena_2025_Clarity_vis <- read.csv("2025/flat_data/ChenaClarity_vis20min.csv") %>%
  mutate(Day=datesmasher(Day))
Salcha_2025_Clarity_vis <- read.csv("2025/flat_data/SalchaClarity_vis20min.csv") %>%
  mutate(Day=datesmasher(Day))

# looking at the raw data files
summary(Chena_2025_Chin_vis)
summary(Chena_2025_Chum_vis)
summary(Chena_2025_Clarity_vis)
summary(Salcha_2025_Chin_vis)
summary(Salcha_2025_Chum_vis)
summary(Salcha_2025_Clarity_vis)

# this was only needed once, but easy enough to fix for everyone
# - one of the count columns was formatted as character
for(j in 3:10) {
  Chena_2025_Chin_vis[,j] <- as.numeric(Chena_2025_Chin_vis[,j])
  Chena_2025_Chum_vis[,j] <- as.numeric(Chena_2025_Chum_vis[,j])
  Chena_2025_Clarity_vis[,j] <- as.numeric(Chena_2025_Clarity_vis[,j])
  Salcha_2025_Chin_vis[,j] <- as.numeric(Salcha_2025_Chin_vis[,j])
  Salcha_2025_Chum_vis[,j] <- as.numeric(Salcha_2025_Chum_vis[,j])
  Salcha_2025_Clarity_vis[,j] <- as.numeric(Salcha_2025_Clarity_vis[,j])
}

# checking to see if counts are present where the shouldn't be
# - bad visibility or recorded no-count (clarity of -1, 4, or 5)
table(as.matrix(is.na(Chena_2025_Chin_vis[,3:10])),
      as.matrix(Chena_2025_Clarity_vis[,3:10]))
table(as.matrix(is.na(Chena_2025_Chum_vis[,3:10])),
      as.matrix(Chena_2025_Clarity_vis[,3:10]))
table(as.matrix(is.na(Salcha_2025_Chin_vis[,3:10])),
      as.matrix(Salcha_2025_Clarity_vis[,3:10]))
table(as.matrix(is.na(Salcha_2025_Chum_vis[,3:10])),
      as.matrix(Salcha_2025_Clarity_vis[,3:10]))

# what ARE the counts we would be missing if we turn these into NA
table(as.matrix(Chena_2025_Chin_vis[,3:10])[as.matrix(Chena_2025_Clarity_vis[,3:10]) %in% c(-1,4,5)], useNA = "ifany")
table(as.matrix(Chena_2025_Chum_vis[,3:10])[as.matrix(Chena_2025_Clarity_vis[,3:10]) %in% c(-1,4,5)], useNA = "ifany")
table(as.matrix(Salcha_2025_Chin_vis[,3:10])[as.matrix(Salcha_2025_Clarity_vis[,3:10]) %in% c(-1,4,5)], useNA = "ifany")
table(as.matrix(Salcha_2025_Chum_vis[,3:10])[as.matrix(Salcha_2025_Clarity_vis[,3:10]) %in% c(-1,4,5)], useNA = "ifany")



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
for(i in 1:nrow(Chena_2025_Chin_vis)) {
  # Cchin_check[i] <- sum(!is.na(Chena_2025_Chin_vis[1,3:10]) & 
  #                         !is.na(Chena_2025_Clarity_vis[1,3:10]) & 
  #                         Chena_2025_Chin_vis[1,3:10] == 
  #                         Chena_2025_Clarity_vis[1,3:10])
  Cchin_check[i] <- summer(checker(Chena_2025_Chin_vis[i, 3:10], Chena_2025_Clarity_vis[i,3:10]))
  Cchum_check[i] <- summer(checker(Chena_2025_Chum_vis[i, 3:10], Chena_2025_Clarity_vis[i,3:10]))
  Schin_check[i] <- summer(checker(Salcha_2025_Chin_vis[i, 3:10], Salcha_2025_Clarity_vis[i,3:10]))
  Schum_check[i] <- summer(checker(Salcha_2025_Chum_vis[i, 3:10], Salcha_2025_Clarity_vis[i,3:10]))
}

cbind(Cchin_check, Chena_2025_Chin_vis, 
      Chena_2025_Chin_vis[, 3:10] == Chena_2025_Clarity_vis[,3:10])[Cchin_check>0,]
cbind(Cchum_check, Chena_2025_Chum_vis, 
      Chena_2025_Chum_vis[, 3:10] == Chena_2025_Clarity_vis[,3:10])[Cchum_check>0,]
cbind(Schin_check, Salcha_2025_Chin_vis, 
      Salcha_2025_Chin_vis[, 3:10] == Salcha_2025_Clarity_vis[,3:10])[Schin_check>0,]
cbind(Schum_check, Salcha_2025_Chum_vis, 
      Salcha_2025_Chum_vis[, 3:10] == Salcha_2025_Clarity_vis[,3:10])[Schum_check>0,]
# looks like Chena King has one date (7/17) with 4 sequential blocks with the same count
# as the clarity values - investigate??


# fixing basic imputation stuff
for(j in 3:10) {
  ## data fix: counts when clarity is 4, 5 or -1 should be NA
  Chena_2025_Chin_vis[,j][Chena_2025_Clarity_vis[,j] %in% c(-1,4,5)] <- NA
  Chena_2025_Chum_vis[,j][Chena_2025_Clarity_vis[,j] %in% c(-1,4,5)] <- NA
  Salcha_2025_Chin_vis[,j][Salcha_2025_Clarity_vis[,j] %in% c(-1,4,5)] <- NA
  Salcha_2025_Chum_vis[,j][Salcha_2025_Clarity_vis[,j] %in% c(-1,4,5)] <- NA
  
  ## data fix: counts when clarity is 1-3 should be zero, not NA
  #### maybe safer to impute from other species??
  Chena_2025_Chin_vis[,j][Chena_2025_Clarity_vis[,j] %in% 1:3 &
                            is.na(Chena_2025_Chin_vis[,j])] <- 0
  Chena_2025_Chum_vis[,j][Chena_2025_Clarity_vis[,j] %in% 1:3 &
                            is.na(Chena_2025_Chum_vis[,j])] <- 0
  Salcha_2025_Chin_vis[,j][Salcha_2025_Clarity_vis[,j] %in% 1:3 &
                             is.na(Salcha_2025_Chin_vis[,j])] <- 0
  Salcha_2025_Chum_vis[,j][Salcha_2025_Clarity_vis[,j] %in% 1:3 &
                             is.na(Salcha_2025_Chum_vis[,j])] <- 0
}

# checking to make sure fixes worked
table(as.matrix(is.na(Chena_2025_Chin_vis[,3:10])),
      as.matrix(Chena_2025_Clarity_vis[,3:10]))
table(as.matrix(is.na(Chena_2025_Chum_vis[,3:10])),
      as.matrix(Chena_2025_Clarity_vis[,3:10]))
table(as.matrix(is.na(Salcha_2025_Chin_vis[,3:10])),
      as.matrix(Salcha_2025_Clarity_vis[,3:10]))
table(as.matrix(is.na(Salcha_2025_Chum_vis[,3:10])),
      as.matrix(Salcha_2025_Clarity_vis[,3:10]))




#### saving output
save_output
if(save_output) {
  save(Chena_2025_Chin_vis, Chena_2025_Chum_vis, 
       Salcha_2025_Chin_vis, Salcha_2025_Chum_vis,
       file="2025/Rdata/vis_2025.Rdata")
}



## The main functionality of the following was to compare the visual count data to the sonar.
## Before trying to do any species apportionment, the hope was that there would be a strong 
## correspondence between the total number of fish counted (both species together) and the
## total number of sonar targets.

## The intent was to do an "apples to apples" comparison (a2a), ONLY considering time periods
## in which we had both visual AND sonar on a given river.

## One concern was that resident species (grayling, suckers, etc) were being counted as sonar
## targets.  To address that, the idea was to apply a length truncation to the sonar data.
## I went through a lot of nonsense to try to identify an optimal length truncation in which 
## the a2a totals matched up well.



## whole bunch of annoying stuff to make the two datasets look similar...

reshapelong <- function(x, year=2025) {
  reshape1 <- reshape(x, direction="long", varying=list(3:10), times=1:8)
  date1 <- paste(year,substr(as.character(reshape1$Day),6,10),sep="-")
  # date1 <- paste(year,substr(as.character(reshape1$Day),1,5),sep="-")
  date_shift_hour_min20 <- paste(date1,reshape1$Shift,reshape1$time,1,sep="_")
  outdf <- data.frame(date=date1,
                      shift=reshape1[,2],
                      hour=reshape1[,3],
                      block=date_shift_hour_min20,
                      count=reshape1[,4])
  outdf1 <- outdf[order(date_shift_hour_min20),]
  return(outdf1)
}

Chena_2025_Chin_vis_long <- reshapelong(x=Chena_2025_Chin_vis)
Chena_2025_Chum_vis_long <- reshapelong(x=Chena_2025_Chum_vis)
Salcha_2025_Chin_vis_long <- reshapelong(x=Salcha_2025_Chin_vis)
Salcha_2025_Chum_vis_long <- reshapelong(x=Salcha_2025_Chum_vis)
Chena_2025_Clarity_vis_long <- reshapelong(x=Chena_2025_Clarity_vis)
Salcha_2025_Clarity_vis_long <- reshapelong(x=Salcha_2025_Clarity_vis)

# making datasets with ALL fish for checking
# - the idea being sonar totals should approximately match visual totals
#   before species apportionment
C_all_vis_long <- Chena_2025_Chin_vis_long
C_all_vis_long$count <- Chena_2025_Chin_vis_long$count + Chena_2025_Chum_vis_long$count
S_all_vis_long <- Salcha_2025_Chin_vis_long
S_all_vis_long$count <- Salcha_2025_Chin_vis_long$count + Salcha_2025_Chum_vis_long$count

C_all_vis_long$clarity <- Chena_2025_Clarity_vis_long$count
S_all_vis_long$clarity <- Salcha_2025_Clarity_vis_long$count




# investigating a possible tech effect for visual quality
# - note: techs are only identifiable by letter A-E
tech25 <- read.csv("2025/flat_data/tech25.csv") %>%
  mutate(date=datesmasher(Date)) %>%
  select(-Date) %>%
  pivot_longer(c("X1","X2","X3"), names_to="shift", values_to = "tech") %>%
  mutate(shift = as.numeric(substr(shift, 2, 2)))
head(tech25, 30)

C_all_vis_long$tech <- NA
for(i in 1:nrow(tech25)) {
  C_all_vis_long$tech[C_all_vis_long$date == tech25$date[i] &
                        C_all_vis_long$shift == tech25$shift[i]] <- tech25$tech[i]
}



## loading sonar data and tabulating totals where there are blocks of time
## with sonar AND visual (apples:apples)

load(file="2025/Rdata/sonardata2025.Rdata")


# COMMENT OUT WHEN THIS IS NOT USED
# if mixture model has been run, this will load the median species apportionment
# for each fish

# # specmed <- CSmix_jags_out$q50$species
# specmed <- CSmix_jags_out_alt$q50$species
# all_sonar$spec <- NA
# all_sonar$spec[!is.na(all_sonar$length) & all_sonar$length>=400] <- specmed


CSouth_all_sonar <- subset(all_sonar, station=="coachman")
CNorth_all_sonar <- subset(all_sonar, station=="towerside")
# SSouth_all_sonar <- subset(all_sonar, station=="Salcha South")
# SNorth_all_sonar <- subset(all_sonar, station=="Salcha North")
C_all_sonar <- subset(all_sonar, river=="Chena")
# S_all_sonar <- subset(all_sonar, river=="Salcha")
C_blocks_both <- intersect(C_all_vis_long$block[!is.na(C_all_vis_long$count)],
                           intersect(CSouth_all_sonar$block, CNorth_all_sonar$block))
# S_blocks_both <- intersect(S_all_vis_long$block[!is.na(S_all_vis_long$count)],
#                            intersect(SSouth_all_sonar$block, SNorth_all_sonar$block))

# S_blocks_both1 <- sort(unique(subset(S_all_vis_long, block %in% S_blocks_both & clarity %in% 1:2)$block))
# S_blocks_both <- S_blocks_both1

C_all_sonar_a2a <- subset(C_all_sonar, block %in% C_blocks_both)
# S_all_sonar_a2a <- subset(S_all_sonar, block %in% S_blocks_both)

C_all_vis_long_a2a <- subset(C_all_vis_long, block %in% C_blocks_both)
# S_all_vis_long_a2a <- subset(S_all_vis_long, block %in% S_blocks_both)

C_sonar_bydate_a2a <- table(C_all_sonar_a2a$date[C_all_sonar_a2a$length >= 400])
# S_sonar_bydate_a2a <- table(S_all_sonar_a2a$date[S_all_sonar_a2a$length >= 400])
C_sonar_byblock_a2a <- table(as.factor(C_all_sonar_a2a$block)[C_all_sonar_a2a$length >= 400])
# S_sonar_byblock_a2a <- table(as.factor(S_all_sonar_a2a$block)[S_all_sonar_a2a$length >= 400])
C_vis_bydate_a2a <- with(C_all_vis_long_a2a, tapply(count, as.character(date), sum))
# S_vis_bydate_a2a <- with(S_all_vis_long_a2a, tapply(count, as.character(date), sum))


# # if species was saved from the mixture model (lines uncommented previously)
# # this will subset to JUST kings.  Comment out when not used.
# 
# # C_sonar_bydate_a2a <- table(C_all_sonar_a2a$date[C_all_sonar_a2a$length >= 400])
# S_sonar_bydate_a2a <- table(S_all_sonar_a2a$date[S_all_sonar_a2a$length >= 400 & S_all_sonar_a2a$spec==1])
# # C_sonar_byblock_a2a <- table(as.factor(C_all_sonar_a2a$block)[C_all_sonar_a2a$length >= 400])
# S_sonar_byblock_a2a <- table(as.factor(S_all_sonar_a2a$block)[S_all_sonar_a2a$length >= 400 & S_all_sonar_a2a$spec==1])
# # C_vis_bydate_a2a <- with(C_all_vis_long_a2a, tapply(count, as.character(date), sum))
# S_vis_bydate_a2a <- with(S_all_vis_long_a2a, tapply(count, as.character(date), sum))



## Plotting:  First DAILY TOTALS summing only apples:apples counting blocks

par(mfrow=c(1,1))
plot(as.Date(names(C_sonar_bydate_a2a)), as.numeric(C_sonar_bydate_a2a),
     ylim=c(0,as.numeric(max(C_sonar_bydate_a2a, C_vis_bydate_a2a))),
     type='l',col=2,lwd=2,main="Chena")
lines(as.Date(names(C_vis_bydate_a2a)), C_vis_bydate_a2a, col=4,lwd=2)
legend("topleft",lwd=2,col=c(2,4),legend=c("sonar (total)","visual (total)"))
# plot(as.Date(names(S_sonar_bydate_a2a)), as.numeric(S_sonar_bydate_a2a), 
#      ylim=c(0,as.numeric(max(S_sonar_bydate_a2a, S_vis_bydate_a2a))),
#      type='l',col=2,lwd=2,main="Salcha")
# points(as.Date(names(S_sonar_bydate_a2a)), as.numeric(S_sonar_bydate_a2a), 
#        ylim=c(0,as.numeric(max(S_sonar_bydate_a2a, S_vis_bydate_a2a))),
#        type='b',col=2,lwd=2,main="Salcha")
# lines(as.Date(names(S_vis_bydate_a2a)), S_vis_bydate_a2a, col=4,lwd=2)
# points(as.Date(names(S_vis_bydate_a2a)), S_vis_bydate_a2a, col=4,lwd=2)
# legend("topleft",lwd=2,col=c(2,4),legend=c("sonar (total)","visual (total)"))
abline(h=0, lty=3)


# xx <- data.frame(sonar=as.numeric(S_sonar_bydate_a2a),
#                  vis=as.numeric(S_vis_bydate_a2a),
#                  date=as.Date(names(S_vis_bydate_a2a))) %>%
#   pivot_longer(!date,values_to="count",names_to="type")
# ggplot(xx, aes(x=date, y=count, col=type)) + 
#   geom_line()

## Plotting:  just corresponding apples:apples counting blocks.  Plot should probably be zoomed bigly.

par(mfrow=c(1,1))
plot(as.numeric(C_sonar_byblock_a2a),
     ylim=c(0,as.numeric(max(C_sonar_byblock_a2a, C_all_vis_long_a2a$count))),
     type='l',col=2,lwd=2,main="Chena")
lines(C_all_vis_long_a2a$count, col=4,lwd=2)
legend("topleft",lwd=2,col=c(2,4),legend=c("sonar (total)","visual (total)"))
# plot(as.numeric(S_sonar_byblock_a2a), 
#      ylim=c(0,as.numeric(max(S_sonar_byblock_a2a, S_all_vis_long_a2a$count))),
#      type='l',col=2,lwd=2,main="Salcha")
# lines(S_all_vis_long_a2a$count, col=4,lwd=2)
# legend("topleft",lwd=2,col=c(2,4),legend=c("sonar (total)","visual (total)"))


# checking overall totals

sum(C_all_sonar_a2a$length >= 400, na.rm=T)
sum(C_all_vis_long_a2a$count)
# sum(S_all_sonar_a2a$length >= 400, na.rm=T)
# sum(S_all_vis_long_a2a$count)
sum(C_sonar_byblock_a2a)
# sum(S_sonar_byblock_a2a)

#################
##
## Repeating most everything for the non-reviewed data!
##
############

load(file="2025/Rdata/sonardata2025_nonreviewed.Rdata")

CSouth_all_sonar_non <- subset(all_sonar_nonreviewed, station=="South")
CNorth_all_sonar_non <- subset(all_sonar_nonreviewed, station=="North")
C_all_sonar_non <- subset(all_sonar_nonreviewed, river=="Chena")
C_blocks_both_non <- intersect(C_all_vis_long$block[!is.na(C_all_vis_long$count)],
                           intersect(CSouth_all_sonar_non$block, CNorth_all_sonar_non$block))

C_all_sonar_a2a_non <- subset(C_all_sonar_non, block %in% C_blocks_both_non)

C_all_vis_long_a2a_non <- subset(C_all_vis_long, block %in% C_blocks_both_non)

C_sonar_bydate_a2a_non <- table(C_all_sonar_a2a_non$date[C_all_sonar_a2a_non$length >= 400])
C_sonar_byblock_a2a_non <- table(as.factor(C_all_sonar_a2a_non$block)[C_all_sonar_a2a_non$length >= 400])
C_vis_bydate_a2a_non <- with(C_all_vis_long_a2a_non, tapply(count, as.character(date), sum))



## Plotting:  First DAILY TOTALS summing only apples:apples counting blocks

par(mfrow=c(1,1))
plot(as.Date(names(C_sonar_bydate_a2a_non)), as.numeric(C_sonar_bydate_a2a_non),
     ylim=c(0,as.numeric(max(C_sonar_bydate_a2a_non, C_vis_bydate_a2a_non))),
     type='l',col=2,lwd=2,main="Chena - nonreviewed")
lines(as.Date(names(C_vis_bydate_a2a_non)), C_vis_bydate_a2a_non, col=4,lwd=2)
legend("topleft",lwd=2,col=c(2,4),legend=c("sonar (total)","visual (total)"))
abline(h=0, lty=3)



## Plotting:  just corresponding apples:apples counting blocks.  Plot should probably be zoomed bigly.

par(mfrow=c(1,1))
plot(as.numeric(C_sonar_byblock_a2a_non),
     ylim=c(0,as.numeric(max(C_sonar_byblock_a2a_non, C_all_vis_long_a2a_non$count))),
     type='l',col=2,lwd=2,main="Chena - nonreviewed")
lines(C_all_vis_long_a2a_non$count, col=4,lwd=2)
legend("topleft",lwd=2,col=c(2,4),legend=c("sonar (total)","visual (total)"))


# checking overall totals

sum(C_all_sonar_a2a_non$length >= 400, na.rm=T)
sum(C_all_vis_long_a2a_non$count)
# sum(S_all_sonar_a2a$length >= 400, na.rm=T)
# sum(S_all_vis_long_a2a$count)
sum(C_sonar_byblock_a2a_non)
# sum(S_sonar_byblock_a2a)






# adding sonar counts to the visual dataframe (long) for easier comparison

all(names(C_sonar_byblock_a2a) == C_all_vis_long_a2a$block)
C_all_vis_long_a2a$sonarcount <- C_sonar_byblock_a2a
C_all_vis_long_a2a$time <- (C_all_vis_long_a2a$shift-1)*8 + (C_all_vis_long_a2a$hour-1)

all(names(C_sonar_byblock_a2a_non) == C_all_vis_long_a2a_non$block)
C_all_vis_long_a2a_non$sonarcount <- C_sonar_byblock_a2a_non
C_all_vis_long_a2a_non$time <- (C_all_vis_long_a2a_non$shift-1)*8 + (C_all_vis_long_a2a_non$hour-1)

# trying a discrepancy score, kinda works
## close to 1 = good agreement between visual and sonar
## further away = less agreement
C_all_vis_long_a2a$dscore1 <- ((C_all_vis_long_a2a$count + 1) / 
                                 (C_all_vis_long_a2a$sonarcount + 1))

a2a <- C_all_vis_long_a2a # making the name shorter for less typing

boxplot(a2a$dscore1 ~ a2a$tech)
abline(h=0:1, lty=3)

boxplot(a2a$dscore1 ~ a2a$shift)
abline(h=0:1, lty=3)

boxplot(a2a$dscore1 ~ a2a$clarity)
abline(h=0:1, lty=3)

boxplot(a2a$dscore1 ~ a2a$time)
abline(h=0:1, lty=3)

boxplot(a2a$dscore1 ~ paste(a2a$tech,a2a$clarity), las=2, xlab="")
abline(h=0:1, lty=3)

# lm(a2a$dscore1 ~ a2a$tech + factor(a2a$time)) %>% summary



# trying a binomial logistic regression, under the assumption that sonar detects
# all fish, and some proportion are detected visually (shaky, i know)
## IN 2025 THIS NO LONGER WORKS

# yglm <- cbind(as.numeric(a2a$count),
#               ifelse(as.numeric(a2a$count)>as.numeric(a2a$sonarcount),
#                      as.numeric(a2a$count), as.numeric(a2a$sonarcount)))
# 
# a2a$tech2 <- ifelse(a2a$tech=="A", "A", "NotA")
# # a2a$tech3 <- ifelse(a2a$tech=="Matt", "a", 
# #                     ifelse(a2a$tech %in% c("Francine", "Mike"), "b", "c"))
# a2a$clarity2 <- ifelse(a2a$clarity == 3, 3, "1-2")
# glm(yglm ~ a2a$tech, family="binomial") %>% AIC
# glm(yglm ~ a2a$tech2, family="binomial") %>% AIC
# glm(yglm ~ a2a$tech3, family="binomial") %>% AIC
# glm(yglm ~ a2a$shift, family="binomial") %>% AIC
# glm(yglm ~ a2a$clarity, family="binomial") %>% AIC
# glm(yglm ~ a2a$clarity2, family="binomial") %>% AIC
# glm(yglm ~ factor(a2a$clarity), family="binomial") %>% AIC
# glm(yglm ~ factor(a2a$time), family="binomial") %>% AIC
# glm(yglm ~ factor(a2a$clarity) + a2a$tech, family="binomial") %>% AIC
# glm(yglm ~ factor(a2a$clarity) * a2a$tech, family="binomial") %>% AIC
# glm(yglm ~ factor(a2a$clarity2) + a2a$tech, family="binomial") %>% AIC
# glm(yglm ~ factor(a2a$clarity2) * a2a$tech, family="binomial") %>% AIC
# glm(yglm ~ factor(a2a$clarity) + a2a$tech2, family="binomial") %>% AIC
# glm(yglm ~ factor(a2a$clarity) * a2a$tech2, family="binomial") %>% AIC
# glm(yglm ~ factor(a2a$clarity2) + a2a$tech2, family="binomial") %>% AIC
# glm(yglm ~ factor(a2a$clarity2) * a2a$tech2, family="binomial") %>% AIC
# glm(yglm ~ factor(a2a$clarity) + a2a$tech3, family="binomial") %>% AIC
# glm(yglm ~ factor(a2a$clarity) * a2a$tech3, family="binomial") %>% AIC
# glm(yglm ~ factor(a2a$clarity2) + a2a$tech3, family="binomial") %>% summary
# glm(yglm ~ factor(a2a$clarity2) * a2a$tech3, family="binomial") %>% AIC

# boxplot(a2a$dscore1 ~ a2a$tech3)
# boxplot(a2a$dscore1 ~ a2a$clarity2)
# with(subset(a2a, !is.na(clarity2)), boxplot(dscore1 ~ paste(tech3, clarity2), col=c(2,3,3,4,4)))



## a whooooooooole bunch of visualizations!
##
## for the most part, these have sonar count on the x-axis and visual count on the y-axis.
## If a count is near the diagonal, that means they match.

C_all_vis_long_a2a %>%
  ggplot(aes(x=jitter(sonarcount), y=jitter(count), colour=factor(clarity))) +
  geom_point() +
  geom_abline(slope=1, intercept=0, lty=3)

C_all_vis_long_a2a %>%
  ggplot(aes(x=jitter(sonarcount), y=jitter(count), colour=factor(tech))) +
  facet_wrap(~factor(clarity)) +
  geom_point() +
  geom_abline(slope=1, intercept=0, lty=3)

## ------------

C_all_vis_long_a2a %>%
  ggplot(aes(x=jitter(sonarcount), y=jitter(count), colour=factor(clarity))) +
  facet_wrap(~factor(tech)) +
  geom_point() +
  geom_abline(slope=1, intercept=0, lty=3)

## ok, this ^^ was the only really interesting one, so it's repeated for the 
## non-reviewed sonar data VV
C_all_vis_long_a2a_non %>%
  ggplot(aes(x=jitter(sonarcount), y=jitter(count), colour=factor(clarity))) +
  facet_wrap(~factor(tech)) +
  geom_point() +
  geom_abline(slope=1, intercept=0, lty=3)

## ------------

C_all_vis_long_a2a %>%
  ggplot(aes(x=jitter(sonarcount), y=jitter(count), colour=factor(clarity))) +
  facet_wrap(~factor(time)) +
  geom_point() +
  geom_abline(slope=1, intercept=0, lty=3)

C_all_vis_long_a2a %>%
  ggplot(aes(x=jitter(sonarcount), y=jitter(count), colour=factor(clarity))) +
  facet_wrap(~factor(date)) +
  geom_point() +
  geom_abline(slope=1, intercept=0, lty=3)

C_all_vis_long_a2a %>%
  ggplot(aes(x=jitter(sonarcount), y=jitter(count), colour=factor(clarity), pch=tech)) +
  facet_wrap(~factor(date)) +
  geom_point() +
  geom_abline(slope=1, intercept=0, lty=3)

C_all_vis_long_a2a %>%
  group_by(tech) %>%
  summarise(sonarsum=sum(sonarcount), visualsum=sum(count)) %>%
  ggplot(aes(x=sonarsum, y=visualsum, colour=tech)) + 
  geom_point() +
  scale_x_continuous(limits=c(0,220)) +
  scale_y_continuous(limits=c(0,220)) +
  geom_abline(slope=1, intercept=0, lty=3)

C_all_vis_long_a2a %>%
  group_by(clarity) %>%
  summarise(sonarsum=sum(sonarcount), visualsum=sum(count)) %>%
  ggplot(aes(x=sonarsum, y=visualsum, colour=factor(clarity))) + 
  geom_point() +
  scale_x_continuous(limits=c(0,260)) +
  scale_y_continuous(limits=c(0,260)) +
  geom_abline(slope=1, intercept=0, lty=3)

C_all_vis_long_a2a %>%
  group_by(tech, clarity) %>%
  summarise(sonarsum=sum(sonarcount), visualsum=sum(count)) %>%
  ggplot(aes(x=sonarsum, y=visualsum, colour=factor(clarity))) + 
  facet_wrap(~tech) +
  geom_point() +
  scale_x_continuous(limits=c(0,80)) +
  scale_y_continuous(limits=c(0,80)) +
  geom_abline(slope=1, intercept=0, lty=3)

C_all_vis_long_a2a %>%
  group_by(time) %>%
  summarise(sonarsum=sum(sonarcount), visualsum=sum(count)) %>%
  ggplot(aes(x=sonarsum, y=visualsum, colour=factor(time))) + 
  geom_point() +
  scale_x_continuous(limits=c(0,45)) +
  scale_y_continuous(limits=c(0,45)) +
  geom_abline(slope=1, intercept=0, lty=3)

C_all_vis_long_a2a %>%
  group_by(date, shift) %>%
  summarise(sonarsum=sum(sonarcount), visualsum=sum(count), tech=tech[1], clarity=round(median(clarity))) %>%
  mutate(dscore1=(visualsum+10)/(sonarsum+10)) %>%
  mutate(shiftshift=as.Date(date)+shift/3) %>%
  ggplot(aes(x=shiftshift, y=dscore1, pch=tech, col=factor(clarity))) + 
  geom_point() #+
# scale_x_continuous(limits=c(0,45)) +
# scale_y_continuous(limits=c(0,45)) +
# geom_abline(slope=1, intercept=0, lty=3)


# are there any stories in the time sequence??
C_all_vis_long_a2a %>%
  group_by(date, clarity) %>%
  summarise(sonarsum=sum(sonarcount), visualsum=sum(count)) -> aa #%>%
# pivot_longer(cols=c("sonarsum","visualsum"), names_to="type") %>%
# ggplot(aes(x=date, y=value, colour=type)) + 
# facet_wrap(~clarity)+
# geom_line() 

## separating the apples:apples comparison by clarity score to see if there is
## more discrepancy when clarity is worse.  It didn't really matter in 2025.
par(mfrow=c(3,1))
for(i in 1:3) {
  plot(sonarsum ~ as.Date(date), data=subset(aa, clarity==i), type="l",
       main=i, lwd=2, col=2, ylim=c(0, max(aa$sonarsum)))
  points(sonarsum ~ as.Date(date), data=subset(aa, clarity==i), 
         col=2)
  lines(visualsum ~ as.Date(date), data=subset(aa, clarity==i), 
        lwd=2, col=4)
  points(visualsum ~ as.Date(date), data=subset(aa, clarity==i), 
         col=4)
}

##############
#
## A few comparisons between the reviewed and non-reviewed sonar data!
#
##############


# how many rows?  Each row is either a fish or a block with no fish
nrow(all_sonar)               # 1661
nrow(all_sonar_nonreviewed)   # 1800

# how many fish >= 400mm?  This is the pre-truncation we generally use.
sum(all_sonar$length >= 400, na.rm=TRUE)              # 1033
sum(all_sonar_nonreviewed$length >= 400, na.rm=TRUE)  # 1099



## Trying to match the pre (nonreviewed) to post (reviewed) sonar targets.
## - Which are the same in both records in terms of length AND datetime?
## - Which are present (in terms of datetime) but changed in length? 
pre <- all_sonar_nonreviewed %>%
  mutate(datetime = date+timefrac) %>%
  mutate(datenum = as.numeric(datetime)) %>%
  select(length, datetime, datenum, date, shift)
post <- all_sonar %>%
  mutate(datetime = date+timefrac) %>%
  mutate(datenum = as.numeric(datetime)) %>%
  select(length, datetime, datenum, shift, date, shift)

# adding tech
pre$tech <- NA
post$tech <- NA
for(i in 1:nrow(tech25)) {
  pre$tech[pre$date==tech25$date[i] & pre$shift==tech25$shift[i]] <- tech25$tech[i]
  post$tech[post$date==tech25$date[i] & post$shift==tech25$shift[i]] <- tech25$tech[i]
}

# "close enough" with a threshold of xx minutes
ce <- function(x1, x2, threshold=0/(24*60)) {  # default to 0
  abs(x1 - x2) <= threshold
}

pre$match <- NA
pre$change <- NA
for(i in 1:nrow(pre)) {
  if(is.na(pre$length[i])) {
    # pre$match[i] <- pre$datenum[i] %in% post$datenum
    # pre$change[i] <- !(pre$datenum[i] %in% post$datenum)
    pre$match[i] <- any(ce(pre$datenum[i], post$datenum), na.rm=TRUE)
    pre$change[i] <- !(any(ce(pre$datenum[i], post$datenum), na.rm=TRUE))
  } else {
    pre$match[i] <- any((pre$length[i]==post$length) & ce(pre$datenum[i],post$datenum), na.rm=TRUE) 
    pre$change[i] <- !any((pre$length[i]==post$length) & ce(pre$datenum[i],post$datenum), na.rm=TRUE) &
      any(ce(pre$datenum[i],post$datenum), na.rm=TRUE)
  }
}
with(pre, table(match, useNA = 'ifany'))
with(pre, table(change, useNA = 'ifany'))
with(pre, table(change, match, useNA = 'ifany'))
with(subset(pre, length>=400), table(match, useNA = 'ifany'))
with(subset(pre, length>=400), table(change, useNA = 'ifany'))
with(subset(pre, length>=400), table(change, match, useNA = 'ifany'))

post$match <- NA
post$change <- NA
for(i in 1:nrow(post)) {
  if(is.na(post$length[i])) {
    # post$match[i] <- post$datenum[i] %in% pre$datenum
    # post$change[i] <- !(post$datenum[i] %in% pre$datenum)
    post$match[i] <- any(ce(post$datenum[i], pre$datenum), na.rm=TRUE)
    post$change[i] <- !(any(ce(post$datenum[i], pre$datenum), na.rm=TRUE))
  } else {
    post$match[i] <- any((post$length[i]==pre$length) & ce(post$datenum[i],pre$datenum), na.rm=TRUE)
    post$change[i] <- !any((post$length[i]==pre$length) & ce(post$datenum[i],pre$datenum), na.rm=TRUE) &
      any(ce(post$datenum[i],pre$datenum), na.rm=TRUE)
  }
}
with(post, table(match, useNA = 'ifany'))
with(post, table(change, useNA = 'ifany'))
with(post, table(change, match, useNA = 'ifany'))
with(subset(post, length>=400), table(match, useNA = 'ifany'))
with(subset(post, length>=400), table(change, useNA = 'ifany'))
with(subset(post, length>=400), table(change, match, useNA = 'ifany'))

par(mfrow=c(1,1))
with(pre, plot(datetime, length, col=adjustcolor(ifelse(match, 1,"white"), alpha.f=.1)))
with(subset(pre, change), points(datetime, length, pch="x", col=4))
with(subset(post, change), points(datetime, length, pch="+", col=3))
legend("topright", legend=c("changed FROM", "changed TO"), pch=c("x","+"), col=4:3)

with(pre, plot(datetime, length, col=adjustcolor(ifelse(match, 1,"white"), alpha.f=.1)))
with(subset(pre, !change & !match), points(datetime, length, col=2, pch="*"))
legend("topright", legend=c("Deleted"), pch=c("*"), col=2)

techs <- sort(unique(pre$tech[pre$tech %in% LETTERS]))
# par(mfrow=c(3,2))
for(techi in techs) {
  with(pre, plot(datetime, length, col=0, main=paste("tech",techi)))
  with(subset(pre, tech==techi), points(datetime, length, col=adjustcolor(ifelse(match, 1,"white"), alpha.f=.1)))
  with(subset(pre, change & tech==techi), points(datetime, length, pch="x", col=4))
  with(subset(post, change & tech==techi), points(datetime, length, pch="+", col=3))
  legend("topright", legend=c("changed FROM", "changed TO"), pch=c("x","+"), col=4:3)
}
for(techi in techs) {
  with(pre, plot(datetime, length, col=0, main=paste("tech",techi)))
  with(subset(pre, tech==techi), points(datetime, length, col=adjustcolor(ifelse(match, 1,"white"), alpha.f=.1)))
  with(subset(pre, !change & !match & tech==techi), points(datetime, length, col=2, pch="*"))
  legend("topright", legend=c("Deleted"), pch=c("*"), col=2)
}





## Trying multiple truncations.  This was a thing in previous years but doesn't
## really add anything in 2025.

par(mfrow=c(3,3))
truncs <- seq(400,600, by=25)
for(i in 1:length(truncs)) {
  CSouth_all_sonar <- subset(all_sonar, station=="coachman")
  CNorth_all_sonar <- subset(all_sonar, station=="towerside")
  # SSouth_all_sonar <- subset(all_sonar, station=="Salcha South")
  # SNorth_all_sonar <- subset(all_sonar, station=="Salcha North")
  C_all_sonar <- subset(all_sonar, river=="Chena")
  # S_all_sonar <- subset(all_sonar, river=="Salcha")
  C_blocks_both <- intersect(C_all_vis_long$block[!is.na(C_all_vis_long$count)],
                             intersect(CSouth_all_sonar$block, CNorth_all_sonar$block))
  # S_blocks_both <- intersect(S_all_vis_long$block[!is.na(S_all_vis_long$count)],
  #                            intersect(SSouth_all_sonar$block, SNorth_all_sonar$block))
  
  # S_blocks_both1 <- sort(unique(subset(S_all_vis_long, block %in% S_blocks_both & clarity %in% 1:2)$block))
  # S_blocks_both <- S_blocks_both1
  
  C_all_sonar_a2a <- subset(C_all_sonar, block %in% C_blocks_both)
  # S_all_sonar_a2a <- subset(S_all_sonar, block %in% S_blocks_both)
  
  C_all_vis_long_a2a <- subset(C_all_vis_long, block %in% C_blocks_both)
  # S_all_vis_long_a2a <- subset(S_all_vis_long, block %in% S_blocks_both)
  
  C_sonar_bydate_a2a <- table(C_all_sonar_a2a$date[C_all_sonar_a2a$length >= truncs[i]])
  # S_sonar_bydate_a2a <- table(as.factor(S_all_sonar_a2a$date)[S_all_sonar_a2a$length >= truncs[i]])
  C_sonar_byblock_a2a <- table(as.factor(C_all_sonar_a2a$block)[C_all_sonar_a2a$length >= truncs[i]])
  # S_sonar_byblock_a2a <- table(as.factor(S_all_sonar_a2a$block)[S_all_sonar_a2a$length >= truncs[i]])
  C_vis_bydate_a2a <- with(C_all_vis_long_a2a, tapply(count, as.character(date), sum))
  # S_vis_bydate_a2a <- with(S_all_vis_long_a2a, tapply(count, as.character(date), sum))
  
  
  
  
  ## Plotting:  First DAILY TOTALS summing only apples:apples counting blocks
  
  # par(mfrow=c(1,1))
  plot(as.Date(names(C_sonar_bydate_a2a)), as.numeric(C_sonar_bydate_a2a),
       ylim=c(0,as.numeric(max(C_sonar_bydate_a2a, C_vis_bydate_a2a))),
       type='l',col=2,lwd=2,main=paste("Chena -",truncs[i]))
  lines(as.Date(names(C_vis_bydate_a2a)), C_vis_bydate_a2a, col=4,lwd=2)
  legend("topright",lwd=2,col=c(2,4),legend=c("sonar (total)","visual (total)"))
  # plot(as.Date(names(S_sonar_bydate_a2a)), as.numeric(S_sonar_bydate_a2a), 
  #      ylim=c(0,as.numeric(max(S_sonar_bydate_a2a, S_vis_bydate_a2a))),
  #      type='l',col=2,lwd=2,main=paste("Salcha -",truncs[i]))
  # points(as.Date(names(S_sonar_bydate_a2a)), as.numeric(S_sonar_bydate_a2a), 
  #        ylim=c(0,as.numeric(max(S_sonar_bydate_a2a, S_vis_bydate_a2a))),
  #        col=2,lwd=2,main=paste("Salcha -",truncs[i]))
  # lines(as.Date(names(S_vis_bydate_a2a)), S_vis_bydate_a2a, col=4,lwd=2)
  # points(as.Date(names(S_vis_bydate_a2a)), S_vis_bydate_a2a, col=4,lwd=2)
  # legend("topleft",lwd=2,col=c(2,4),legend=c("sonar (total)","visual (total)"))
  # abline(h=0, lty=3)
}


