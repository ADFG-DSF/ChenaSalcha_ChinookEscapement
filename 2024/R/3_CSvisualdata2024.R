#################################
#
#   5. Reading the visual count data and comparing to sonar
#
#################################




# library(readxl)
# Chena_2024_Chin_vis <- as.data.frame(read_xlsx("2024/addl_data/Chena Tower 2024.xlsx", sheet="King-20 min", range="A3:J126"))
# Chena_2024_Chum_vis <- as.data.frame(read_xlsx("2024/addl_data/Chena Tower 2024.xlsx", sheet="Chum 20 min",  range="A3:J126"))
# Salcha_2024_Chin_vis <- as.data.frame(read_xlsx("2024/addl_data/Salcha Tower 2024.xlsx", sheet="King-20 min",  range="A3:J126"))
# Salcha_2024_Chum_vis <- as.data.frame(read_xlsx("2024/addl_data/Salcha Tower 2024.xlsx", sheet="Chum 20 min",  range="A3:J126"))


library(tidyverse)

datesmasher <- function(x, year=2024) { # horrible kludge that turns 1-Jul into 07-01
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

Chena_2024_Chin_vis <- read.csv("2024/flat_data/ChenaKing_vis20min.csv") %>%
  mutate(Day=datesmasher(Day))
Chena_2024_Chum_vis <- read.csv("2024/flat_data/ChenaChum_vis20min.csv") %>%
  mutate(Day=datesmasher(Day))

Salcha_2024_Chin_vis <- read.csv("2024/flat_data/SalchaKing_vis20min.csv") %>%
  mutate(Day=datesmasher(Day))
Salcha_2024_Chum_vis <- read.csv("2024/flat_data/SalchaChum_vis20min.csv") %>%
  mutate(Day=datesmasher(Day))

Chena_2024_Clarity_vis <- read.csv("2024/flat_data/ChenaClarity_vis20min.csv") %>%
  mutate(Day=datesmasher(Day))
Salcha_2024_Clarity_vis <- read.csv("2024/flat_data/SalchaClarity_vis20min.csv") %>%
  mutate(Day=datesmasher(Day))

summary(Chena_2024_Chin_vis)
summary(Chena_2024_Chum_vis)
summary(Chena_2024_Clarity_vis)
summary(Salcha_2024_Chin_vis)
summary(Salcha_2024_Chum_vis)
summary(Salcha_2024_Clarity_vis)

# this was only needed once, but easy enough to fix for everyone
for(j in 3:10) {
  Chena_2024_Chin_vis[,j] <- as.numeric(Chena_2024_Chin_vis[,j])
  Chena_2024_Chum_vis[,j] <- as.numeric(Chena_2024_Chum_vis[,j])
  Chena_2024_Clarity_vis[,j] <- as.numeric(Chena_2024_Clarity_vis[,j])
  Salcha_2024_Chin_vis[,j] <- as.numeric(Salcha_2024_Chin_vis[,j])
  Salcha_2024_Chum_vis[,j] <- as.numeric(Salcha_2024_Chum_vis[,j])
  Salcha_2024_Clarity_vis[,j] <- as.numeric(Salcha_2024_Clarity_vis[,j])
}

table(as.matrix(is.na(Chena_2024_Chin_vis[,3:10])),
      as.matrix(Chena_2024_Clarity_vis[,3:10]))
table(as.matrix(is.na(Chena_2024_Chum_vis[,3:10])),
      as.matrix(Chena_2024_Clarity_vis[,3:10]))
table(as.matrix(is.na(Salcha_2024_Chin_vis[,3:10])),
      as.matrix(Salcha_2024_Clarity_vis[,3:10]))
table(as.matrix(is.na(Salcha_2024_Chum_vis[,3:10])),
      as.matrix(Salcha_2024_Clarity_vis[,3:10]))

table(as.matrix(Chena_2024_Chin_vis[,3:10])[as.matrix(Chena_2024_Clarity_vis[,3:10]) %in% c(-1,5)], useNA = "ifany")
table(as.matrix(Chena_2024_Chum_vis[,3:10])[as.matrix(Chena_2024_Clarity_vis[,3:10]) %in% c(-1,5)], useNA = "ifany")
table(as.matrix(Salcha_2024_Chin_vis[,3:10])[as.matrix(Salcha_2024_Clarity_vis[,3:10]) %in% c(-1,5)], useNA = "ifany")
table(as.matrix(Salcha_2024_Chum_vis[,3:10])[as.matrix(Salcha_2024_Clarity_vis[,3:10]) %in% c(-1,5)], useNA = "ifany")

## data fix: counts when clarity is 5 or -1 should be NA
for(j in 3:10) {
  Chena_2024_Chin_vis[,j][Chena_2024_Clarity_vis[,j] %in% c(-1,5)] <- NA
  Chena_2024_Chum_vis[,j][Chena_2024_Clarity_vis[,j] %in% c(-1,5)] <- NA
  Salcha_2024_Chin_vis[,j][Salcha_2024_Clarity_vis[,j] %in% c(-1,5)] <- NA
  Salcha_2024_Chum_vis[,j][Salcha_2024_Clarity_vis[,j] %in% c(-1,5)] <- NA
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

reshapelong <- function(x, year=2024) {
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

Chena_2024_Chin_vis_long <- reshapelong(x=Chena_2024_Chin_vis)
Chena_2024_Chum_vis_long <- reshapelong(x=Chena_2024_Chum_vis)
Salcha_2024_Chin_vis_long <- reshapelong(x=Salcha_2024_Chin_vis)
Salcha_2024_Chum_vis_long <- reshapelong(x=Salcha_2024_Chum_vis)

# makeNA0 <- function(x) ifelse(is.na(x), 0, x)
C_all_vis_long <- Chena_2024_Chin_vis_long
C_all_vis_long$count <- Chena_2024_Chin_vis_long$count + Chena_2024_Chum_vis_long$count
S_all_vis_long <- Salcha_2024_Chin_vis_long
S_all_vis_long$count <- Salcha_2024_Chin_vis_long$count + Salcha_2024_Chum_vis_long$count


# ## needed a name, and it works kind of like tapply with a sum
# tsum <- function(mat,index) {
#   uindex <- sort(unique(index))
#   outmat <- matrix(nrow=nrow(mat), ncol=length(uindex))
#   for(j in 1:length(uindex)) {
#     outmat[,j] <- rowSums(as.matrix(mat[,index==uindex[j]]), na.rm=T)
#   }
#   colnames(outmat) <- as.character(uindex)
#   return(outmat)
# }
# 
# 
# # tsum_long <- function(mat,index) {
# #   uindex <- sort(unique(index))
# #   outmat <- matrix(nrow=length(uindex), ncol=ncol(mat))
# #   for(j in 1:length(uindex)) {
# #     outmat[j,] <- colSums(as.matrix(mat[,index==uindex[j]]), na.rm=T)
# #   }
# #   colnames(outmat) <- as.character(uindex)
# #   return(outmat)
# # }


load(file="2024/Rdata/sonardata2024.Rdata")

# CSouth_all_sonar <- subset(all_sonar, station=="Chena South")
# CNorth_all_sonar <- subset(all_sonar, station=="Chena North")
SSouth_all_sonar <- subset(all_sonar, station=="Salcha South")
SNorth_all_sonar <- subset(all_sonar, station=="Salcha North")
# C_all_sonar <- subset(all_sonar, river=="Chena")
S_all_sonar <- subset(all_sonar, river=="Salcha")
# C_blocks_both <- intersect(C_all_vis_long$block[!is.na(C_all_vis_long$count)],
#                            intersect(CSouth_all_sonar$block, CNorth_all_sonar$block))
S_blocks_both <- intersect(S_all_vis_long$block[!is.na(S_all_vis_long$count)],
                           intersect(SSouth_all_sonar$block, SNorth_all_sonar$block))

# C_all_sonar_a2a <- subset(C_all_sonar, block %in% C_blocks_both)
S_all_sonar_a2a <- subset(S_all_sonar, block %in% S_blocks_both)

# C_all_vis_long_a2a <- subset(C_all_vis_long, block %in% C_blocks_both)
S_all_vis_long_a2a <- subset(S_all_vis_long, block %in% S_blocks_both)

# C_sonar_bydate_a2a <- table(C_all_sonar_a2a$date[C_all_sonar_a2a$length >= 400])
S_sonar_bydate_a2a <- table(S_all_sonar_a2a$date[S_all_sonar_a2a$length >= 400])
# C_sonar_byblock_a2a <- table(as.factor(C_all_sonar_a2a$block)[C_all_sonar_a2a$length >= 400])
S_sonar_byblock_a2a <- table(as.factor(S_all_sonar_a2a$block)[S_all_sonar_a2a$length >= 400])
# C_vis_bydate_a2a <- with(C_all_vis_long_a2a, tapply(count, as.character(date), sum))
S_vis_bydate_a2a <- with(S_all_vis_long_a2a, tapply(count, as.character(date), sum))



## Plotting:  First DAILY TOTALS summing only apples:apples counting blocks

par(mfrow=c(1,1))
# plot(as.Date(names(C_sonar_bydate_a2a)), as.numeric(C_sonar_bydate_a2a), 
#      ylim=c(0,as.numeric(max(C_sonar_bydate_a2a, C_vis_bydate_a2a))),
#      type='l',col=2,lwd=2,main="Chena")
# lines(as.Date(names(C_vis_bydate_a2a)), C_vis_bydate_a2a, col=4,lwd=2)
# legend("topleft",lwd=2,col=c(2,4),legend=c("sonar (total)","visual (total)"))
plot(as.Date(names(S_sonar_bydate_a2a)), as.numeric(S_sonar_bydate_a2a), 
     ylim=c(0,as.numeric(max(S_sonar_bydate_a2a, S_vis_bydate_a2a))),
     type='l',col=2,lwd=2,main="Salcha")
points(as.Date(names(S_sonar_bydate_a2a)), as.numeric(S_sonar_bydate_a2a), 
       ylim=c(0,as.numeric(max(S_sonar_bydate_a2a, S_vis_bydate_a2a))),
       type='b',col=2,lwd=2,main="Salcha")
lines(as.Date(names(S_vis_bydate_a2a)), S_vis_bydate_a2a, col=4,lwd=2)
points(as.Date(names(S_vis_bydate_a2a)), S_vis_bydate_a2a, col=4,lwd=2)
legend("topleft",lwd=2,col=c(2,4),legend=c("sonar (total)","visual (total)"))
abline(h=0, lty=3)


# xx <- data.frame(sonar=as.numeric(S_sonar_bydate_a2a),
#                  vis=as.numeric(S_vis_bydate_a2a),
#                  date=as.Date(names(S_vis_bydate_a2a))) %>%
#   pivot_longer(!date,values_to="count",names_to="type")
# ggplot(xx, aes(x=date, y=count, col=type)) + 
#   geom_line()

## Plotting:  just corresponding apples:apples counting blocks.  Plot should probably be zoomed bigly.

par(mfrow=c(1,1))
# plot(as.numeric(C_sonar_byblock_a2a), 
#      ylim=c(0,as.numeric(max(C_sonar_byblock_a2a, C_all_vis_long_a2a$count))),
#      type='l',col=2,lwd=2,main="Chena")
# lines(C_all_vis_long_a2a$count, col=4,lwd=2)
# legend("topleft",lwd=2,col=c(2,4),legend=c("sonar (total)","visual (total)"))
plot(as.numeric(S_sonar_byblock_a2a), 
     ylim=c(0,as.numeric(max(S_sonar_byblock_a2a, S_all_vis_long_a2a$count))),
     type='l',col=2,lwd=2,main="Salcha")
lines(S_all_vis_long_a2a$count, col=4,lwd=2)
legend("topleft",lwd=2,col=c(2,4),legend=c("sonar (total)","visual (total)"))



sum(C_all_sonar_a2a$length >= 400, na.rm=T)
sum(C_all_vis_long_a2a$count)
sum(S_all_sonar_a2a$length >= 400, na.rm=T)
sum(S_all_vis_long_a2a$count)



## Trying multiple truncations.  Difficult to know what's best.

par(mfrow=c(3,3))
truncs <- seq(400,600, by=25)
for(i in 1:length(truncs)) {
  # CSouth_all_sonar <- subset(all_sonar, station=="Chena South")
  # CNorth_all_sonar <- subset(all_sonar, station=="Chena North")
  SSouth_all_sonar <- subset(all_sonar, station=="Salcha South")
  SNorth_all_sonar <- subset(all_sonar, station=="Salcha North")
  # C_all_sonar <- subset(all_sonar, river=="Chena")
  S_all_sonar <- subset(all_sonar, river=="Salcha")
  # C_blocks_both <- intersect(C_all_vis_long$block[!is.na(C_all_vis_long$count)],
  #                            intersect(CSouth_all_sonar$block, CNorth_all_sonar$block))
  S_blocks_both <- intersect(S_all_vis_long$block[!is.na(S_all_vis_long$count)],
                             intersect(SSouth_all_sonar$block, SNorth_all_sonar$block))
  
  # C_all_sonar_a2a <- subset(C_all_sonar, block %in% C_blocks_both)
  S_all_sonar_a2a <- subset(S_all_sonar, block %in% S_blocks_both)
  
  # C_all_vis_long_a2a <- subset(C_all_vis_long, block %in% C_blocks_both)
  S_all_vis_long_a2a <- subset(S_all_vis_long, block %in% S_blocks_both)
  
  # C_sonar_bydate_a2a <- table(C_all_sonar_a2a$date[C_all_sonar_a2a$length >= truncs[i]])
  S_sonar_bydate_a2a <- table(as.factor(S_all_sonar_a2a$date)[S_all_sonar_a2a$length >= truncs[i]])
  # C_sonar_byblock_a2a <- table(as.factor(C_all_sonar_a2a$block)[C_all_sonar_a2a$length >= truncs[i]])
  S_sonar_byblock_a2a <- table(as.factor(S_all_sonar_a2a$block)[S_all_sonar_a2a$length >= truncs[i]])
  # C_vis_bydate_a2a <- with(C_all_vis_long_a2a, tapply(count, as.character(date), sum))
  S_vis_bydate_a2a <- with(S_all_vis_long_a2a, tapply(count, as.character(date), sum))
  
  
  
  
  ## Plotting:  First DAILY TOTALS summing only apples:apples counting blocks
  
  # par(mfrow=c(1,1))
  # plot(as.Date(names(C_sonar_bydate_a2a)), as.numeric(C_sonar_bydate_a2a), 
  #      ylim=c(0,as.numeric(max(C_sonar_bydate_a2a, C_vis_bydate_a2a))),
  #      type='l',col=2,lwd=2,main=paste("Chena -",truncs[i]))
  # lines(as.Date(names(C_vis_bydate_a2a)), C_vis_bydate_a2a, col=4,lwd=2)
  # legend("topleft",lwd=2,col=c(2,4),legend=c("sonar (total)","visual (total)"))
  plot(as.Date(names(S_sonar_bydate_a2a)), as.numeric(S_sonar_bydate_a2a), 
       ylim=c(0,as.numeric(max(S_sonar_bydate_a2a, S_vis_bydate_a2a))),
       type='l',col=2,lwd=2,main=paste("Salcha -",truncs[i]))
  points(as.Date(names(S_sonar_bydate_a2a)), as.numeric(S_sonar_bydate_a2a), 
       ylim=c(0,as.numeric(max(S_sonar_bydate_a2a, S_vis_bydate_a2a))),
       col=2,lwd=2,main=paste("Salcha -",truncs[i]))
  lines(as.Date(names(S_vis_bydate_a2a)), S_vis_bydate_a2a, col=4,lwd=2)
  points(as.Date(names(S_vis_bydate_a2a)), S_vis_bydate_a2a, col=4,lwd=2)
  legend("topleft",lwd=2,col=c(2,4),legend=c("sonar (total)","visual (total)"))
  abline(h=0, lty=3)
}


