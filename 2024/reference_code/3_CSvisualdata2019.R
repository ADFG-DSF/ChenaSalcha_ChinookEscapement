# setwd("~/2014/Analyses/Chena and Delta Clearwater/2018 analysis/2018 TOWERS")
setwd("C:/Users/mbtyers/Desktop/laptop work/ChenaSalcha2019")

#################################
#
#   5. Reading the visual count data and comparing to sonar
#
#################################




library(readxl)
Chena_2019_Chin_vis <- as.data.frame(read_xlsx("Chena Tower 19.xlsx", sheet="King-20 min", range="A3:J126"))
Chena_2019_Chum_vis <- as.data.frame(read_xlsx("Chena Tower 19.xlsx", sheet="Chum 20 min",  range="A3:J126"))
Salcha_2019_Chin_vis <- as.data.frame(read_xlsx("Salcha Tower 19.xlsx", sheet="King-20 min",  range="A3:J126"))
Salcha_2019_Chum_vis <- as.data.frame(read_xlsx("Salcha Tower 19.xlsx", sheet="Chum 20 min",  range="A3:J126"))

# setwd("~/2014/Analyses/Chena and Delta Clearwater/2018 analysis")
# save(Chena_2019_Chin_vis,Chena_2019_Chum_vis,Salcha_2019_Chin_vis,Salcha_2019_Chum_vis,file="vis_2019.Rdata")




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

reshapelong <- function(x, year=2019) {
  reshape1 <- reshape(x, direction="long", varying=list(3:10), times=1:8)
  date1 <- paste(year,substr(as.character(reshape1$Day),6,10),sep="-")
  date_shift_hour_min20 <- paste(date1,reshape1$Shift,reshape1$time,1,sep="_")
  outdf <- data.frame(date=date1,
                      shift=reshape1[,2],
                      hour=reshape1[,3],
                      block=date_shift_hour_min20,
                      count=reshape1[,4])
  outdf1 <- outdf[order(date_shift_hour_min20),]
  return(outdf1)
}

Chena_2019_Chin_vis_long <- reshapelong(x=Chena_2019_Chin_vis)
Chena_2019_Chum_vis_long <- reshapelong(x=Chena_2019_Chum_vis)
Salcha_2019_Chin_vis_long <- reshapelong(x=Salcha_2019_Chin_vis)
Salcha_2019_Chum_vis_long <- reshapelong(x=Salcha_2019_Chum_vis)

# makeNA0 <- function(x) ifelse(is.na(x), 0, x)
C_all_vis_long <- Chena_2019_Chin_vis_long
C_all_vis_long$count <- Chena_2019_Chin_vis_long$count + Chena_2019_Chum_vis_long$count
S_all_vis_long <- Salcha_2019_Chin_vis_long
S_all_vis_long$count <- Salcha_2019_Chin_vis_long$count + Salcha_2019_Chum_vis_long$count


## needed a name, and it works kind of like tapply with a sum
tsum <- function(mat,index) {
  uindex <- sort(unique(index))
  outmat <- matrix(nrow=nrow(mat), ncol=length(uindex))
  for(j in 1:length(uindex)) {
    outmat[,j] <- rowSums(as.matrix(mat[,index==uindex[j]]), na.rm=T)
  }
  colnames(outmat) <- as.character(uindex)
  return(outmat)
}


# tsum_long <- function(mat,index) {
#   uindex <- sort(unique(index))
#   outmat <- matrix(nrow=length(uindex), ncol=ncol(mat))
#   for(j in 1:length(uindex)) {
#     outmat[j,] <- colSums(as.matrix(mat[,index==uindex[j]]), na.rm=T)
#   }
#   colnames(outmat) <- as.character(uindex)
#   return(outmat)
# }


# setwd("~/2014/Analyses/Chena and Delta Clearwater/2018 analysis")

load(file="sonardata2019.Rdata")

CSouth_all_sonar <- subset(all_sonar, station=="Chena South")
CNorth_all_sonar <- subset(all_sonar, station=="Chena North")
SSouth_all_sonar <- subset(all_sonar, station=="Salcha South")
SNorth_all_sonar <- subset(all_sonar, station=="Salcha North")
C_all_sonar <- subset(all_sonar, river=="Chena")
S_all_sonar <- subset(all_sonar, river=="Salcha")
C_blocks_both <- intersect(C_all_vis_long$block[!is.na(C_all_vis_long$count)],
                           intersect(CSouth_all_sonar$block, CNorth_all_sonar$block))
S_blocks_both <- intersect(S_all_vis_long$block[!is.na(S_all_vis_long$count)],
                           intersect(SSouth_all_sonar$block, SNorth_all_sonar$block))

C_all_sonar_a2a <- subset(C_all_sonar, block %in% C_blocks_both)
S_all_sonar_a2a <- subset(S_all_sonar, block %in% S_blocks_both)

C_all_vis_long_a2a <- subset(C_all_vis_long, block %in% C_blocks_both)
S_all_vis_long_a2a <- subset(S_all_vis_long, block %in% S_blocks_both)

C_sonar_bydate_a2a <- table(C_all_sonar_a2a$date[C_all_sonar_a2a$length >= 400])
S_sonar_bydate_a2a <- table(S_all_sonar_a2a$date[S_all_sonar_a2a$length >= 400])
C_sonar_byblock_a2a <- table(as.factor(C_all_sonar_a2a$block)[C_all_sonar_a2a$length >= 400])
S_sonar_byblock_a2a <- table(as.factor(S_all_sonar_a2a$block)[S_all_sonar_a2a$length >= 400])
C_vis_bydate_a2a <- with(C_all_vis_long_a2a, tapply(count, as.character(date), sum))
S_vis_bydate_a2a <- with(S_all_vis_long_a2a, tapply(count, as.character(date), sum))




## Plotting:  First DAILY TOTALS summing only apples:apples counting blocks

par(mfrow=c(2,1))
plot(as.Date(names(C_sonar_bydate_a2a)), as.numeric(C_sonar_bydate_a2a), 
     ylim=c(0,as.numeric(max(C_sonar_bydate_a2a, C_vis_bydate_a2a))),
     type='l',col=2,lwd=2,main="Chena")
lines(as.Date(names(C_vis_bydate_a2a)), C_vis_bydate_a2a, col=4,lwd=2)
legend("topleft",lwd=2,col=c(2,4),legend=c("sonar (total)","visual (total)"))
plot(as.Date(names(S_sonar_bydate_a2a)), as.numeric(S_sonar_bydate_a2a), 
     ylim=c(0,as.numeric(max(S_sonar_bydate_a2a, S_vis_bydate_a2a))),
     type='l',col=2,lwd=2,main="Salcha")
lines(as.Date(names(S_vis_bydate_a2a)), S_vis_bydate_a2a, col=4,lwd=2)
legend("topleft",lwd=2,col=c(2,4),legend=c("sonar (total)","visual (total)"))



## Plotting:  just corresponding apples:apples counting blocks.  Plot should probably be zoomed bigly.

par(mfrow=c(2,1))
plot(as.numeric(C_sonar_byblock_a2a), 
     ylim=c(0,as.numeric(max(C_sonar_byblock_a2a, C_all_vis_long_a2a$count))),
     type='l',col=2,lwd=2,main="Chena")
lines(C_all_vis_long_a2a$count, col=4,lwd=2)
legend("topleft",lwd=2,col=c(2,4),legend=c("sonar (total)","visual (total)"))
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

truncs <- seq(400,600, by=25)
for(i in 1:length(truncs)) {
  CSouth_all_sonar <- subset(all_sonar, station=="Chena South")
  CNorth_all_sonar <- subset(all_sonar, station=="Chena North")
  SSouth_all_sonar <- subset(all_sonar, station=="Salcha South")
  SNorth_all_sonar <- subset(all_sonar, station=="Salcha North")
  C_all_sonar <- subset(all_sonar, river=="Chena")
  S_all_sonar <- subset(all_sonar, river=="Salcha")
  C_blocks_both <- intersect(C_all_vis_long$block[!is.na(C_all_vis_long$count)],
                             intersect(CSouth_all_sonar$block, CNorth_all_sonar$block))
  S_blocks_both <- intersect(S_all_vis_long$block[!is.na(S_all_vis_long$count)],
                             intersect(SSouth_all_sonar$block, SNorth_all_sonar$block))
  
  C_all_sonar_a2a <- subset(C_all_sonar, block %in% C_blocks_both)
  S_all_sonar_a2a <- subset(S_all_sonar, block %in% S_blocks_both)
  
  C_all_vis_long_a2a <- subset(C_all_vis_long, block %in% C_blocks_both)
  S_all_vis_long_a2a <- subset(S_all_vis_long, block %in% S_blocks_both)
  
  C_sonar_bydate_a2a <- table(C_all_sonar_a2a$date[C_all_sonar_a2a$length >= truncs[i]])
  S_sonar_bydate_a2a <- table(S_all_sonar_a2a$date[S_all_sonar_a2a$length >= truncs[i]])
  C_sonar_byblock_a2a <- table(as.factor(C_all_sonar_a2a$block)[C_all_sonar_a2a$length >= truncs[i]])
  S_sonar_byblock_a2a <- table(as.factor(S_all_sonar_a2a$block)[S_all_sonar_a2a$length >= truncs[i]])
  C_vis_bydate_a2a <- with(C_all_vis_long_a2a, tapply(count, as.character(date), sum))
  S_vis_bydate_a2a <- with(S_all_vis_long_a2a, tapply(count, as.character(date), sum))
  
  
  
  
  ## Plotting:  First DAILY TOTALS summing only apples:apples counting blocks
  
  par(mfrow=c(2,1))
  plot(as.Date(names(C_sonar_bydate_a2a)), as.numeric(C_sonar_bydate_a2a), 
       ylim=c(0,as.numeric(max(C_sonar_bydate_a2a, C_vis_bydate_a2a))),
       type='l',col=2,lwd=2,main=paste("Chena -",truncs[i]))
  lines(as.Date(names(C_vis_bydate_a2a)), C_vis_bydate_a2a, col=4,lwd=2)
  legend("topleft",lwd=2,col=c(2,4),legend=c("sonar (total)","visual (total)"))
  plot(as.Date(names(S_sonar_bydate_a2a)), as.numeric(S_sonar_bydate_a2a), 
       ylim=c(0,as.numeric(max(S_sonar_bydate_a2a, S_vis_bydate_a2a))),
       type='l',col=2,lwd=2,main=paste("Salcha -",truncs[i]))
  lines(as.Date(names(S_vis_bydate_a2a)), S_vis_bydate_a2a, col=4,lwd=2)
  legend("topleft",lwd=2,col=c(2,4),legend=c("sonar (total)","visual (total)"))
}




## Trying a whole bunch of truncations.  I don't remember what most of this does
## and I also don't remember it seeming useful.

truncs <- 350:500
Csum <- Ssum <- NA
Cmat <- matrix(NA, nrow=length(C_sonar_byblock_a2a), ncol=length(truncs))
Smat <- matrix(NA, nrow=length(S_sonar_byblock_a2a), ncol=length(truncs))
for(i in 1:length(truncs)) {
  Csum[i] <- sum(C_all_sonar_a2a$length >= truncs[i], na.rm=T)
  Ssum[i] <- sum(S_all_sonar_a2a$length >= truncs[i], na.rm=T)
  Cmat[,i] <- as.numeric(table(as.factor(C_all_sonar_a2a$block)[C_all_sonar_a2a$length >= truncs[i]]))
  Smat[,i] <- as.numeric(table(as.factor(S_all_sonar_a2a$block)[S_all_sonar_a2a$length >= truncs[i]]))
}
par(mfrow=c(2,1))
plot(truncs, Csum)
abline(h=sum(C_all_vis_long_a2a$count), main="Chena")
plot(truncs, Ssum)
abline(h=sum(S_all_vis_long_a2a$count), main="Salcha")

Cdiffmat <- Cmat - C_all_vis_long_a2a$count
Sdiffmat <- Smat - S_all_vis_long_a2a$count
# Cdiffmat <- (Cmat+1) / (C_all_vis_long_a2a$count+1)
# Sdiffmat <- (Smat+1) / (S_all_vis_long_a2a$count+1)

par(mfrow=c(2,1))
plot(truncs, apply(Cdiffmat,2,median), main="Chena")
plot(truncs, apply(Sdiffmat,2,median), main="Salcha")

par(mfrow=c(2,1))
cols <- adjustcolor(rainbow(length(truncs)), red.f=.85, blue.f=.85, green.f=.85)
plot(Cdiffmat[,1], ylim=range(Cdiffmat),type='l', main="Chena")
for(i in 1:ncol(Cdiffmat)) lines(Cdiffmat[,i], col=cols[i])
abline(h=0, lty=2)
whichones <- seq(1,length(cols),by=10)
legend("bottomleft",lwd=1,col=cols[whichones], legend=truncs[whichones])
plot(Sdiffmat[,1], ylim=range(Sdiffmat),type='l', main="Salcha")
for(i in 1:ncol(Sdiffmat)) lines(Sdiffmat[,i], col=cols[i])
abline(h=0, lty=2)
legend("bottomleft",lwd=1,col=cols[whichones], legend=truncs[whichones])

par(mfrow=c(1,1))
plot(NA, xlim=range(truncs), ylim=range(Cdiffmat), main="Chena")
for(i in 1:length(truncs)) points(rep(truncs[i], nrow(Cdiffmat)), Cdiffmat[,i])
abline(h=0)
plot(NA, xlim=range(truncs), ylim=range(Sdiffmat), main="Salcha")
for(i in 1:length(truncs)) points(rep(truncs[i], nrow(Sdiffmat)), Sdiffmat[,i])
abline(h=0)



Cdiffmat_day <- matrix(nrow=length(C_vis_bydate_a2a), ncol=length(truncs))
Sdiffmat_day <- matrix(nrow=length(S_vis_bydate_a2a), ncol=length(truncs))
for(i in 1:length(truncs)) {
  Cdiffmat_day[,i] <- tapply(Cdiffmat[,i], as.character(C_all_vis_long_a2a$date), sum, na.rm=T)
  Sdiffmat_day[,i] <- tapply(Sdiffmat[,i], as.character(S_all_vis_long_a2a$date), sum, na.rm=T)
}
par(mfrow=c(2,1))
cols <- adjustcolor(rainbow(length(truncs)), red.f=.85, blue.f=.85, green.f=.85)
plot(Cdiffmat_day[,1], ylim=range(Cdiffmat_day),type='l', main="Chena")
for(i in 1:ncol(Cdiffmat_day)) lines(Cdiffmat_day[,i], col=cols[i])
abline(h=0, lty=2)
whichones <- seq(1,length(cols),by=10)
legend("bottomleft",lwd=1,col=cols[whichones], legend=truncs[whichones])
plot(Sdiffmat_day[,1], ylim=range(Sdiffmat_day),type='l', main="Salcha")
for(i in 1:ncol(Sdiffmat_day)) lines(Sdiffmat_day[,i], col=cols[i])
abline(h=0, lty=2)
legend("bottomleft",lwd=1,col=cols[whichones], legend=truncs[whichones])



Cdiffdensmat <- Sdiffdensmat <- matrix(nrow=512, ncol=length(truncs))
bw <- density(c(as.numeric(Cdiffmat), as.numeric(Sdiffmat)))$bw
for(i in 1:length(truncs)) {
  Cdiffdensmat[,i] <- density(Cdiffmat[,i], from=min(Cdiffmat), to=max(Cdiffmat), bw=bw)$y
  Sdiffdensmat[,i] <- density(Sdiffmat[,i], from=min(Sdiffmat), to=max(Sdiffmat), bw=bw)$y
}
par(mfrow=c(2,1))
contour(z=Cdiffdensmat, y=truncs, x=density(Cdiffmat[,i], from=min(Cdiffmat), 
                                            to=max(Cdiffmat), bw=bw)$x, xlim=1*c(-1,1), 
        nlevels=20, main="Chena")
abline(v=0, lty=2, lwd=2)
contour(z=Sdiffdensmat, y=truncs, x=density(Sdiffmat[,i], from=min(Sdiffmat), 
                                            to=max(Sdiffmat), bw=bw)$x, xlim=1*c(-1,1), 
        nlevels=20, main="Salcha")
abline(v=0, lty=2, lwd=2)



# # didson v aris
# C_all_sonar_a2a_D <- subset(C_all_sonar_a2a, sonartype=="DIDSON")
# C_all_sonar_a2a_A <- subset(C_all_sonar_a2a, sonartype=="ARIS")
# S_all_sonar_a2a_D <- subset(S_all_sonar_a2a, sonartype=="DIDSON")
# S_all_sonar_a2a_A <- subset(S_all_sonar_a2a, sonartype=="ARIS")
# 
# C_blocks_both_D <- sort(unique(C_all_sonar_a2a_D$block))
# C_blocks_both_A <- sort(unique(C_all_sonar_a2a_A$block))
# S_blocks_both_D <- sort(unique(S_all_sonar_a2a_D$block))
# S_blocks_both_A <- sort(unique(S_all_sonar_a2a_A$block))
# 
# C_all_vis_long_a2a_D <- subset(C_all_vis_long, block %in% C_blocks_both_D)
# S_all_vis_long_a2a_D <- subset(S_all_vis_long, block %in% S_blocks_both_D)
# C_all_vis_long_a2a_A <- subset(C_all_vis_long, block %in% C_blocks_both_A)
# S_all_vis_long_a2a_A <- subset(S_all_vis_long, block %in% S_blocks_both_A)
# # this is not going to work