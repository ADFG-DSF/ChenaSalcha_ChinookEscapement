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

# looking at the raw data files
summary(Chena_2024_Chin_vis)
summary(Chena_2024_Chum_vis)
summary(Chena_2024_Clarity_vis)
summary(Salcha_2024_Chin_vis)
summary(Salcha_2024_Chum_vis)
summary(Salcha_2024_Clarity_vis)

# this was only needed once, but easy enough to fix for everyone
# - one of the count columns was formatted as character
for(j in 3:10) {
  Chena_2024_Chin_vis[,j] <- as.numeric(Chena_2024_Chin_vis[,j])
  Chena_2024_Chum_vis[,j] <- as.numeric(Chena_2024_Chum_vis[,j])
  Chena_2024_Clarity_vis[,j] <- as.numeric(Chena_2024_Clarity_vis[,j])
  Salcha_2024_Chin_vis[,j] <- as.numeric(Salcha_2024_Chin_vis[,j])
  Salcha_2024_Chum_vis[,j] <- as.numeric(Salcha_2024_Chum_vis[,j])
  Salcha_2024_Clarity_vis[,j] <- as.numeric(Salcha_2024_Clarity_vis[,j])
}

# checking to see if counts are present where the shouldn't be
# - bad visibility or recorded no-count (clarity of -1, 4, or 5)
table(as.matrix(is.na(Chena_2024_Chin_vis[,3:10])),
      as.matrix(Chena_2024_Clarity_vis[,3:10]))
table(as.matrix(is.na(Chena_2024_Chum_vis[,3:10])),
      as.matrix(Chena_2024_Clarity_vis[,3:10]))
table(as.matrix(is.na(Salcha_2024_Chin_vis[,3:10])),
      as.matrix(Salcha_2024_Clarity_vis[,3:10]))
table(as.matrix(is.na(Salcha_2024_Chum_vis[,3:10])),
      as.matrix(Salcha_2024_Clarity_vis[,3:10]))

# what ARE the counts we would be missing if we turn these into NA
table(as.matrix(Chena_2024_Chin_vis[,3:10])[as.matrix(Chena_2024_Clarity_vis[,3:10]) %in% c(-1,4,5)], useNA = "ifany")
table(as.matrix(Chena_2024_Chum_vis[,3:10])[as.matrix(Chena_2024_Clarity_vis[,3:10]) %in% c(-1,4,5)], useNA = "ifany")
table(as.matrix(Salcha_2024_Chin_vis[,3:10])[as.matrix(Salcha_2024_Clarity_vis[,3:10]) %in% c(-1,4,5)], useNA = "ifany")
table(as.matrix(Salcha_2024_Chum_vis[,3:10])[as.matrix(Salcha_2024_Clarity_vis[,3:10]) %in% c(-1,4,5)], useNA = "ifany")

# fixing
for(j in 3:10) {
  ## data fix: counts when clarity is 4, 5 or -1 should be NA
  Chena_2024_Chin_vis[,j][Chena_2024_Clarity_vis[,j] %in% c(-1,4,5)] <- NA
  Chena_2024_Chum_vis[,j][Chena_2024_Clarity_vis[,j] %in% c(-1,4,5)] <- NA
  Salcha_2024_Chin_vis[,j][Salcha_2024_Clarity_vis[,j] %in% c(-1,4,5)] <- NA
  Salcha_2024_Chum_vis[,j][Salcha_2024_Clarity_vis[,j] %in% c(-1,4,5)] <- NA
  
  ## data fix: counts when clarity is 1-3 should be zero, not NA
  #### maybe safer to impute from other species??
  Chena_2024_Chin_vis[,j][Chena_2024_Clarity_vis[,j] %in% 1:3 &
                            is.na(Chena_2024_Chin_vis[,j])] <- 0
  Chena_2024_Chum_vis[,j][Chena_2024_Clarity_vis[,j] %in% 1:3 &
                            is.na(Chena_2024_Chum_vis[,j])] <- 0
  Salcha_2024_Chin_vis[,j][Salcha_2024_Clarity_vis[,j] %in% 1:3 &
                             is.na(Salcha_2024_Chin_vis[,j])] <- 0
  Salcha_2024_Chum_vis[,j][Salcha_2024_Clarity_vis[,j] %in% 1:3 &
                             is.na(Salcha_2024_Chum_vis[,j])] <- 0
}

# checking to make sure fixes worked
table(as.matrix(is.na(Chena_2024_Chin_vis[,3:10])),
      as.matrix(Chena_2024_Clarity_vis[,3:10]))
table(as.matrix(is.na(Chena_2024_Chum_vis[,3:10])),
      as.matrix(Chena_2024_Clarity_vis[,3:10]))
table(as.matrix(is.na(Salcha_2024_Chin_vis[,3:10])),
      as.matrix(Salcha_2024_Clarity_vis[,3:10]))
table(as.matrix(is.na(Salcha_2024_Chum_vis[,3:10])),
      as.matrix(Salcha_2024_Clarity_vis[,3:10]))


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
Chena_2024_Clarity_vis_long <- reshapelong(x=Chena_2024_Clarity_vis)
Salcha_2024_Clarity_vis_long <- reshapelong(x=Salcha_2024_Clarity_vis)

# making datasets with ALL fish for checking
# - the idea being sonar totals should approximately match visual totals
#   before species apportionment
C_all_vis_long <- Chena_2024_Chin_vis_long
C_all_vis_long$count <- Chena_2024_Chin_vis_long$count + Chena_2024_Chum_vis_long$count
S_all_vis_long <- Salcha_2024_Chin_vis_long
S_all_vis_long$count <- Salcha_2024_Chin_vis_long$count + Salcha_2024_Chum_vis_long$count

C_all_vis_long$clarity <- Chena_2024_Clarity_vis_long$count
S_all_vis_long$clarity <- Salcha_2024_Clarity_vis_long$count




# investigating a possible tech effect for visual quality
# - note: this dataset is not publicly available
tech24 <- read.csv("2024/addl_data/tech24.csv") %>%
  mutate(date=datesmasher(Date)) %>%
  select(-Date) %>%
  pivot_longer(c("X1","X2","X3"), names_to="shift", values_to = "tech") %>% 
  mutate(shift = as.numeric(substr(shift, 2, 2)))
head(tech24, 30)

S_all_vis_long$tech <- NA
for(i in 1:nrow(tech24)) {
  S_all_vis_long$tech[S_all_vis_long$date == tech24$date[i] & 
                        S_all_vis_long$shift == tech24$shift[i]] <- tech24$tech[i]
}




## loading sonar data and tabulating totals where there are blocks of time
## with sonar AND visual (apples:apples)

load(file="2024/Rdata/sonardata2024.Rdata")


# COMMENT OUT WHEN THIS IS NOT USED
# if mixture model has been run, this will load the median species apportionment
# for each fish

# # specmed <- CSmix_jags_out$q50$species
# specmed <- CSmix_jags_out_alt$q50$species
# all_sonar$spec <- NA
# all_sonar$spec[!is.na(all_sonar$length) & all_sonar$length>=400] <- specmed


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

# S_blocks_both1 <- sort(unique(subset(S_all_vis_long, block %in% S_blocks_both & clarity %in% 1:2)$block))
# S_blocks_both <- S_blocks_both1

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


# checking overall totals

# sum(C_all_sonar_a2a$length >= 400, na.rm=T)
# sum(C_all_vis_long_a2a$count)
sum(S_all_sonar_a2a$length >= 400, na.rm=T)
sum(S_all_vis_long_a2a$count)
sum(S_sonar_byblock_a2a)


# adding sonar counts to the visual dataframe (long) for easier comparison

all(names(S_sonar_byblock_a2a) == S_all_vis_long_a2a$block)
S_all_vis_long_a2a$sonarcount <- S_sonar_byblock_a2a
S_all_vis_long_a2a$time <- (S_all_vis_long_a2a$shift-1)*8 + (S_all_vis_long_a2a$hour-1)

# trying a discrepancy score, kinda works
S_all_vis_long_a2a$dscore1 <- ((S_all_vis_long_a2a$count + 1) / 
                              (S_all_vis_long_a2a$sonarcount + 1))

a2a <- S_all_vis_long_a2a # making the name shorter for less typing

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

yglm <- cbind(as.numeric(a2a$count),
              ifelse(as.numeric(a2a$count)>as.numeric(a2a$sonarcount),
                     as.numeric(a2a$count), as.numeric(a2a$sonarcount)))

a2a$tech2 <- ifelse(a2a$tech=="Matt", "Matt", "NotMatt")
a2a$tech3 <- ifelse(a2a$tech=="Matt", "a", 
                    ifelse(a2a$tech %in% c("Francine", "Mike"), "b", "c"))
a2a$clarity2 <- ifelse(a2a$clarity == 3, 3, "1-2")
glm(yglm ~ a2a$tech, family="binomial") %>% AIC
glm(yglm ~ a2a$tech2, family="binomial") %>% AIC
glm(yglm ~ a2a$tech3, family="binomial") %>% AIC
glm(yglm ~ a2a$shift, family="binomial") %>% AIC
glm(yglm ~ a2a$clarity, family="binomial") %>% AIC
glm(yglm ~ a2a$clarity2, family="binomial") %>% AIC
glm(yglm ~ factor(a2a$clarity), family="binomial") %>% AIC
glm(yglm ~ factor(a2a$time), family="binomial") %>% AIC
glm(yglm ~ factor(a2a$clarity) + a2a$tech, family="binomial") %>% AIC
glm(yglm ~ factor(a2a$clarity) * a2a$tech, family="binomial") %>% AIC
glm(yglm ~ factor(a2a$clarity2) + a2a$tech, family="binomial") %>% AIC
glm(yglm ~ factor(a2a$clarity2) * a2a$tech, family="binomial") %>% AIC
glm(yglm ~ factor(a2a$clarity) + a2a$tech2, family="binomial") %>% AIC
glm(yglm ~ factor(a2a$clarity) * a2a$tech2, family="binomial") %>% AIC
glm(yglm ~ factor(a2a$clarity2) + a2a$tech2, family="binomial") %>% AIC
glm(yglm ~ factor(a2a$clarity2) * a2a$tech2, family="binomial") %>% AIC
glm(yglm ~ factor(a2a$clarity) + a2a$tech3, family="binomial") %>% AIC
glm(yglm ~ factor(a2a$clarity) * a2a$tech3, family="binomial") %>% AIC
glm(yglm ~ factor(a2a$clarity2) + a2a$tech3, family="binomial") %>% summary
glm(yglm ~ factor(a2a$clarity2) * a2a$tech3, family="binomial") %>% AIC

boxplot(a2a$dscore1 ~ a2a$tech3)
boxplot(a2a$dscore1 ~ a2a$clarity2)
boxplot(a2a$dscore1 ~ paste(a2a$tech3,a2a$clarity2), las=2, xlab="")
with(subset(a2a, !is.na(clarity2)), boxplot(dscore1 ~ paste(tech3, clarity2), col=c(2,3,3,4,4)))



# a whooooooooole bunch of visualizations!

S_all_vis_long_a2a %>%
  ggplot(aes(x=jitter(sonarcount), y=jitter(count), colour=factor(clarity))) +
  geom_point() +
  geom_abline(slope=1, intercept=0, lty=3)

S_all_vis_long_a2a %>%
  ggplot(aes(x=jitter(sonarcount), y=jitter(count), colour=factor(tech))) +
  facet_wrap(~factor(clarity)) +
  geom_point() +
  geom_abline(slope=1, intercept=0, lty=3)

S_all_vis_long_a2a %>%
  ggplot(aes(x=jitter(sonarcount), y=jitter(count), colour=factor(clarity))) +
  facet_wrap(~factor(tech)) +
  geom_point() +
  geom_abline(slope=1, intercept=0, lty=3)

S_all_vis_long_a2a %>%
  ggplot(aes(x=jitter(sonarcount), y=jitter(count), colour=factor(clarity))) +
  facet_wrap(~factor(time)) +
  geom_point() +
  geom_abline(slope=1, intercept=0, lty=3)

S_all_vis_long_a2a %>%
  ggplot(aes(x=jitter(sonarcount), y=jitter(count), colour=factor(clarity))) +
  facet_wrap(~factor(date)) +
  geom_point() +
  geom_abline(slope=1, intercept=0, lty=3)

S_all_vis_long_a2a %>%
  ggplot(aes(x=jitter(sonarcount), y=jitter(count), colour=factor(clarity), pch=tech)) +
  facet_wrap(~factor(date)) +
  geom_point() +
  geom_abline(slope=1, intercept=0, lty=3)

S_all_vis_long_a2a %>%
  group_by(tech) %>%
  summarise(sonarsum=sum(sonarcount), visualsum=sum(count)) %>%
  ggplot(aes(x=sonarsum, y=visualsum, colour=tech)) + 
  geom_point() +
  scale_x_continuous(limits=c(0,220)) +
  scale_y_continuous(limits=c(0,220)) +
  geom_abline(slope=1, intercept=0, lty=3)

S_all_vis_long_a2a %>%
  group_by(clarity) %>%
  summarise(sonarsum=sum(sonarcount), visualsum=sum(count)) %>%
  ggplot(aes(x=sonarsum, y=visualsum, colour=factor(clarity))) + 
  geom_point() +
  scale_x_continuous(limits=c(0,260)) +
  scale_y_continuous(limits=c(0,260)) +
  geom_abline(slope=1, intercept=0, lty=3)

S_all_vis_long_a2a %>%
  group_by(tech, clarity) %>%
  summarise(sonarsum=sum(sonarcount), visualsum=sum(count)) %>%
  ggplot(aes(x=sonarsum, y=visualsum, colour=factor(clarity))) + 
  facet_wrap(~tech) +
  geom_point() +
  scale_x_continuous(limits=c(0,80)) +
  scale_y_continuous(limits=c(0,80)) +
  geom_abline(slope=1, intercept=0, lty=3)

S_all_vis_long_a2a %>%
  group_by(time) %>%
  summarise(sonarsum=sum(sonarcount), visualsum=sum(count)) %>%
  ggplot(aes(x=sonarsum, y=visualsum, colour=factor(time))) + 
  geom_point() +
  scale_x_continuous(limits=c(0,45)) +
  scale_y_continuous(limits=c(0,45)) +
  geom_abline(slope=1, intercept=0, lty=3)

S_all_vis_long_a2a %>%
  group_by(date, shift) %>%
  summarise(sonarsum=sum(sonarcount), visualsum=sum(count), tech=tech[1], clarity=round(median(clarity))) %>%
  mutate(dscore1=(visualsum+10)/(sonarsum+10)) %>%
  mutate(shiftshift=as.Date(date)+shift/3) %>%
  ggplot(aes(x=shiftshift, y=dscore1, pch=tech, col=factor(clarity))) + 
  geom_point() #+
  # scale_x_continuous(limits=c(0,45)) +
  # scale_y_continuous(limits=c(0,45)) +
  # geom_abline(slope=1, intercept=0, lty=3)



S_all_vis_long_a2a %>%
  group_by(date, clarity) %>%
  summarise(sonarsum=sum(sonarcount), visualsum=sum(count)) -> aa #%>%
  # pivot_longer(cols=c("sonarsum","visualsum"), names_to="type") %>%
  # ggplot(aes(x=date, y=value, colour=type)) + 
  # facet_wrap(~clarity)+
  # geom_line() 

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
  
  # S_blocks_both1 <- sort(unique(subset(S_all_vis_long, block %in% S_blocks_both & clarity %in% 1:2)$block))
  # S_blocks_both <- S_blocks_both1
  
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


