# setwd("~/2014/Analyses/Chena and Delta Clearwater/2018 analysis")
setwd("C:/Users/mbtyers/Desktop/laptop work/ChenaSalcha2019")

#################################
#
#   4. The mixture model!
#
#################################




load(file="CSpriors2019.Rdata")
load(file="sonardata2019.Rdata")

# all_fish <- subset(all_sonar, length>=400)


## recentering(ish) the dates

days1.205 <- all_fish$date - as.numeric(as.Date("2018-12-31",format="%Y-%m-%d")) - 205
days1.205 <- as.numeric(days1.205)

## sonar-measured lengths and actual lengths from the tethered-fish experiment 
## This was once used directly in the JAGS model but now runs in an lm() outside to simplify 
dl <- c(632,602,1049,664,768,663,1025,685,681,957,953,747,646,666,627,531,584)
al <- c(740,600,1170,710,950,650,1020,665,760,1040,970,840,700,700,620,620,690)



## behold the beast

cat('model {
    for(i in 1:n.fish) {
      L.mm.D[i] ~ dnorm(muL[i],precL)
      muL[i] <- betaD0[sonar[i]] + betaD1[sonar[i]]*L.mm.act[i]        ########
      L.mm.act[i] ~ dnorm(mu[i],tau[i])
      mu[i] <- lambda[species[i],sex[i],river[i]]
      tau[i] <- prec[species[i],sex[i],river[i]]
  
      species[i] ~ dcat(ps[i,1:2])
      sex[i] ~ dcat(psex[species[i],1:2])
  
      logit(pi[i]) <- b0[river[i]]+b1[river[i]]*day[i]
      alpha.inf[i,1] <- pi[i]
      alpha.inf[i,2] <- (1-pi[i])
      ps[i,1:2] ~ ddirch(alpha.inf[i,1:2])
    }
    
    prec[1,1,1] <- pow(chena_chin_m_sd,-2)
    prec[1,2,1] <- pow(chena_chin_f_sd,-2)
    prec[2,1,1] <- pow(chena_chum_m_sd,-2)
    prec[2,2,1] <- pow(chena_chum_f_sd,-2)
    prec[1,1,2] <- pow(salcha_chin_m_sd,-2)
    prec[1,2,2] <- pow(salcha_chin_f_sd,-2)
    prec[2,1,2] <- pow(salcha_chum_m_sd,-2)
    prec[2,2,2] <- pow(salcha_chum_f_sd,-2)
    
    chena_chin_m_sd ~ dnorm(chena_chin_m_sd_mn, chena_chin_m_sd_prec)
    chena_chin_f_sd ~ dnorm(chena_chin_f_sd_mn, chena_chin_f_sd_prec)
    chena_chum_m_sd ~ dnorm(chena_chum_m_sd_mn, chena_chum_m_sd_prec)
    chena_chum_f_sd ~ dnorm(chena_chum_f_sd_mn, chena_chum_f_sd_prec)
    salcha_chin_m_sd ~ dnorm(salcha_chin_m_sd_mn, salcha_chin_m_sd_prec)
    salcha_chin_f_sd ~ dnorm(salcha_chin_f_sd_mn, salcha_chin_f_sd_prec)
    salcha_chum_m_sd ~ dnorm(salcha_chum_m_sd_mn, salcha_chum_m_sd_prec)
    salcha_chum_f_sd ~ dnorm(salcha_chum_f_sd_mn, salcha_chum_f_sd_prec)
    chena_chin_m_sd_prec <- pow(chena_chin_m_sd_sd,-2)
    chena_chin_f_sd_prec <- pow(chena_chin_f_sd_sd,-2)
    chena_chum_m_sd_prec <- pow(chena_chum_m_sd_sd,-2)
    chena_chum_f_sd_prec <- pow(chena_chum_f_sd_sd,-2)
    salcha_chin_m_sd_prec <- pow(salcha_chin_m_sd_sd,-2)
    salcha_chin_f_sd_prec <- pow(salcha_chin_f_sd_sd,-2)
    salcha_chum_m_sd_prec <- pow(salcha_chum_m_sd_sd,-2)
    salcha_chum_f_sd_prec <- pow(salcha_chum_f_sd_sd,-2)

    # for(j in 1:m) {
    #   DL.star[j] ~ dnorm(mu.starD[j],prec.star)
    #   mu.star[j] <- betaD0[1] + betaD1[1]*AL.star[j]         ########
    # }
    precL <- 1/(54.59*54.59)
    # betaD0[1] ~ dnorm(0,0.01)         ########
    # betaD1[1] ~ dnorm(1,0.01)         ########
    # betaD0[2] ~ dnorm(0,0.01)         ########
    # betaD1[2] ~ dnorm(1,0.01)         ########
    # prec.star ~ dunif(0.0001,1000)
    # sig.star <- 1/sqrt(prec.star)

    betaD0[1] ~ dnorm(betaD0_mn,betaD0_prec)         ########
    betaD1[1] ~ dnorm(betaD1_mn,betaD1_prec)         ########
    betaD0[2] ~ dnorm(betaD0_mn,betaD0_prec)         ########
    betaD1[2] ~ dnorm(betaD1_mn,betaD1_prec)         ########
    # precbD0 <- 1/betaD0_se/betaD0_se         ########
    # precbD1 <- 1/betaD1_se/betaD1_se         ########

    # betaD0_mn <- 44.81988
    # betaD1_mn <- 0.87156
    # betaD0_se <- 63.36818
    # betaD1_se <- 0.07835

    b0 <- mu.b0 # ~ dnorm(mu.b0,prec.b0) #
    b1 <- mu.b1 # ~ dnorm(mu.b1,prec.b1) #

    # b0 ~ dmnorm(mu.b0[],tau.b0[,])
    # b1 ~ dmnorm(mu.b1[],tau.b1[,])
    # tau.b0[1:2,1:2] <- inverse(sig.b0[,])
    # tau.b1[1:2,1:2] <- inverse(sig.b1[,])
    # b0[1] ~ dnorm(mu.b0[1],tau.b0[1])
    # b0[2] ~ dnorm(mu.b0[2],tau.b0[2])
    # b1[1] ~ dnorm(mu.b1[1],tau.b1[1])
    # b1[2] ~ dnorm(mu.b1[2],tau.b1[2])

    # mu.b1 <- -0.1642404                    # calculated from 2015 visual counts (limited)
    # mu.b0 <- -0.7403224
    # mu.b1 <- -0.2812552                    # calculated from 1993-2015 counts
    # mu.b0 <- -0.4587457#-0.5185915
    prec.b0 <- 1/0.6543603#1/2.01842
    prec.b1 <- 1/0.003060084

    psex[1,1:2] ~ ddirch(alpha.sex.chin[])
    psex[2,1:2] ~ ddirch(alpha.sex.chum[])

    lambda[1,1,1] ~ dnorm(chena_chin_m_mn,t1)
    lambda[1,2,1] ~ dnorm(chena_chin_f_mn,t2)
    lambda[2,1,1] ~ dnorm(chena_chum_m_mn,t3)
    lambda[2,2,1] ~ dnorm(chena_chum_f_mn,t4)
    lambda[1,1,2] ~ dnorm(salcha_chin_m_mn,t5)
    lambda[1,2,2] ~ dnorm(salcha_chin_f_mn,t6)
    lambda[2,1,2] ~ dnorm(salcha_chum_m_mn,t7)
    lambda[2,2,2] ~ dnorm(salcha_chum_f_mn,t8)

    t1 <- pow(chena_chin_m_se,-2)
    t2 <- pow(chena_chin_f_se,-2)
    t3 <- pow(chena_chum_m_se,-2)
    t4 <- pow(chena_chum_f_se,-2)
    t5 <- pow(salcha_chin_m_se,-2)
    t6 <- pow(salcha_chin_f_se,-2)
    t7 <- pow(salcha_chum_m_se,-2)
    t8 <- pow(salcha_chum_f_se,-2)
    
    # N.chum <- sum(species[]) - n.fish       # posterior distributions of the totals of each species
    # N.chin <- (2*n.fish) - sum(species[])
    
    # # Nchum_chena <- sum(species[1:9042]) - nchena
    # # Nchin_chena <- (2*nchena) - sum(species[1:9042])
    # # Nchum_salcha <- sum(species[9043:12361]) - nsalcha
    # # Nchin_salcha <- (2*nsalcha) - sum(species[9043:12361])
    # for(k in 1:ndays) {   ### this is each day, but I dont think I want it after all
    # N.chum.day[k] <- sum(species[ifirst[k]:ilast[k]]) - nfishday[k]
    # N.chin.day[k] <- 2*nfishday[k] - sum(species[ifirst[k]:ilast[k]])
    # }
    # # N.chum.before <- sum(species[9693:9704]) + sum(species[9712:9717]) - 18
    # # N.chin.before <- 2*18 - sum(species[9693:9704]) - sum(species[9712:9717])
    # # N.chum.after <- sum(species[9705:9711]) + sum(species[9718:9719]) - 9
    # # N.chin.after <- 2*9 - sum(species[9705:9711]) - sum(species[9718:9719])
    
    }', file="mixl_logitd_3.jags")



mixl_logitd_3.data <- list(n.fish=nrow(all_fish),alpha.sex.chin=c(5,5),alpha.sex.chum=c(5,5),day=days1.205,
                           L.mm.D=all_fish$length,sonar=as.numeric(as.factor(all_fish$sonartype)),
                           river=as.numeric(as.factor(all_fish$river)),
                           nchena=sum(all_fish$river=="Chena"),nsalcha=sum(all_fish$river=="Salcha"))#,






## adding prior values to the input bundle

mixl_logitd_3.data$chena_chin_m_mn <- prior_mn[1] # 630.6527 # 628.9737
mixl_logitd_3.data$chena_chin_f_mn <- prior_mn[2] # 772.8799 # 770.6395
mixl_logitd_3.data$chena_chum_m_mn <- prior_mn[3] # 577.0627 # 576.696
mixl_logitd_3.data$chena_chum_f_mn <- prior_mn[4] # 536.9237 # 536.3548
mixl_logitd_3.data$salcha_chin_m_mn <- prior_mn[5] # 633.8789 # 630.8786
mixl_logitd_3.data$salcha_chin_f_mn <- prior_mn[6] # 748.5821 # 746.3704
mixl_logitd_3.data$salcha_chum_m_mn <- prior_mn[7] # 582.6091 # 581.6707
mixl_logitd_3.data$salcha_chum_f_mn <- prior_mn[8] # 553.7088 # 553.427
mixl_logitd_3.data$chena_chin_m_sd_mn <- prior_sd_mn[1] # 125.3323 # 125.2858
mixl_logitd_3.data$chena_chin_f_sd_mn <- prior_sd_mn[2] # 62.50772 # 62.47632
mixl_logitd_3.data$chena_chum_m_sd_mn <- prior_sd_mn[3] # 32.84721 # 32.79985
mixl_logitd_3.data$chena_chum_f_sd_mn <- prior_sd_mn[4] # 28.85259 # 28.79817
mixl_logitd_3.data$salcha_chin_m_sd_mn <- prior_sd_mn[5] # 140.101 # 140.0916
mixl_logitd_3.data$salcha_chin_f_sd_mn <- prior_sd_mn[6] # 70.46991 # 70.47269
mixl_logitd_3.data$salcha_chum_m_sd_mn <- prior_sd_mn[7] # 36.42222 # 36.39309
mixl_logitd_3.data$salcha_chum_f_sd_mn <- prior_sd_mn[8] # 32.35963 # 32.34815
mixl_logitd_3.data$chena_chin_m_se <- prior_se[1] # 7.28284 # 4.560142
mixl_logitd_3.data$chena_chin_f_se <- prior_se[2] # 6.749164 # 7.089904
mixl_logitd_3.data$chena_chum_m_se <- prior_se[3] # 2.927705 # 2.974368
mixl_logitd_3.data$chena_chum_f_se <- prior_se[4] # 2.985835 # 3.109489
mixl_logitd_3.data$salcha_chin_m_se <- prior_se[5] # 7.706633 # 4.569675
mixl_logitd_3.data$salcha_chin_f_se <- prior_se[6] # 5.077828 # 6.689336
mixl_logitd_3.data$salcha_chum_m_se <- prior_se[7] # 3.769341 # 3.454878
mixl_logitd_3.data$salcha_chum_f_se <- prior_se[8] # 3.383679 # 2.76787
mixl_logitd_3.data$chena_chin_m_sd_sd <- prior_sd_sd[1] # 0.8679354
mixl_logitd_3.data$chena_chin_f_sd_sd <- prior_sd_sd[2] # 0.4953951
mixl_logitd_3.data$chena_chum_m_sd_sd <- prior_sd_sd[3] # 0.8202903
mixl_logitd_3.data$chena_chum_f_sd_sd <- prior_sd_sd[4] # 0.7246442
mixl_logitd_3.data$salcha_chin_m_sd_sd <- prior_sd_sd[5] # 1.044187
mixl_logitd_3.data$salcha_chin_f_sd_sd <- prior_sd_sd[6] # 0.6431075
mixl_logitd_3.data$salcha_chum_m_sd_sd <- prior_sd_sd[7] # 0.6287916
mixl_logitd_3.data$salcha_chum_f_sd_sd <- prior_sd_sd[8] # 0.5613359



# what kind of lengths do we expect??
# nsim <- 10000
# simdf <- matrix(NA, nrow=nsim, ncol=8)
# for(i in 1:8) {
#   mn <- rnorm(nsim, prior_mn[i], prior_se[i])
#   sd <- rnorm(nsim, prior_sd_mn[i], prior_sd_sd[i])
#   simdf[,i] <- rnorm(nsim, mn, sd)
# }
# boxplot(simdf)
# abline(h=500, lty=2)
# apply(simdf, 2, function(x) mean(x<500))


## measured length vs true length regression ests (don't think this is currently used)
mixl_logitd_3.data$betaD0_mn <- 44.81988
mixl_logitd_3.data$betaD1_mn <- 0.87156
mixl_logitd_3.data$betaD0_prec <- 63.36818^(-2)
mixl_logitd_3.data$betaD1_prec <- 0.07835^(-2)



## Recentering(ish) the length data!  This gets messy but helps convergence.

mn_length <- mean(all_fish$length)

mixl_logitd_3.data$chena_chin_m_mn <- mixl_logitd_3.data$chena_chin_m_mn - mn_length
mixl_logitd_3.data$chena_chin_f_mn <- mixl_logitd_3.data$chena_chin_f_mn  - mn_length
mixl_logitd_3.data$chena_chum_m_mn <- mixl_logitd_3.data$chena_chum_m_mn  - mn_length
mixl_logitd_3.data$chena_chum_f_mn <- mixl_logitd_3.data$chena_chum_f_mn  - mn_length
mixl_logitd_3.data$salcha_chin_m_mn <- mixl_logitd_3.data$salcha_chin_m_mn  - mn_length
mixl_logitd_3.data$salcha_chin_f_mn <- mixl_logitd_3.data$salcha_chin_f_mn  - mn_length
mixl_logitd_3.data$salcha_chum_m_mn <- mixl_logitd_3.data$salcha_chum_m_mn  - mn_length
mixl_logitd_3.data$salcha_chum_f_mn <- mixl_logitd_3.data$salcha_chum_f_mn - mn_length



## updating the measured vs true length regression with centered lengths

# summary(lm(dl~al))
al_c <- al - mn_length
c_mod <- lm(dl~al_c)
# summary(c_mod)
c_mod_coef <- summary(c_mod)$coefficients

mixl_logitd_3.data$betaD0_mn <- c_mod_coef[1,1] # 585.15589
mixl_logitd_3.data$betaD1_mn <- c_mod_coef[2,1] # 0.87156
mixl_logitd_3.data$betaD0_prec <- c_mod_coef[1,2]^(-2) # 18.83218
mixl_logitd_3.data$betaD1_prec <- c_mod_coef[2,2]^(-2) # 0.07835



# differential run-timing priors

mixl_logitd_3.data$mu.b0 <- a0[nrow(a0),] # c(-1.977449,-3.241939)
mixl_logitd_3.data$mu.b1 <- a1[nrow(a1),] # c(-0.2657377,-0.3761800)

mixl_logitd_3.data_alt <- mixl_logitd_3.data
mixl_logitd_3.data_alt$mu.b0 <- a0_alt[nrow(a0_alt),] # c(-1.977449,-3.241939)
mixl_logitd_3.data_alt$mu.b1 <- a1_alt[nrow(a1_alt),] # c(-0.2657377,-0.3761800)



## these nodes needed initial values at one point, I think recentering the lengths may have fixed it??
# someinits <- function() list(betaD0=rnorm(2,-190,5),betaD1=rnorm(2,1.4,0.1))



## actually running the thing!!

niter <- 500000  # 2000 takes 9 min in parallel on the laptop, 4000 takes 14.3 min:   3.44 min + 2.73/thousand
                # 50k takes 2.3 hours, 200k takes 8.9 hours, 500k takes 22ish

.1 + .000044*niter  # anticipated number of hours...

t.start <- Sys.time()
print(t.start)
ncores <- 3
mixl_logitd_2.jags.out3 <- jagsUI::jags(model.file="mixl_logitd_3.jags",data=mixl_logitd_3.data, 
                                        parameters.to.save=c("N.chum.day","N.chin.day","lambda","betaD0","betaD1","N.chin","N.chum","sig","species","L.mm.act","sex","prec"),
                                        n.chains=ncores, parallel = T, n.iter=niter, n.burnin=niter/2, n.thin=niter/2000, n.adapt=niter/10) # inits=someinits, 


# ,"sex","pi"
# mixl_logitd_2.jags.out3 <- jags(model.file="jiaqi.jags",data=mixl_logitd_3.data, parameters.to.save=c("N.chum.day","N.chin.day","L.mm.act","species","sex","betaD0","betaD1","lambda","N.chin","N.chum","sig","Nchum_chena","Nchin_chena","Nchum_salcha","Nchin_salcha","N.chin.before","N.chin.after","N.chum.before","N.chum.after"), 
#                                 n.chains=3, n.iter=n.iter)
time <- Sys.time()-t.start
print(time)

df_2019 <- as.data.frame(as.matrix(mixl_logitd_2.jags.out3$samples))
# save(df_2019, file="df_2019a.Rdata")
# load(file="df_2019a.Rdata")


mixl_logitd_2.jags.out3_alt <- jagsUI::jags(model.file="mixl_logitd_3.jags",data=mixl_logitd_3.data_alt, 
                                            parameters.to.save=c("N.chum.day","N.chin.day","lambda","betaD0","betaD1","N.chin","N.chum","sig","species","L.mm.act","sex","prec"),
                                            n.chains=ncores, parallel = T, n.iter=niter, n.burnin=niter/2, n.thin=niter/2000, n.adapt=niter/10) # inits=someinits, 


df_2019_alt <- as.data.frame(as.matrix(mixl_logitd_2.jags.out3_alt$samples))
# save(df_2019, df_2019_alt, file="df_2019.Rdata")

mixl_logitd_2.jags.out3$DIC      # 65964.82    - model using Salcha data as-is wins
mixl_logitd_2.jags.out3_alt$DIC  # 66647.61



## playing with diagnostics

hist(unlist(mixl_logitd_2.jags.out3$Rhat))
sum(unlist(mixl_logitd_2.jags.out3$Rhat)>1.1, na.rm=T) 
sapply(mixl_logitd_2.jags.out3$Rhat, function(x) sum(x>1.1, na.rm=T))


hist(unlist(mixl_logitd_2.jags.out3$n.eff))
sapply(mixl_logitd_2.jags.out3$n.eff, function(x) mean(x<500, na.rm=T))


hist(unlist(mixl_logitd_2.jags.out3_alt$Rhat))
sum(unlist(mixl_logitd_2.jags.out3_alt$Rhat)>1.1, na.rm=T) 
sapply(mixl_logitd_2.jags.out3_alt$Rhat, function(x) sum(x>1.1, na.rm=T))


hist(unlist(mixl_logitd_2.jags.out3_alt$n.eff))
sapply(mixl_logitd_2.jags.out3_alt$n.eff, function(x) mean(x<500, na.rm=T))




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

# a function to pull a subset of columns from a data.frame based on the column names
pull_post <- function(x, p) x[,substr(names(x),1,nchar(p))==p]  



## trace plot greatest hits

par(mfrow=c(2,2))
trace_df(pull_post(df_2019, "beta"))
par(mfrow=c(4,2))
trace_df(pull_post(df_2019, "lambda"))
trace_df(pull_post(df_2019, "prec"))

par(mfrow=c(2,2))
trace_df(pull_post(df_2019_alt, "beta"))
par(mfrow=c(4,2))
trace_df(pull_post(df_2019_alt, "lambda"))
trace_df(pull_post(df_2019_alt, "prec"))



## if sex & species are the only offenders, this isn't very informative

tracethebadRhat <- function(jagsout,thresh=1.1) {
  Rhatvec <- unlist(jagsout$Rhat)
  theseones <- Rhatvec > thresh
  theseones[is.na(theseones)] <- F
  df <- as.data.frame(as.matrix(jagsout$samples))
  trace_df(df[,theseones])
}
summary(unlist(mixl_logitd_2.jags.out3_alt$Rhat) > 1.2)
par(mfrow=c(5,4))
tracethebadRhat(jagsout=mixl_logitd_2.jags.out3_alt, thresh=1.2)

tracethebadneff <- function(thresh=500) {
  neffvec <- unlist(jagsout$n.eff)
  theseones <- neffvec < thresh
  theseones[is.na(theseones)] <- F
  df <- as.data.frame(as.matrix(jagsout$samples))
  trace_df(df[,theseones])
}
summary(unlist(mixl_logitd_2.jags.out3_alt$n.eff) < 50)
# tracethebadneff(jagsout=mixl_logitd_2.jags.out3_alt, 50)