#################################
#
#   4. The mixture model!
#
#################################

library(jagsUI)
library(jagshelper)

load(file="2024/Rdata/CSpriors2024.Rdata")
load(file="2024/Rdata/sonardata2024.Rdata")

# all_fish <- subset(all_sonar, length>=400)


## recentering(ish) the dates

days1.205 <- all_fish$date - as.numeric(as.Date("2023-12-31",format="%Y-%m-%d")) - 205
days1.205 <- as.numeric(days1.205)

## sonar-measured lengths and actual lengths from the tethered-fish experiment 
## This was once used directly in the JAGS model but now runs in an lm() outside to simplify 
dl <- c(632,602,1049,664,768,663,1025,685,681,957,953,747,646,666,627,531,584)
al <- c(740,600,1170,710,950,650,1020,665,760,1040,970,840,700,700,620,620,690)



## behold the beast
CSmix_reg <- tempfile()
cat('model {
    for(i in 1:n.fish) {
      L.mm.D[i] ~ dnorm(muL[i],precL*precfac)   #######################
      ypp[i] ~ dnorm(muL[i],precL*precfac)
      muL[i] <- betaD0[sonar[i]] + betaD1[sonar[i]]*L.mm.act[i]
      L.mm.act[i] ~ dnorm(mu[i],tau[i])
      
      # L.mm.D[i] ~ dnorm(mu[i],tau[i])   #######################
      # ypp[i] ~ dnorm(mu[i],tau[i])   #######################
      
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

    # moved this whole piece outside the model  -- MIGHT BE BETTER IN MODEL
    for(j in 1:m) {
      DL.star[j] ~ dnorm(mu.star[j],precL)
      mu.star[j] <- betaD0[1] + betaD1[1]*AL.star[j]         ########
    }
    # # precL <- 1/(54.59*54.59)
    # betaD0[1] ~ dnorm(0,0.0001)         ########
    # betaD1[1] ~ dnorm(1,0.0001)         ########
    # betaD0[2] ~ dnorm(0,0.0001)         ########
    # betaD1[2] ~ dnorm(1,0.0001)         ########
    betaD0[1] ~ dnorm(betaD0_mn,0.0001)         ########
    betaD1[1] ~ dnorm(betaD1_mn,0.1)         ########
    betaD0[2] ~ dnorm(betaD0_mn,0.0001)         ########
    betaD1[2] ~ dnorm(betaD1_mn,0.1)         ########
    # prec.star ~ dunif(0.0001,1000)
    # sig.star <- 1/sqrt(prec.star)
    sig.star ~ dunif(0, 100)
    precL <- pow(sig.star, -2)

    # betaD0[1] ~ dnorm(betaD0_mn,betaD0_prec)         ########
    # betaD1[1] ~ dnorm(betaD1_mn,betaD1_prec)         ########
    # betaD0[2] ~ dnorm(betaD0_mn,betaD0_prec)         ########
    # betaD1[2] ~ dnorm(betaD1_mn,betaD1_prec)         ########
    # # precbD0 <- 1/betaD0_se/betaD0_se         ########
    # # precbD1 <- 1/betaD1_se/betaD1_se         ########

    b0 <- mu.b0 # ~ dnorm(mu.b0,prec.b0) # # # # # # # # #
    b1 <- mu.b1 # ~ dnorm(mu.b1,prec.b1) # # # # # # # # #

    # b0 ~ dmnorm(mu.b0[],tau.b0[,])
    # b1 ~ dmnorm(mu.b1[],tau.b1[,])
    # tau.b0[1:2,1:2] <- inverse(sig.b0[,])
    # tau.b1[1:2,1:2] <- inverse(sig.b1[,])
    # b0[1] ~ dnorm(mu.b0[1],prec.b0[1]) # # # # # # # # #  this fails with
    # b0[2] ~ dnorm(mu.b0[2],prec.b0[2]) # # # # # # # # #  the alt model
    # b1[1] ~ dnorm(mu.b1[1],prec.b1[1]) # # # # # # # # #
    # b1[2] ~ dnorm(mu.b1[2],prec.b1[2]) # # # # # # # # #

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
    
    }', file=CSmix_reg)

CSmix_base <- tempfile()
cat('model {
    for(i in 1:n.fish) {
      L.mm.D[i] ~ dnorm(muL[i],precL_data*precfac)   #######################
      ypp[i] ~ dnorm(muL[i],precL_data*precfac)
      muL[i] <- betaD0[sonar[i]] + betaD1[sonar[i]]*L.mm.act[i]
      L.mm.act[i] ~ dnorm(mu[i],tau[i])
      
      # L.mm.D[i] ~ dnorm(mu[i],tau[i])   #######################
      # ypp[i] ~ dnorm(mu[i],tau[i])   #######################
      
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

    # # moved this whole piece outside the model  -- MIGHT BE BETTER IN MODEL
    # for(j in 1:m) {
    #   DL.star[j] ~ dnorm(mu.star[j],precL)
    #   mu.star[j] <- betaD0[1] + betaD1[1]*AL.star[j]         ########
    # }
    # # # precL <- 1/(54.59*54.59)
    # # betaD0[1] ~ dnorm(0,0.0001)         ########
    # # betaD1[1] ~ dnorm(1,0.0001)         ########
    # # betaD0[2] ~ dnorm(0,0.0001)         ########
    # # betaD1[2] ~ dnorm(1,0.0001)         ########
    # betaD0[1] ~ dnorm(betaD0_mn,0.0001)         ########
    # betaD1[1] ~ dnorm(betaD1_mn,0.1)         ########
    # betaD0[2] ~ dnorm(betaD0_mn,0.0001)         ########
    # betaD1[2] ~ dnorm(betaD1_mn,0.1)         ########
    # # prec.star ~ dunif(0.0001,1000)
    # # sig.star <- 1/sqrt(prec.star)
    # sig.star ~ dunif(0, 100)
    # precL <- pow(sig.star, -2)

    betaD0[1] ~ dnorm(betaD0_mn,betaD0_prec)         ########
    betaD1[1] ~ dnorm(betaD1_mn,betaD1_prec)         ########
    betaD0[2] ~ dnorm(betaD0_mn,betaD0_prec)         ########
    betaD1[2] ~ dnorm(betaD1_mn,betaD1_prec)         ########
    # precbD0 <- 1/betaD0_se/betaD0_se         ########
    # precbD1 <- 1/betaD1_se/betaD1_se         ########

    b0 <- mu.b0 # ~ dnorm(mu.b0,prec.b0) # # # # # # # # #
    b1 <- mu.b1 # ~ dnorm(mu.b1,prec.b1) # # # # # # # # #

    # b0 ~ dmnorm(mu.b0[],tau.b0[,])
    # b1 ~ dmnorm(mu.b1[],tau.b1[,])
    # tau.b0[1:2,1:2] <- inverse(sig.b0[,])
    # tau.b1[1:2,1:2] <- inverse(sig.b1[,])
    # b0[1] ~ dnorm(mu.b0[1],prec.b0[1]) # # # # # # # # #  this fails with
    # b0[2] ~ dnorm(mu.b0[2],prec.b0[2]) # # # # # # # # #  the alt model
    # b1[1] ~ dnorm(mu.b1[1],prec.b1[1]) # # # # # # # # #
    # b1[2] ~ dnorm(mu.b1[2],prec.b1[2]) # # # # # # # # #

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
    
    }', file=CSmix_base)


{
  CSmix_data <- list(n.fish=nrow(all_fish),
                     alpha.sex.chin=c(5,5),
                     alpha.sex.chum=c(5,5),
                     day=days1.205,
                     L.mm.D=all_fish$length,
                     # L.mm.D=all_fish$length - mean(all_fish$length),
                     DL.star=dl,
                     AL.star=al - mean(all_fish$length),
                     m=length(dl),
                     sonar=as.numeric(factor(all_fish$sonartype,
                                             levels=c("ARIS","DIDSON"))),
                     river=as.numeric(factor(all_fish$river,
                                             levels=c("Chena","Salcha"))))
  
  
  
  
  
  
  ## adding prior values to the input bundle
  
  CSmix_data$chena_chin_m_mn <- prior_mn[1] # 630.6527 # 628.9737
  CSmix_data$chena_chin_f_mn <- prior_mn[2] # 772.8799 # 770.6395
  CSmix_data$chena_chum_m_mn <- prior_mn[3] # 577.0627 # 576.696
  CSmix_data$chena_chum_f_mn <- prior_mn[4] # 536.9237 # 536.3548
  CSmix_data$salcha_chin_m_mn <- prior_mn[5] # 633.8789 # 630.8786
  CSmix_data$salcha_chin_f_mn <- prior_mn[6] # 748.5821 # 746.3704
  CSmix_data$salcha_chum_m_mn <- prior_mn[7] # 582.6091 # 581.6707
  CSmix_data$salcha_chum_f_mn <- prior_mn[8] # 553.7088 # 553.427
  CSmix_data$chena_chin_m_sd_mn <- prior_sd_mn[1] # 125.3323 # 125.2858
  CSmix_data$chena_chin_f_sd_mn <- prior_sd_mn[2] # 62.50772 # 62.47632
  CSmix_data$chena_chum_m_sd_mn <- prior_sd_mn[3] # 32.84721 # 32.79985
  CSmix_data$chena_chum_f_sd_mn <- prior_sd_mn[4] # 28.85259 # 28.79817
  CSmix_data$salcha_chin_m_sd_mn <- prior_sd_mn[5] # 140.101 # 140.0916
  CSmix_data$salcha_chin_f_sd_mn <- prior_sd_mn[6] # 70.46991 # 70.47269
  CSmix_data$salcha_chum_m_sd_mn <- prior_sd_mn[7] # 36.42222 # 36.39309
  CSmix_data$salcha_chum_f_sd_mn <- prior_sd_mn[8] # 32.35963 # 32.34815
  CSmix_data$chena_chin_m_se <- prior_se[1] # 7.28284 # 4.560142
  CSmix_data$chena_chin_f_se <- prior_se[2] # 6.749164 # 7.089904
  CSmix_data$chena_chum_m_se <- prior_se[3] # 2.927705 # 2.974368
  CSmix_data$chena_chum_f_se <- prior_se[4] # 2.985835 # 3.109489
  CSmix_data$salcha_chin_m_se <- prior_se[5] # 7.706633 # 4.569675
  CSmix_data$salcha_chin_f_se <- prior_se[6] # 5.077828 # 6.689336
  CSmix_data$salcha_chum_m_se <- prior_se[7] # 3.769341 # 3.454878
  CSmix_data$salcha_chum_f_se <- prior_se[8] # 3.383679 # 2.76787
  CSmix_data$chena_chin_m_sd_sd <- prior_sd_sd[1] # 0.8679354
  CSmix_data$chena_chin_f_sd_sd <- prior_sd_sd[2] # 0.4953951
  CSmix_data$chena_chum_m_sd_sd <- prior_sd_sd[3] # 0.8202903
  CSmix_data$chena_chum_f_sd_sd <- prior_sd_sd[4] # 0.7246442
  CSmix_data$salcha_chin_m_sd_sd <- prior_sd_sd[5] # 1.044187
  CSmix_data$salcha_chin_f_sd_sd <- prior_sd_sd[6] # 0.6431075
  CSmix_data$salcha_chum_m_sd_sd <- prior_sd_sd[7] # 0.6287916
  CSmix_data$salcha_chum_f_sd_sd <- prior_sd_sd[8] # 0.5613359
  
  
  
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
  
  
  # ## measured length vs true length regression ests (don't think this is currently used)
  # CSmix_data$betaD0_mn <- 44.81988
  # CSmix_data$betaD1_mn <- 0.87156
  # CSmix_data$betaD0_prec <- 63.36818^(-2)
  # CSmix_data$betaD1_prec <- 0.07835^(-2)
  
  
  
  ## Recentering(ish) the length data!  This gets messy but helps convergence.
  
  mn_length <- mean(all_fish$length)
  
  CSmix_data$chena_chin_m_mn <- CSmix_data$chena_chin_m_mn - mn_length
  CSmix_data$chena_chin_f_mn <- CSmix_data$chena_chin_f_mn  - mn_length
  CSmix_data$chena_chum_m_mn <- CSmix_data$chena_chum_m_mn  - mn_length
  CSmix_data$chena_chum_f_mn <- CSmix_data$chena_chum_f_mn  - mn_length
  CSmix_data$salcha_chin_m_mn <- CSmix_data$salcha_chin_m_mn  - mn_length
  CSmix_data$salcha_chin_f_mn <- CSmix_data$salcha_chin_f_mn  - mn_length
  CSmix_data$salcha_chum_m_mn <- CSmix_data$salcha_chum_m_mn  - mn_length
  CSmix_data$salcha_chum_f_mn <- CSmix_data$salcha_chum_f_mn - mn_length
  
  
  
  ## updating the measured vs true length regression with centered lengths
  
  # summary(lm(dl~al))
  al_c <- al - mn_length
  c_mod <- lm(dl~al_c)
  # summary(c_mod)
  c_mod_coef <- summary(c_mod)$coefficients
  
  CSmix_data$betaD0_mn <- c_mod_coef[1,1] # 585.15589
  CSmix_data$betaD1_mn <- c_mod_coef[2,1] # 0.87156
  CSmix_data$betaD0_prec <- c_mod_coef[1,2]^(-2) # 18.83218
  CSmix_data$betaD1_prec <- c_mod_coef[2,2]^(-2) # 0.07835
  CSmix_data$precL_data <- summary(c_mod)$sigma^(-2)
  
  
  # differential run-timing priors
  
  CSmix_data$mu.b0 <- a0[nrow(a0),] 
  CSmix_data$mu.b1 <- a1[nrow(a1),] 
  CSmix_data$prec.b0 <- a0_prec[nrow(a0),] 
  CSmix_data$prec.b1 <- a1_prec[nrow(a1),] 
  
  CSmix_data_alt <- CSmix_data
  CSmix_data_alt$mu.b0 <- a0_alt[nrow(a0_alt),] 
  CSmix_data_alt$mu.b1 <- a1_alt[nrow(a1_alt),] 
  CSmix_data_alt$prec.b0 <- a0_prec_alt[nrow(a0_alt),] 
  CSmix_data_alt$prec.b1 <- a1_prec_alt[nrow(a1_alt),] 
}


## actually running the thing!!

niter <- 20*1000  ##500000 
ncores <- 10 
# 50k now takes 7 min

allouts <- list()
params <- c("lambda","L.mm.act","betaD0","betaD1", "sig.star", "species","sex",
            "ps", "psex","b0","b1","prec", "ypp")
{
  t.start <- Sys.time()
  print(t.start)
  
  # base data
  # precfac=1
  # regression outside model (base)
  CSmix_data$precfac <- 1
  allouts[[1]] <- jagsUI::jags(model.file=CSmix_base,data=CSmix_data, 
                                 parameters.to.save=params,
                                 n.chains=ncores, parallel = T, n.iter=niter, n.burnin=niter/2, n.thin=niter/2000, n.adapt=niter/10) # inits=someinits, 
  
  time <- Sys.time()-t.start
  print(time)
}

{
# base data
# precfac=1
# regression inside model (reg)
CSmix_data$precfac <- 1
allouts[[2]] <- jagsUI::jags(model.file=CSmix_reg,data=CSmix_data, 
                             parameters.to.save=params,
                             n.chains=ncores, parallel = T, n.iter=niter, n.burnin=niter/2, n.thin=niter/2000, n.adapt=niter/10) # inits=someinits, 


# base data
# precfac=9
# regression outside model (base)
CSmix_data$precfac <- 9
allouts[[3]] <- jagsUI::jags(model.file=CSmix_base,data=CSmix_data, 
                             parameters.to.save=params,
                             n.chains=ncores, parallel = T, n.iter=niter, n.burnin=niter/2, n.thin=niter/2000, n.adapt=niter/10) # inits=someinits, 


# base data
# precfac=9
# regression inside model (reg)
CSmix_data$precfac <- 9
allouts[[4]] <- jagsUI::jags(model.file=CSmix_reg,data=CSmix_data, 
                             parameters.to.save=params,
                             n.chains=ncores, parallel = T, n.iter=niter, n.burnin=niter/2, n.thin=niter/2000, n.adapt=niter/10) # inits=someinits, 

# alt data
# precfac=1
# regression outside model (base)
CSmix_data_alt$precfac <- 1
allouts[[5]] <- jagsUI::jags(model.file=CSmix_base,data=CSmix_data_alt, 
                             parameters.to.save=params,
                             n.chains=ncores, parallel = T, n.iter=niter, n.burnin=niter/2, n.thin=niter/2000, n.adapt=niter/10) # inits=someinits, 


# alt data
# precfac=1
# regression inside model (reg)
CSmix_data_alt$precfac <- 1
allouts[[6]] <- jagsUI::jags(model.file=CSmix_reg,data=CSmix_data_alt, 
                             parameters.to.save=params,
                             n.chains=ncores, parallel = T, n.iter=niter, n.burnin=niter/2, n.thin=niter/2000, n.adapt=niter/10) # inits=someinits, 


# alt data
# precfac=9
# regression outside model (base)
CSmix_data_alt$precfac <- 9
allouts[[7]] <- jagsUI::jags(model.file=CSmix_base,data=CSmix_data_alt, 
                             parameters.to.save=params,
                             n.chains=ncores, parallel = T, n.iter=niter, n.burnin=niter/2, n.thin=niter/2000, n.adapt=niter/10) # inits=someinits, 


# alt data
# precfac=9
# regression inside model (reg)
CSmix_data_alt$precfac <- 9
allouts[[8]] <- jagsUI::jags(model.file=CSmix_reg,data=CSmix_data_alt, 
                             parameters.to.save=params,
                             n.chains=ncores, parallel = T, n.iter=niter, n.burnin=niter/2, n.thin=niter/2000, n.adapt=niter/10) # inits=someinits, 
}

par(mfrow=c(3,3))
for(i in 1:length(allouts)) plotRhats(allouts[[i]])

par(mfrow=c(3,3))
for(i in 1:length(allouts)) qq_postpred(allouts[[i]], p="ypp", y=CSmix_data$L.mm.D)

par(mfrow=c(3,3))
for(i in 1:length(allouts)) {
  plot(CSmix_data$L.mm.D, allouts[[i]]$q50$ypp)
  abline(0,1)
}

theDICs <- sapply(allouts, \(x) x$DIC)
theDICs-min(theDICs)
plot(theDICs-min(theDICs))
theDICs[c(1,3,5,7)] - min(theDICs[c(1,3,5,7)])
theDICs[c(2,4,6,8)] - min(theDICs[c(2,4,6,8)])

par(mfrow=c(2,2))
chincounts <- sapply(allouts, \(x) rowSums(x$sims.list$species==1))
chumcounts <- sapply(allouts, \(x) rowSums(x$sims.list$species==2))
caterpillar(chincounts, main="Chinook counts - all sonar")
caterpillar(chumcounts, main="chum counts - all sonar")

chincounts_sub <- sapply(allouts, \(x) rowSums(x$sims.list$species[,all_fish$date >= as.Date("2024-07-06") & all_fish$date <= as.Date("2024-07-10")]==1))
chumcounts_sub <- sapply(allouts, \(x) rowSums(x$sims.list$species[,all_fish$date >= as.Date("2024-07-06") & all_fish$date <= as.Date("2024-07-10")]==2))
caterpillar(chincounts_sub, main="Chinook counts - July 6-10")
caterpillar(chumcounts_sub, main="chum counts - July 6-10")
