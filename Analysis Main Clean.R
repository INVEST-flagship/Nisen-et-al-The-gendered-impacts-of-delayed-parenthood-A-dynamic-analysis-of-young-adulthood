
# The R-code provided on this repository falls under GNU
# GENERAL PUBLIC LICENSE (detailed below).  
#
# GNU GENERAL PUBLIC LICENSE
# Version 3, 29 June 2007
#
# Copyright (C) 2007 Free Software Foundation, Inc. <https://fsf.org/>
#   Everyone is permitted to copy and distribute verbatim copies
#   of this license document, with due reference to the authors,
#   but changing it is not allowed.
#   for full authoritative version, see fsf.org

# make model formulas
formula.outcome <- c("outcome ~ 
                     
                     ## constants ##
                     
                     momafb +
                     
                     momnsib +
                     
                     momorder +
                     
                     famtype_1 +
                     
                     # urban.urban +
                     urban.semi + urban.rural +
                     
                     # famtype.married +
                     famtype.cohab + famtype.lone +
                     famtype.notwithpar +
                     
                     pedu.pri + pedu.sec +
                     # pedu.tertlow + 
                     pedu.terthi +
                     
                     punempl +
                     
                     phouse.owner +
                     
                     phhinc.1 + phhinc.2 +
                     # phhinc.3 +
                     phhinc.4 + phhinc.5 +
                     
                     ## time-varying variables ##
                     
                     # age.1619 + 
                     (age.2023 + age.2427 +
                     age.2830 + age.3133) *
                     
                     (l.edu.pri + l.edu.sec +
                     # l.edu.tertlow +
                     l.edu.terthi +
                     
                     l.enrol +
                     
                     l.enrolc) +
                     
                     # age.1619 + 
                     (age.2023 + age.2427 +
                     age.2830 + age.3133) * l.parent +
                     
                     l.entryc +
                     
                     # l.partner.single +
                     l.partner.cohab + l.partner.married +
                     
                     l.empl +
                     
                     l.cumu.empl +
                     
                     l.unempl +
                     
                     l.inc +
                     
                     l.cumu.inc +
                     
                     l.inc.res + 
                     
                     # l.house.parents +
                     l.house.owner + l.house.tenant +
                     l.house.other +
                     
                     l.birth +
                     
                     tsfb 
                     ")

# formula.edu is used for education outcomes
formula.edu <- c("outcome ~ 
                 
                 ## constants ##
                 
                 momafb +
                 
                 momnsib +
                 
                 momorder +
                 
                 famtype_1 +
                 
                 # urban.urban +
                 urban.semi + urban.rural +
                 
                 # famtype.married +
                 famtype.cohab + famtype.lone +
                 famtype.notwithpar +
                 
                 pedu.pri + pedu.sec +
                 # pedu.tertlow + 
                 pedu.terthi +
                 
                 punempl +
                 
                 phouse.owner +
                 
                 phhinc.1 + phhinc.2 +
                 # phhinc.3 +
                 phhinc.4 + phhinc.5 +
                 
                 ## time-varying variables ##
                 
                 # age.1619 + 
                 (age.2023 + age.2427 +
                 age.2830 + age.3133) *
                 
                 (l.edu.pri + l.edu.sec +
                 # l.edu.tertlow + 
                 l.edu.terthi +
                 
                 enrol +
                 
                 l.enrol +
                 
                 l.enrolc) +
                 
                 # age.1619 + 
                 (age.2023 + age.2427 +
                 age.2830 + age.3133) * l.parent +
                 
                 l.entryc +
                 
                 # l.partner.single +
                 l.partner.cohab + l.partner.married +
                 
                 l.empl +
                 
                 l.cumu.empl +
                 
                 l.unempl +
                 
                 l.inc +
                 
                 l.cumu.inc +
                 
                 l.inc.res + 
                 
                 # l.house.parents +
                 l.house.owner + l.house.tenant +
                 l.house.other +
                 
                 l.birth +
                 
                 tsfb 
                 ")

formula.birth <- as.formula(sub('outcome','birth',formula.outcome))
formula.edu.pri <- as.formula(sub('outcome','edu.pri',formula.edu))
formula.edu.sec <- as.formula(sub('outcome','edu.sec',formula.edu))
formula.edu.tertlowst <- as.formula(sub('outcome','edu.tertlowst',formula.edu))
formula.edu.tertlow <- as.formula(sub('outcome','edu.tertlow',formula.edu))
formula.edu.terthi <- as.formula(sub('outcome','edu.terthi',formula.edu))
formula.enrol <- as.formula(sub('outcome','enrol',formula.outcome))
formula.partner.single <- as.formula(sub('outcome','partner.single',formula.outcome))
formula.partner.cohab <- as.formula(sub('outcome','partner.cohab',formula.outcome))
formula.partner.married <- as.formula(sub('outcome','partner.married',formula.outcome))
formula.empl <- as.formula(sub('outcome','empl',formula.outcome))
formula.unempl <- as.formula(sub('outcome','unempl',formula.outcome))
formula.inc <- as.formula(sub('outcome','inc',formula.outcome))
formula.hhinc <- as.formula(sub('outcome','inc.res',formula.outcome))
formula.house.parents <- as.formula(sub('outcome','house.parents',formula.outcome))
formula.house.owner <- as.formula(sub('outcome','house.owner',formula.outcome))
formula.house.tenant <- as.formula(sub('outcome','house.tenant',formula.outcome))
formula.house.other <- as.formula(sub('outcome','house.other',formula.outcome))

# deterministic time varying values:

# age.cent  # deterministically dependent on time
# enrolc  # deterministically dependent on enrol
# parent # deterministically dependent on birth
# tsfb # deterministically dependent on birth and time
# entryc # deterministically dependent on enrol

# predict functions needed
multinomial.predict.5var <- function(predict.x1,predict.x2,predict.x3,
                                     predict.x4,predict.x5,
                                     dataset) {
  x1 <- predict(predict.x1,dataset,type='response')
  x2 <- predict(predict.x2,dataset,type='response')
  x3 <- predict(predict.x3,dataset,type='response')
  x4 <- predict(predict.x4,dataset,type='response')
  x5 <- predict(predict.x5,dataset,type='response')
  
  # find rescale value
  rescaler <- 1/(x1+x2+x3+x4+x5)
  
  # rescale constituent variables
  x1r <- x1*rescaler
  x2r <- x2*rescaler
  x3r <- x3*rescaler
  x4r <- x4*rescaler
  # x5r does not need to be determined
  
  # determine boundary values
  x1b <- x1r
  x2b <- x1r + x2r
  x3b <- x1r + x2r + x3r
  x4b <- x1r + x2r + x3r + x4r
  # x5b does not need to be determined
  
  # determine n
  n2 <- length(x1)
  
  # draw random variables
  decider <- runif(n2,0,1)
  
  # determine category
  x1c <- ifelse(decider <= x1b,1,0)
  x2c <- ifelse(decider > x1b & decider <= x2b,1,0)
  x3c <- ifelse(decider > x2b & decider <= x3b,1,0)
  x4c <- ifelse(decider > x3b & decider <= x4b,1,0)
  x5c <- ifelse(decider > x4b,1,0)
  
  return(cbind(x1c,x2c,x3c,x4c,x5c))
}

multinomial.predict.4var <- function(predict.x1,predict.x2,
                                     predict.x3,predict.x4,
                                     dataset) {
  x1 <- predict(predict.x1,dataset,type='response')
  x2 <- predict(predict.x2,dataset,type='response')
  x3 <- predict(predict.x3,dataset,type='response')
  x4 <- predict(predict.x4,dataset,type='response')
  
  # find rescale value
  rescaler <- 1/(x1+x2+x3+x4)
  
  # rescale constituent variables
  x1r <- x1*rescaler
  x2r <- x2*rescaler
  x3r <- x3*rescaler
  # x4r does not need to be determined
  
  # determine boundary values
  x1b <- x1r
  x2b <- x1r + x2r
  x3b <- x1r + x2r + x3r
  # x4b does not need to be determined
  
  # determine n
  n2 <- length(x1)
  
  # draw random variables
  decider <- runif(n2,0,1)
  
  # determine category
  x1c <- ifelse(decider <= x1b,1,0)
  x2c <- ifelse(decider > x1b & decider <= x2b,1,0)
  x3c <- ifelse(decider > x2b & decider <= x3b,1,0)
  x4c <- ifelse(decider > x3b,1,0)
  
  return(cbind(x1c,x2c,x3c,x4c))
}

multinomial.predict.3var <- function(predict.x1,predict.x2,predict.x3,
                                     dataset) {
  x1 <- predict(predict.x1,dataset,type='response')
  x2 <- predict(predict.x2,dataset,type='response')
  x3 <- predict(predict.x3,dataset,type='response')
  
  # find rescale value
  rescaler <- 1/(x1+x2+x3)
  
  # rescale constituent variables
  x1r <- x1*rescaler
  x2r <- x2*rescaler
  # x3r does not need to be determined
  
  # determine boundary values
  x1b <- x1r
  x2b <- x1r + x2r
  # x3b does not need to be determined
  
  # determine n
  n2 <- length(x1)
  
  # draw random variables
  decider <- runif(n2,0,1)
  
  # determine category
  x1c <- ifelse(decider <= x1b,1,0)
  x2c <- ifelse(decider > x1b & decider <= x2b,1,0)
  x3c <- ifelse(decider > x2b,1,0)
  
  return(cbind(x1c,x2c,x3c))
}

predict.lm.withvar <- function(predict.x1,dataset) {
  
  # determine how many new values have to be created
  n2 <- dim(dataset)[1]
  
  # determine randomness around the mean predicted value
  # I choose normally distributed residuals
  # but I could choose another distribution
  # normal makes sense since LM also assumes normality
  newres <- rnorm(n2,mean=0,sd(predict.x1$residuals))
  
  # make mean predictions
  preds <- predict(predict.x1,dataset,
                   interval='none',type='response')
  
  # add randomness on top and return the values
  return(preds + newres)
  
}

binomial.predict.1var <- function(predict.x1,dataset) {
  
  a <- predict(predict.x1,dataset,link='response')
  p <- exp(a)/(exp(a)+1)
  n2 <- length(a)
  
  return(rbinom(n2,1,p))
  
}


# longitudinal sampling function
long.sample <- function(originaldata, originaldataid) {
  # select a bunch of IDs
  IDs <- unique(originaldataid)
  y <- sample(IDs,length(IDs),replace=T)
  z <- table(table(y))
  
  # from there, select a group once
  selectID <- sample(IDs,size=z[1],replace=F)
  newdata <- originaldata[which(originaldataid %in% selectID),]
  
  if(length(z) > 1) {
    
    for(i in 2:length(z)) {
      
      # select a new group of IDs that was not yet selected
      IDs2 <- setdiff(IDs,selectID)
      
      # from there, randomly select a group of people of the right size
      selectID2 <- sample(IDs2,size=z[i],replace=F)
      selectID <- c(selectID,selectID2) # so we don't re-select the newly
      # selected people either
      
      for(j in 1:i) {
        
        # copy the new dataset i number of times
        newdata <- rbind(newdata,originaldata[which(originaldataid %in% selectID2),])
        
      }
      
    }
    
    return(newdata)
  }
}

# save column location info
col.index.from <- NULL
col.index.from[1] <- grep(paste0('^','birth','$'), colnames(par.dat))
col.index.from[2] <- grep(paste0('^','edu.pri','$'), colnames(par.dat))
col.index.from[3] <- grep(paste0('^','edu.sec','$'), colnames(par.dat))
col.index.from[4] <- grep(paste0('^','edu.tertlowst','$'), colnames(par.dat))
col.index.from[5] <- grep(paste0('^','edu.tertlow','$'), colnames(par.dat))
col.index.from[6] <- grep(paste0('^','edu.terthi','$'), colnames(par.dat))
col.index.from[7] <- grep(paste0('^','enrol','$'), colnames(par.dat))
col.index.from[8] <- grep(paste0('^','enrolc','$'), colnames(par.dat))
col.index.from[9] <- grep(paste0('^','partner.single','$'), colnames(par.dat))
col.index.from[10] <- grep(paste0('^','partner.cohab','$'), colnames(par.dat))
col.index.from[11] <- grep(paste0('^','partner.married','$'), colnames(par.dat))
col.index.from[12] <- grep(paste0('^','empl','$'), colnames(par.dat))
col.index.from[13] <- grep(paste0('^','unempl','$'), colnames(par.dat))
col.index.from[14] <- grep(paste0('^','inc','$'), colnames(par.dat))
col.index.from[15] <- grep(paste0('^','inc.res','$'), colnames(par.dat))
col.index.from[16] <- grep(paste0('^','house.parents','$'), colnames(par.dat))
col.index.from[17] <- grep(paste0('^','house.owner','$'), colnames(par.dat))
col.index.from[18] <- grep(paste0('^','house.tenant','$'), colnames(par.dat))
col.index.from[19] <- grep(paste0('^','house.other','$'), colnames(par.dat))
col.index.from[20] <- grep(paste0('^','parent','$'), colnames(par.dat))
col.index.from[21] <- grep(paste0('^','tsfb','$'), colnames(par.dat))
col.index.from[22] <- grep(paste0('^','postpone','$'), colnames(par.dat))
col.index.from[23] <- grep(paste0('^','entryc','$'), colnames(par.dat))
col.index.from[24] <- grep(paste0('^','cumu.empl','$'), colnames(par.dat))
col.index.from[25] <- grep(paste0('^','cumu.inc','$'), colnames(par.dat))


col.index.to <- NULL
col.index.to[1] <- grep(paste0('^','l.birth','$'), colnames(par.dat))
col.index.to[2] <- grep(paste0('^','l.edu.pri','$'), colnames(par.dat))
col.index.to[3] <- grep(paste0('^','l.edu.sec','$'), colnames(par.dat))
col.index.to[4] <- grep(paste0('^','l.edu.tertlowst','$'), colnames(par.dat))
col.index.to[5] <- grep(paste0('^','l.edu.tertlow','$'), colnames(par.dat))
col.index.to[6] <- grep(paste0('^','l.edu.terthi','$'), colnames(par.dat))
col.index.to[7] <- grep(paste0('^','l.enrol','$'), colnames(par.dat))
col.index.to[8] <- grep(paste0('^','l.enrolc','$'), colnames(par.dat))
col.index.to[9] <- grep(paste0('^','l.partner.single','$'), colnames(par.dat))
col.index.to[10] <- grep(paste0('^','l.partner.cohab','$'), colnames(par.dat))
col.index.to[11] <- grep(paste0('^','l.partner.married','$'), colnames(par.dat))
col.index.to[12] <- grep(paste0('^','l.empl','$'), colnames(par.dat))
col.index.to[13] <- grep(paste0('^','l.unempl','$'), colnames(par.dat))
col.index.to[14] <- grep(paste0('^','l.inc','$'), colnames(par.dat))
col.index.to[15] <- grep(paste0('^','l.inc.res','$'), colnames(par.dat))
col.index.to[16] <- grep(paste0('^','l.house.parents','$'), colnames(par.dat))
col.index.to[17] <- grep(paste0('^','l.house.owner','$'), colnames(par.dat))
col.index.to[18] <- grep(paste0('^','l.house.tenant','$'), colnames(par.dat))
col.index.to[19] <- grep(paste0('^','l.house.other','$'), colnames(par.dat))
col.index.to[20] <- grep(paste0('^','l.parent','$'), colnames(par.dat))
col.index.to[21] <- grep(paste0('^','l.tsfb','$'), colnames(par.dat))
col.index.to[22] <- grep(paste0('^','l.postpone','$'), colnames(par.dat))
col.index.to[23] <- grep(paste0('^','l.entryc','$'), colnames(par.dat))
col.index.to[24] <- grep(paste0('^','l.cumu.empl','$'), colnames(par.dat))
col.index.to[25] <- grep(paste0('^','l.cumu.inc','$'), colnames(par.dat))


# make indicator for counterfactual fut
par.dat$cf.fut <- 0

# the order of the above variables in the index.from and index.to
# must be the same!
# and of course, the length of both vectors must also be the same
length(col.index.from)
length(col.index.to)

# save all the operations up to now
# save.image("U:/Collaboration/Nisen/Data/line 862.RData")

# bootstrap size
bssize <- 100

# monte carlo error reduction iteration number
msize <- 10

# output array
output.arr.ATT <- rep(NA,bssize*19*18)
dim(output.arr.ATT) <- c(bssize,19,18)
output.arr.ATT.3 <- output.arr.ATT.2 <- output.arr.ATT          
output.arr.PA.3 <- output.arr.PA.2 <- output.arr.PA <- output.arr.ATT 

youngparent.output.arr <- rep(NA,bssize*19*18)
dim(youngparent.output.arr) <- c(bssize,19,18)
youngparent.output.arr.3 <- youngparent.output.arr.2 <- youngparent.output.arr.mediation <- youngparent.output.arr

# Monte Carlo Error reduction arrays
monte.arr.ATT <- rep(NA,msize*19*18)
dim(monte.arr.ATT) <- c(msize,19,18)
monte.arr.ATT.3 <- monte.arr.ATT.2 <- monte.arr.ATT
monte.arr.PA <- monte.arr.PA.2 <- monte.arr.PA.3 <- monte.arr.ATT

youngparent.monte.arr <- rep(NA,msize*19*18)
dim(youngparent.monte.arr) <- c(msize,19,18)
youngparent.monte.arr.mediation <- youngparent.monte.arr

youngparent.monte.timetest <- matrix(rep(NA,msize*19),ncol=19)
youngparent.timetest <- matrix(rep(NA,bssize*19),ncol=19)
youngparent.timetest.mediation <- youngparent.timetest
youngparent.monte.timetest.mediation <- youngparent.monte.timetest

# perform the analysis for women or for men?
par.dat <- par.dat[par.dat$female==1,] # 0 = men, 1 = women

t1 <- Sys.time()

for(bs in 1:bssize) {
  
  # sample individuals from par.dat
  par.sample <- long.sample(par.dat,par.dat$id)
  
  # (re)fit models to par.sample
  # bsf = bootstrap fit
  # estimate relations
  fit.bsf.birth <- glm(formula.birth, family=binomial,subset=par.dat$tsfb==0, data=par.sample)
  
  fit.bsf.edu.pri <- glm(formula.edu.pri, family=binomial, data=par.sample)
  fit.bsf.edu.sec <- glm(formula.edu.sec, family=binomial, data=par.sample)
  fit.bsf.edu.tertlow <- glm(formula.edu.tertlow, family=binomial, data=par.sample)
  fit.bsf.edu.terthi <- glm(formula.edu.terthi, family=binomial, data=par.sample)
  
  fit.bsf.enrol <- glm(formula.enrol, family=binomial, data=par.sample)
  
  fit.bsf.partner.single <- glm(formula.partner.single, family=binomial, data=par.sample)
  fit.bsf.partner.cohab <- glm(formula.partner.cohab, family=binomial, data=par.sample)
  fit.bsf.partner.married <- glm(formula.partner.married, family=binomial, data=par.sample)
  
  fit.bsf.empl <- glm(formula.empl, family=binomial, data=par.sample)
  
  fit.bsf.unempl <- glm(formula.unempl, family=binomial, data=par.sample)
  
  fit.bsf.inc <- lm(formula.inc, data=par.sample)
  
  fit.bsf.hhinc <- lm(formula.hhinc, data=par.sample)
  
  fit.bsf.house.parents <- glm(formula.house.parents, family=binomial, data=par.sample)
  fit.bsf.house.owner <- glm(formula.house.owner, family=binomial, data=par.sample)
  fit.bsf.house.tenant <- glm(formula.house.tenant, family=binomial, data=par.sample)
  fit.bsf.house.other <- glm(formula.house.other, family=binomial, data=par.sample)
  
  # take individuals at fut==1
  # they get a special dataset here since we exclude people later
  par.sample.fut1 <- par.sample[par.sample$fut==1,]
  
  for(m in 1:msize) {
    
    ## this is the Monte Carlo Error reduction loop ##
    # I take the average over a few iterations
    # since each iteration has variation that is not due to
    # sampling variability, but due to probablistic predictions
    # and we are not interested in that type of error
    
    # take individuals at fut==1
    par.sample <- par.sample.fut1
    
    # give each individual a new ID, since
    # otherwise the ordering below will go wrong when
    # individuals are ordered by ID and time (since
    # due to resampling with replacement, multiple individuals
    # can have the same ID)
    par.sample$idnr <- 1:length(par.sample$id)
    
    # and the same file, but then for the intervention loop
    par.sample.3 <- par.sample.2 <- par.sample
    
    par.sample$group <- 0
    ## NATURAL LOOP ##
    # start a loop that moves through the follow-up time units
    
    for(t in 2:17) {
      
      par.sample.temp <- par.sample[par.sample$fut==(t-1) & par.sample$cens==0,]
      par.sample.temp$fut <- par.sample.temp$fut+1
      par.sample.temp$year <- par.sample.temp$year+1
      par.sample.temp$age <- par.sample.temp$age+1
      par.sample.temp$age.cent <- par.sample.temp$age.cent+1
      
      # lag values: since the new values for t have not yet been produced
      # I can actually take the column information from a row at t
      # and put it in the same row at t but then in a column meant for
      # lagged values
      # the first are the 'from' columns, and the second the 'to' columns
      # so: take values from the the 'from' column and put them in the 'to' column
      par.sample.temp[,col.index.to] <- par.sample.temp[,col.index.from]
      
      # put together with entire dataset
      par.sample <- rbind(par.sample,par.sample.temp)
      rm(par.sample.temp)
      
      # order by ID variable (needed for lags below)
      par.sample <- par.sample[order(par.sample$idnr,par.sample$year),]
      
      # update categorical age
      par.sample$age.1619 <- ifelse(par.sample$age <= 19,1,0)
      par.sample$age.2023 <- ifelse(par.sample$age >= 20 & par.sample$age <= 23,1,0)
      par.sample$age.2427 <- ifelse(par.sample$age >= 24 & par.sample$age <= 27,1,0)
      par.sample$age.2830 <- ifelse(par.sample$age >= 28 & par.sample$age <= 30,1,0)
      par.sample$age.3133 <- ifelse(par.sample$age >= 31,1,0)
      
      ## predict functions here
      par.sample <- par.sample[!is.na(par.sample$year),]
      
      # predict birth 
      # (only allowed if individual hasn't had a birth yet
      # since technically this is 'first live birth')
      # so that's why tsfb==0
      # (it is on purpose that I use parent==0 and not tsfb==0
      # since tsfb is updated later)
      par.sample$birth[par.sample$fut==t & par.sample$parent==0] <- 
        binomial.predict.1var(fit.bsf.birth, par.sample[par.sample$fut==t & par.sample$parent==0,])
      
      # since birth is only predicted for those who have not yet had
      # a birth, I set birth to 0 for those who already had a birth
      par.sample$birth[par.sample$fut==t & par.sample$parent==1] <- 0
      
      # get rid of those who still create NA stuff
      # because some predictor is missing
      par.sample <- par.sample[!is.na(par.sample$birth),]
      
      # update tsfb (= 0 when birth happens but 1 when l.birth=1)
      # and a +1 higher score for every year after that
      # this makes use of l.parent, so no problem that parent is updated after this
      par.sample$tsfb[par.sample$fut==t & par.sample$l.birth==1] <- 1
      par.sample$tsfb[par.sample$fut==t & par.sample$l.birth==0 &
                        par.sample$l.parent==1] <- par.sample$l.tsfb[par.sample$fut==t & par.sample$l.birth==0 &
                                                                       par.sample$l.parent==1] + 1
      
      # set parenthood based on birth history
      # namely if they had a birth this year
      # or if tsfb > 0 (indicating they had a 
      # birth some time in the past)
      par.sample$parent[par.sample$fut==t] <- ifelse(par.sample$birth[par.sample$fut==t]==1 |
                                                       par.sample$tsfb[par.sample$fut==t] > 0,
                                                     1,0)
      
      # predict enrolment
      par.sample$enrol[par.sample$fut==t] <- binomial.predict.1var(fit.bsf.enrol, par.sample[par.sample$fut==t,])
      
      # update enrolc
      par.sample$enrolc[par.sample$fut==t] <- par.sample$l.enrolc[par.sample$fut==t] + par.sample$enrol[par.sample$fut==t]
      
      # update entryc
      par.sample$entryc[par.sample$fut==t] <- ifelse(par.sample$enrol[par.sample$fut==t]==1 & par.sample$l.enrol[par.sample$fut==t]==0,
                                                     par.sample$entryc[par.sample$fut==t]+1,par.sample$entryc[par.sample$fut==t])
      
      # predict education
      x <- multinomial.predict.4var(fit.bsf.edu.pri,fit.bsf.edu.sec,
                                    fit.bsf.edu.tertlow,fit.bsf.edu.terthi,
                                    par.sample[par.sample$fut==t,])
      par.sample$edu.pri[par.sample$fut==t] <- x[,1]
      par.sample$edu.sec[par.sample$fut==t] <- x[,2]
      par.sample$edu.tertlow[par.sample$fut==t] <- x[,3]
      par.sample$edu.terthi[par.sample$fut==t] <- x[,4]
      
      # predict partnership
      x <- multinomial.predict.3var(fit.bsf.partner.single,fit.bsf.partner.cohab,fit.bsf.partner.married,
                                    par.sample[par.sample$fut==t,])
      par.sample$partner.single[par.sample$fut==t] <- x[,1]
      par.sample$partner.cohab[par.sample$fut==t] <- x[,2]
      par.sample$partner.married[par.sample$fut==t] <- x[,3]
      
      # predict employment
      par.sample$empl[par.sample$fut==t] <- binomial.predict.1var(fit.bsf.empl, par.sample[par.sample$fut==t,])
      
      # update cumulative employment
      par.sample$cumu.empl[par.sample$fut==t] <- par.sample$l.cumu.empl[par.sample$fut==t] + par.sample$empl[par.sample$fut==t]
      
      # predict unemployment
      par.sample$unempl[par.sample$fut==t] <- binomial.predict.1var(fit.bsf.unempl, par.sample[par.sample$fut==t,])
      
      # predict income
      par.sample$inc[par.sample$fut==t] <- predict.lm.withvar(fit.bsf.inc,par.sample[par.sample$fut==t,])
      
      # update cumulative income
      par.sample$cumu.inc[par.sample$fut==t] <- par.sample$l.cumu.inc[par.sample$fut==t] + par.sample$inc[par.sample$fut==t]
      
      # predict household income
      par.sample$inc.res[par.sample$fut==t] <- predict.lm.withvar(fit.bsf.hhinc,par.sample[par.sample$fut==t,])
      
      # predict house
      x <- multinomial.predict.4var(fit.bsf.house.parents,fit.bsf.house.owner,
                                    fit.bsf.house.tenant,fit.bsf.house.other,
                                    par.sample[par.sample$fut==t,])
      par.sample$house.parents[par.sample$fut==t] <- x[,1]
      par.sample$house.owner[par.sample$fut==t] <- x[,2]
      par.sample$house.tenant[par.sample$fut==t] <- x[,3]
      par.sample$house.other[par.sample$fut==t] <- x[,4]
      
      # end loop
    }
    
    ## intervention loop ##
    par.sample.2$group <- 1
    
    # start a loop that moves through the follow-up time units
    
    # for those who had a birth in par.sample
    # we can take their data and introduce the counterfactual
    # and let the rest be random
    # for those who don't, we could just take their data and
    # do no prediction whatsoever
    # this saves computational time
    all.idnr <- unique(par.sample$idnr)
    parent.idnr <- unique(par.sample$idnr[par.sample$birth==1])
    notparent.idnr <- setdiff(all.idnr,parent.idnr)
    
    par.sample.2 <- par.sample[which(par.sample$idnr %in% parent.idnr),]
    # the data of those who don't get a baby in the ns will be added at
    # the end of the counterfactual loop
    
    # shift birth by 3 years
    for(id in parent.idnr) {
      
      par.sample.2.temp <- par.sample.2[par.sample.2$idnr==id,]
      
      birthfut <- par.sample.2.temp$fut[par.sample.2.temp$birth==1]
      par.sample.2.temp$birth <- 0
      
      if(birthfut < 15) {
        par.sample.2.temp$birth[par.sample.2.temp$fut==(birthfut+3)] <- 1
      }
      
      # keep original tsfb saved for later
      par.sample.2.temp$tsfb.original <- par.sample.2.temp$tsfb
      par.sample.2.temp$parent.original <- par.sample.2.temp$parent
      
      # update tsfb
      par.sample.2.temp$tsfb <- par.sample.2.temp$tsfb-3
      par.sample.2.temp$tsfb[par.sample.2.temp$tsfb < 0] <- 0
      par.sample.2.temp$l.tsfb <- lag1.func(par.sample.2.temp$tsfb,par.sample.2.temp$idnr)
      
      # update parent
      par.sample.2.temp$parent <- ifelse(par.sample.2.temp$fut >= birthfut+3,1,0)
      par.sample.2.temp$l.parent <- lag1.func(par.sample.2.temp$parent,par.sample.2.temp$idnr)
      
      # make indicator for when counterfactual happens
      # which indicates when we can start making predictions again
      # in a dynamic way in the intervention loop below
      par.sample.2.temp$cf.fut <- ifelse(par.sample.2.temp$fut > birthfut,1,0)
      # I use 'larger than' because we only use
      # lagged effects in predictions 
      # otherwise it would have been 'equal to or larger than'
      # and of course, it is birthfut and not the postponed birth fut
      # because postponement can have an effect on e.g. education
      # before the new birth occurs
      
      # put back in larger dataset
      par.sample.2[par.sample.2$idnr==id,] <- par.sample.2.temp
      
    }
    
    # put this dataset into par.sample.3 as well
    # since these are the people we will intervene on
    # while keeping labour and income constant
    par.sample.3 <- par.sample.2
    
    for(t in sort(unique(par.sample.2$fut[par.sample.2$cf.fut==1]))) {
      
      ## I won't remove the rows from after birth
      ## I simply re-estimate the values in those rows
      
      # I will only re-estimate values for people
      # at fut==tsfb-2
      # I choose that value, because the postponement of birth
      # means that since they didn't have a birth yet
      # they can now at tsfb-2 have other events happen to them
      
      # at fut < tsfb-2 I won't re-estimate since that is before
      # the counterfactual situation and so things should by definition be
      # exactly the same
      
      # lag values: new values for t have been produced
      # so I take the column information from a row at t-1
      # and put it in the columns meant for lagged values at t
      # the first are the 'from' columns, and the second the 'to' columns
      # so: take values from the the 'from' column and put them in the 'to' column
      par.sample.2[par.sample.2$fut==t,col.index.to] <- par.sample.2[par.sample.2$fut==t-1,col.index.from]
      
      # order by ID variable (needed for lags below)
      par.sample.2 <- par.sample.2[order(par.sample.2$idnr,par.sample.2$year),]
      
      ## predict functions here
      
      # predict birth no longer needed
      # and derived variables neither
      
      # predict enrolment
      par.sample.2$enrol[par.sample.2$fut==t & par.sample.2$cf.fut==1] <- binomial.predict.1var(fit.bsf.enrol, par.sample.2[par.sample.2$fut==t & par.sample.2$cf.fut==1,])
      
      # update enrolc
      par.sample.2$enrolc[par.sample.2$fut==t & par.sample.2$cf.fut==1] <- par.sample.2$l.enrolc[par.sample.2$fut==t & par.sample.2$cf.fut==1] + par.sample.2$enrol[par.sample.2$fut==t & par.sample.2$cf.fut==1]
      
      # update entryc
      par.sample.2$entryc[par.sample.2$fut==t & par.sample.2$cf.fut==1] <- ifelse(par.sample.2$enrol[par.sample.2$fut==t & par.sample.2$cf.fut==1]==1 & par.sample.2$l.enrol[par.sample.2$fut==t & par.sample.2$cf.fut==1]==0,
                                                                                  par.sample.2$entryc[par.sample.2$fut==t & par.sample.2$cf.fut==1]+1,par.sample.2$entryc[par.sample.2$fut==t & par.sample.2$cf.fut==1])
      
      # predict education
      x <- multinomial.predict.4var(fit.bsf.edu.pri,fit.bsf.edu.sec,
                                    fit.bsf.edu.tertlow,fit.bsf.edu.terthi,
                                    par.sample.2[par.sample.2$fut==t & par.sample.2$cf.fut==1,])
      par.sample.2$edu.pri[par.sample.2$fut==t & par.sample.2$cf.fut==1] <- x[,1]
      par.sample.2$edu.sec[par.sample.2$fut==t & par.sample.2$cf.fut==1] <- x[,2]
      par.sample.2$edu.tertlow[par.sample.2$fut==t & par.sample.2$cf.fut==1] <- x[,3]
      par.sample.2$edu.terthi[par.sample.2$fut==t & par.sample.2$cf.fut==1] <- x[,4]
      
      # predict partnership
      x <- multinomial.predict.3var(fit.bsf.partner.single,fit.bsf.partner.cohab,fit.bsf.partner.married,
                                    par.sample.2[par.sample.2$fut==t & par.sample.2$cf.fut==1,])
      par.sample.2$partner.single[par.sample.2$fut==t & par.sample.2$cf.fut==1] <- x[,1]
      par.sample.2$partner.cohab[par.sample.2$fut==t & par.sample.2$cf.fut==1] <- x[,2]
      par.sample.2$partner.married[par.sample.2$fut==t & par.sample.2$cf.fut==1] <- x[,3]
      
      # predict employment
      par.sample.2$empl[par.sample.2$fut==t & par.sample.2$cf.fut==1] <- binomial.predict.1var(fit.bsf.empl, par.sample.2[par.sample.2$fut==t & par.sample.2$cf.fut==1,])
      
      # update cumulative employment
      par.sample.2$cumu.empl[par.sample.2$fut==t] <- par.sample.2$l.cumu.empl[par.sample.2$fut==t] + par.sample.2$empl[par.sample.2$fut==t]
      
      # predict unemployment
      par.sample.2$unempl[par.sample.2$fut==t & par.sample.2$cf.fut==1] <- binomial.predict.1var(fit.bsf.unempl, par.sample.2[par.sample.2$fut==t & par.sample.2$cf.fut==1,])
      
      # predict income
      par.sample.2$inc[par.sample.2$fut==t & par.sample.2$cf.fut==1] <- predict.lm.withvar(fit.bsf.inc,par.sample.2[par.sample.2$fut==t & par.sample.2$cf.fut==1,])
      
      # update cumulative income
      par.sample.2$cumu.inc[par.sample.2$fut==t] <- par.sample.2$l.cumu.inc[par.sample.2$fut==t] + par.sample.2$inc[par.sample.2$fut==t]
      
      # predict household income
      par.sample.2$inc.res[par.sample.2$fut==t & par.sample.2$cf.fut==1] <- predict.lm.withvar(fit.bsf.hhinc,par.sample.2[par.sample.2$fut==t & par.sample.2$cf.fut==1,])
      
      # predict house
      x <- multinomial.predict.4var(fit.bsf.house.parents,fit.bsf.house.owner,
                                    fit.bsf.house.tenant,fit.bsf.house.other,
                                    par.sample.2[par.sample.2$fut==t & par.sample.2$cf.fut==1,])
      par.sample.2$house.parents[par.sample.2$fut==t & par.sample.2$cf.fut==1] <- x[,1]
      par.sample.2$house.owner[par.sample.2$fut==t & par.sample.2$cf.fut==1] <- x[,2]
      par.sample.2$house.tenant[par.sample.2$fut==t & par.sample.2$cf.fut==1] <- x[,3]
      par.sample.2$house.other[par.sample.2$fut==t & par.sample.2$cf.fut==1] <- x[,4]
      
      # end loop
    }
    
    par.sample.temp <- par.sample[which(par.sample$idnr %in% parent.idnr),]
    
    for(t in sort(unique(par.sample.3$fut[par.sample.3$cf.fut==1]))) {
      
      ## I won't remove the rows from after birth
      ## I simply re-estimate the values in those rows
      
      # I will only re-estimate values for people
      # at fut==tsfb-2
      # I choose that value, because the postponement of birth
      # means that since they didn't have a birth yet
      # they can now at tsfb-2 have other events happen to them
      
      # at fut < tsfb-2 I won't re-estimate since that is before
      # the counterfactual situation and so things should by definition be
      # exactly the same
      
      # lag values: new values for t have been produced
      # so I take the column information from a row at t-1
      # and put it in the columns meant for lagged values at t
      # the first are the 'from' columns, and the second the 'to' columns
      # so: take values from the the 'from' column and put them in the 'to' column
      par.sample.3[par.sample.3$fut==t,col.index.to] <- par.sample.3[par.sample.3$fut==t-1,col.index.from]
      
      # order by ID variable (needed for lags below)
      par.sample.3 <- par.sample.3[order(par.sample.3$idnr,par.sample.3$year),]
      
      # since we keep the labour market variables constant, we will
      # draw them from the natural course
      # we can draw this deterministically, since we don't model censoring or competing risks

      # draw employment
      par.sample.3$empl[par.sample.3$fut==t] <- par.sample.temp$empl[par.sample.temp$fut==t]
      
      # draw cumulative employment
      par.sample.3$cumu.empl[par.sample.3$fut==t] <- par.sample.temp$cumu.empl[par.sample.temp$fut==t]
        
      # draw unemployment
      par.sample.3$unempl[par.sample.3$fut==t] <- par.sample.temp$unempl[par.sample.temp$fut==t]
      
      # draw income
      par.sample.3$inc[par.sample.3$fut==t] <- par.sample.temp$inc[par.sample.temp$fut==t]
      
      # draw cumulative income
      par.sample.3$cumu.inc[par.sample.3$fut==t] <- par.sample.temp$cumu.inc[par.sample.temp$fut==t]
      

      ## predict functions here
      
      # predict birth no longer needed
      # and derived variables neither
      
      # predict enrolment
      par.sample.3$enrol[par.sample.3$fut==t & par.sample.3$cf.fut==1] <- binomial.predict.1var(fit.bsf.enrol, par.sample.3[par.sample.3$fut==t & par.sample.3$cf.fut==1,])
      
      # update enrolc
      par.sample.3$enrolc[par.sample.3$fut==t & par.sample.3$cf.fut==1] <- par.sample.3$l.enrolc[par.sample.3$fut==t & par.sample.3$cf.fut==1] + par.sample.3$enrol[par.sample.3$fut==t & par.sample.3$cf.fut==1]
      
      # update entryc
      par.sample.3$entryc[par.sample.3$fut==t & par.sample.3$cf.fut==1] <- ifelse(par.sample.3$enrol[par.sample.3$fut==t & par.sample.3$cf.fut==1]==1 & par.sample.3$l.enrol[par.sample.3$fut==t & par.sample.3$cf.fut==1]==0,
                                                                                  par.sample.3$entryc[par.sample.3$fut==t & par.sample.3$cf.fut==1]+1,par.sample.3$entryc[par.sample.3$fut==t & par.sample.3$cf.fut==1])
      
      # predict education
      x <- multinomial.predict.4var(fit.bsf.edu.pri,fit.bsf.edu.sec,
                                    fit.bsf.edu.tertlow,fit.bsf.edu.terthi,
                                    par.sample.3[par.sample.3$fut==t & par.sample.3$cf.fut==1,])
      par.sample.3$edu.pri[par.sample.3$fut==t & par.sample.3$cf.fut==1] <- x[,1]
      par.sample.3$edu.sec[par.sample.3$fut==t & par.sample.3$cf.fut==1] <- x[,2]
      par.sample.3$edu.tertlow[par.sample.3$fut==t & par.sample.3$cf.fut==1] <- x[,3]
      par.sample.3$edu.terthi[par.sample.3$fut==t & par.sample.3$cf.fut==1] <- x[,4]
      
      # predict partnership
      x <- multinomial.predict.3var(fit.bsf.partner.single,fit.bsf.partner.cohab,fit.bsf.partner.married,
                                    par.sample.3[par.sample.3$fut==t & par.sample.3$cf.fut==1,])
      par.sample.3$partner.single[par.sample.3$fut==t & par.sample.3$cf.fut==1] <- x[,1]
      par.sample.3$partner.cohab[par.sample.3$fut==t & par.sample.3$cf.fut==1] <- x[,2]
      par.sample.3$partner.married[par.sample.3$fut==t & par.sample.3$cf.fut==1] <- x[,3]
      
      # predict house
      x <- multinomial.predict.4var(fit.bsf.house.parents,fit.bsf.house.owner,
                                    fit.bsf.house.tenant,fit.bsf.house.other,
                                    par.sample.3[par.sample.3$fut==t & par.sample.3$cf.fut==1,])
      par.sample.3$house.parents[par.sample.3$fut==t & par.sample.3$cf.fut==1] <- x[,1]
      par.sample.3$house.owner[par.sample.3$fut==t & par.sample.3$cf.fut==1] <- x[,2]
      par.sample.3$house.tenant[par.sample.3$fut==t & par.sample.3$cf.fut==1] <- x[,3]
      par.sample.3$house.other[par.sample.3$fut==t & par.sample.3$cf.fut==1] <- x[,4]
      
      # predict household income
      par.sample.3$inc.res[par.sample.3$fut==t & par.sample.3$cf.fut==1] <- predict.lm.withvar(fit.bsf.hhinc,par.sample.3[par.sample.3$fut==t & par.sample.3$cf.fut==1,])
      
      
      # end loop
    }
    
    rm(par.sample.temp)
    
    # save covariate information
    # in the natural course and the intervention scenario
    # by fut and monte carlo iteration
    ## only keep parents in both datasets
    # first let's save the non-parents information
    notpar.sample <- par.sample[which(par.sample$idnr %in% notparent.idnr),]
    
    # we already filtered out the non-parents in par.sample.2 and par.sample.3
    # so let's filter out the ones in par.sample as well
    par.sample <- par.sample[which(par.sample$idnr %in% parent.idnr),]
    
    ## calculate tfsb for parents from par.sample.2 and par.sample.3
    # simply put it back based on original tsfb
    par.sample.2$tsfb <- par.sample.2$tsfb.original
    par.sample.2$parent <- par.sample.2$parent.original
    
    par.sample.3$tsfb <- par.sample.3$tsfb.original
    par.sample.3$parent <- par.sample.3$parent.original
    
    ## add parent to both tsfbs
    # so that the moment of birth is also included
    # but not the tsfb==0 when a birth is not happening
    par.sample$tsfb <- par.sample$tsfb + par.sample$parent
    par.sample.2$tsfb <- par.sample.2$tsfb + par.sample.2$parent
    par.sample.3$tsfb <- par.sample.3$tsfb + par.sample.3$parent
    # note that the meaning of TSFB in the initial graphs is therefore
    # different than what you might expect!
    
    # save IDs for subgroup comparison
    # namely: make a snapshot of the ids that people have
    # when they have their first child at the appropriate age
    # which is now at tsfb==1 because of adding parent to tsfb calc above
    youngparent.idnr <- unique(par.sample$idnr[par.sample$age <= 29 & par.sample$tsfb==1])
    
    # make subsets based on that
    youngparent.par.sample <- par.sample[which(par.sample$idnr %in% youngparent.idnr),]
    youngparent.par.sample.2 <- par.sample.2[which(par.sample.2$idnr %in% youngparent.idnr),]
    youngparent.par.sample.3 <- par.sample.3[which(par.sample.3$idnr %in% youngparent.idnr),]
    
    # take individual-level differences for youngparents
    # make sure they are ordered the same
    youngparent.par.sample <- youngparent.par.sample[order(youngparent.par.sample$idnr,youngparent.par.sample$fut),]
    youngparent.par.sample.2 <- youngparent.par.sample.2[order(youngparent.par.sample.2$idnr,youngparent.par.sample.2$fut),]
    youngparent.par.sample.3 <- youngparent.par.sample.3[order(youngparent.par.sample.3$idnr,youngparent.par.sample.3$fut),]
    youngparent.par.diff <- youngparent.par.sample.2[,col.index.from[1:19]] - youngparent.par.sample[,col.index.from[1:19]]
    youngparent.par.diff$tsfb <- youngparent.par.sample$tsfb
    youngparent.par.diff.mediation <- youngparent.par.sample.3[,col.index.from[1:19]] - youngparent.par.sample[,col.index.from[1:19]]
    youngparent.par.diff.mediation$tsfb <- youngparent.par.sample$tsfb
    
    youngparent.monte.timetest[m,] <- apply(youngparent.par.sample.2[youngparent.par.sample.2$tsfb==4,col.index.from[1:19]] - 
                                              youngparent.par.sample[youngparent.par.sample.2$tsfb==1,col.index.from[1:19]],
                                            MARGIN=2,mean,na.rm=T)
    
    youngparent.monte.timetest.mediation[m,] <- apply(youngparent.par.sample.3[youngparent.par.sample.3$tsfb==4,col.index.from[1:19]] - 
                                                        youngparent.par.sample[youngparent.par.sample.3$tsfb==1,col.index.from[1:19]],
                                                      MARGIN=2,mean,na.rm=T)
    
    for(a in sort(unique(par.sample$tsfb))[-1]) {
      
      # subset analysis: young parents (IATT)
      youngparent.monte.arr[m,,a] <- apply(youngparent.par.diff[youngparent.par.diff$tsfb==a,],MARGIN=2,FUN=mean,na.rm=T)[1:19]
      
      # subset analysis: young parents mediation (IATT)
      youngparent.monte.arr.mediation[m,,a] <- apply(youngparent.par.diff.mediation[youngparent.par.diff.mediation$tsfb==a,],MARGIN=2,FUN=mean,na.rm=T)[1:19]
      
    }
    
    # treatment effect for the treated by age
    for(a in sort(unique(par.sample$age))) {
      
      # sample 1: natural course (ATT)
      monte.arr.ATT[m,,a-15] <- apply(par.sample[par.sample$age==a,][,col.index.from[1:19]],MARGIN=2,FUN=mean,na.rm=T)
      
      # sample 2: intervention scenario (ATT)
      monte.arr.ATT.2[m,,a-15] <- apply(par.sample.2[par.sample.2$age==a,][,col.index.from[1:19]],MARGIN=2,FUN=mean,na.rm=T)
      
      # sample 2: intervention scenario with education drawn from the natural course (ATT)
      monte.arr.ATT.3[m,,a-15] <- apply(par.sample.3[par.sample.3$age==a,][,col.index.from[1:19]],MARGIN=2,FUN=mean,na.rm=T)
      
    }
    
    ## put parents back in both datasets and save pop averaged effect
    # for everyone by age
    par.sample <- rbind(par.sample,notpar.sample)
    par.sample.2 <- rbind(par.sample.2,notpar.sample)
    par.sample.3 <- rbind(par.sample.3,notpar.sample)
    
    for(a in sort(unique(par.sample$age))) {
      
      # sample 1: natural course (pop averaged)
      monte.arr.PA[m,,a-15] <- apply(par.sample[par.sample$age==a,][,col.index.from[1:19]],MARGIN=2,FUN=mean,na.rm=T)
      
      # sample 2: intervention scenario (pop averaged)
      monte.arr.PA.2[m,,a-15] <- apply(par.sample.2[par.sample.2$age==a,][,col.index.from[1:19]],MARGIN=2,FUN=mean,na.rm=T)
      
      # sample 3: intervention scenario with education drawn from the natural course (pop averaged)
      monte.arr.PA.3[m,,a-15] <- apply(par.sample.3[par.sample.3$age==a,][,col.index.from[1:19]],MARGIN=2,FUN=mean,na.rm=T)
      
    }
    
    
  }
  
  # average over the monte carlo iterations and save
  # by bootstrap to only leave sampling variability
  # first for ATT
  for(a in sort(unique(par.sample$tsfb))[-1]) {
    
    # subset analysis: young parents
    # YOUNG PARENTS
    youngparent.output.arr[bs,,a] <- apply(youngparent.monte.arr[,,a],MARGIN=2,FUN=mean,na.rm=T)
    
    youngparent.output.arr.mediation[bs,,a] <- apply(youngparent.monte.arr.mediation[,,a],MARGIN=2,FUN=mean,na.rm=T)
    
  }
  
  # !! then for the timetest
  youngparent.timetest[bs,] <- apply(youngparent.monte.timetest,MARGIN=2,mean,na.rm=T)
  
  youngparent.timetest.mediation[bs,] <- apply(youngparent.monte.timetest.mediation,MARGIN=2,mean,na.rm=T)
  
  # then for PA and ATT
  for(a in sort(unique(par.sample$age))) {
    
    # sample 1: natural course
    output.arr.ATT[bs,,a-15] <- apply(monte.arr.ATT[,,a-15],MARGIN=2,FUN=mean,na.rm=T)
    
    # sample 2: intervention scenario
    output.arr.ATT.2[bs,,a-15] <- apply(monte.arr.ATT.2[,,a-15],MARGIN=2,FUN=mean,na.rm=T)
    
    # sample 3: intervention scenario with education drawn from the natural course
    output.arr.ATT.3[bs,,a-15] <- apply(monte.arr.ATT.3[,,a-15],MARGIN=2,FUN=mean,na.rm=T)
    
    # sample 1: natural course
    output.arr.PA[bs,,a-15] <- apply(monte.arr.PA[,,a-15],MARGIN=2,FUN=mean,na.rm=T)
    
    # sample 2: intervention scenario
    output.arr.PA.2[bs,,a-15] <- apply(monte.arr.PA.2[,,a-15],MARGIN=2,FUN=mean,na.rm=T)
    
    # sample 3: intervention scenario with education drawn from the natural course
    output.arr.PA.3[bs,,a-15] <- apply(monte.arr.PA.3[,,a-15],MARGIN=2,FUN=mean,na.rm=T)
    
  }
  
  print(bs)
}

t2 <- Sys.time()

t2 - t1


time.test <- function(nc, cf, alpha) {
  
  x <- apply(cf - nc,2,quantile,na.rm=T,
             probs=c(alpha/2,1-(alpha/2)))
  return(cbind(1:length(x[1,]),!(x[1,] <= 0 &  x[2,] >= 0)))
  
}

t(time.test(output.arr.ATT[,1,],output.arr.ATT.2[,1,],0.05))

# average treatment effect for the treated
plot(16:33,apply(output.arr.ATT[,1,],2,mean,na.rm=T),type='l',lwd=2,main='birth',xlab='Age')
lines(16:33,apply(output.arr.ATT.2[,1,],2,mean,na.rm=T),type='l',lwd=2,lty=2)
lines(16:33,apply(output.arr.ATT.3[,1,],2,mean,na.rm=T),type='l',lwd=2,lty=3)
t(time.test(output.arr.ATT[,1,],output.arr.ATT.2[,1,],0.05))

plot(16:33,apply(output.arr.ATT[,2,],2,mean,na.rm=T),type='l',lwd=2,ylim=c(0,1),main='edu prime',xlab='Age')
lines(16:33,apply(output.arr.ATT.2[,2,],2,mean,na.rm=T),type='l',lwd=2,lty=2)
abline(v=6)
t(time.test(output.arr.ATT[,2,],output.arr.ATT.2[,2,],0.05))

plot(16:33,apply(output.arr.ATT[,3,],2,mean,na.rm=T),type='l',lwd=2,ylim=c(0,1),main='edu second',xlab='Age')
lines(16:33,apply(output.arr.ATT.2[,3,],2,mean,na.rm=T),type='l',lwd=2,lty=2)
lines(16:33,apply(output.arr.ATT.3[,3,],2,mean,na.rm=T),type='l',lwd=2,lty=3)
abline(v=6)
t(time.test(output.arr.ATT[,3,],output.arr.ATT.2[,3,],0.05))

plot(16:33,apply(output.arr.ATT[,5,],2,mean,na.rm=T),type='l',lwd=2,ylim=c(0,0.35),main='edu tert low',xlab='Age')
lines(16:33,apply(output.arr.ATT.2[,5,],2,mean,na.rm=T),type='l',lwd=2,lty=2)
lines(16:33,apply(output.arr.ATT.3[,5,],2,mean,na.rm=T),type='l',lwd=2,lty=3)
abline(v=6)
t(time.test(output.arr.ATT[,5,],output.arr.ATT.2[,5,],0.05))

plot(16:33,apply(output.arr.ATT[,6,],2,mean,na.rm=T),type='l',lwd=2,ylim=c(0,0.15),main='edu tert high',xlab='Age')
lines(16:33,apply(output.arr.ATT.2[,6,],2,mean,na.rm=T),type='l',lwd=2,lty=2)
lines(16:33,apply(output.arr.ATT.3[,6,],2,mean,na.rm=T),type='l',lwd=2,lty=3)
abline(v=6)
t(time.test(output.arr.ATT[,6,],output.arr.ATT.2[,6,],0.05))

plot(16:33,apply(output.arr.ATT[,7,],2,mean,na.rm=T),type='l',lwd=2,ylim=c(0.1,1),main='enrolment',xlab='Age')
lines(16:33,apply(output.arr.ATT.2[,7,],2,mean,na.rm=T),type='l',lwd=2,lty=2)
lines(16:33,apply(output.arr.ATT.3[,7,],2,mean,na.rm=T),type='l',lwd=2,lty=3)
abline(v=6)
t(time.test(output.arr.ATT[,7,],output.arr.ATT.2[,7,],0.05))

plot(16:33,apply(output.arr.ATT[,8,],2,mean,na.rm=T),type='l',lwd=2,ylim=c(3,7),main='cumulative enrolment',xlab='Age')
lines(16:33,apply(output.arr.ATT.2[,8,],2,mean,na.rm=T),type='l',lwd=2,lty=2)
abline(v=6)
t(time.test(output.arr.ATT[,8,],output.arr.ATT.2[,8,],0.05))

plot(16:33,apply(output.arr.ATT[,9,],2,mean,na.rm=T),type='l',lwd=2,ylim=c(0.15,1),main='partnership single',xlab='Age')
lines(16:33,apply(output.arr.ATT.2[,9,],2,mean,na.rm=T),type='l',lwd=2,lty=2)
lines(16:33,apply(output.arr.ATT.3[,9,],2,mean,na.rm=T),type='l',lwd=2,lty=3)
abline(v=6)
t(time.test(output.arr.ATT[,10,],output.arr.ATT.2[,10,],0.05))

plot(16:33,apply(output.arr.ATT[,10,],2,mean,na.rm=T),type='l',lwd=2,main='partnership cohabitation',xlab='Age')
lines(16:33,apply(output.arr.ATT.2[,10,],2,mean,na.rm=T),type='l',lwd=2,lty=2)
lines(16:33,apply(output.arr.ATT.3[,10,],2,mean,na.rm=T),type='l',lwd=2,lty=3)
abline(v=6)
t(time.test(output.arr.ATT[,10,],output.arr.ATT.2[,10,],0.05))

plot(16:33,apply(output.arr.ATT[,11,],2,mean,na.rm=T),type='l',lwd=2,main='partnership married',xlab='Age')
lines(16:33,apply(output.arr.ATT.2[,11,],2,mean,na.rm=T),type='l',lwd=2,lty=2)
lines(16:33,apply(output.arr.ATT.3[,11,],2,mean,na.rm=T),type='l',lwd=2,lty=3)
abline(v=6)
t(time.test(output.arr.ATT[,11,],output.arr.ATT.2[,11,],0.05))

plot(16:33,apply(output.arr.ATT[,12,],2,mean,na.rm=T),ylim=c(0,1),type='l',lwd=2,main='employment',xlab='Age')
lines(16:33,apply(output.arr.ATT.2[,12,],2,mean,na.rm=T),type='l',lwd=2,lty=2)
lines(16:33,apply(output.arr.ATT.3[,12,],2,mean,na.rm=T),type='l',lwd=2,lty=3)
abline(v=6)
t(time.test(output.arr.ATT[,12,],output.arr.ATT.2[,12,],0.05))

plot(16:33,apply(output.arr.ATT[,13,],2,mean,na.rm=T),type='l',lwd=2,ylim=c(0.00,0.50),main='unemployment',xlab='Age')
lines(16:33,apply(output.arr.ATT.2[,13,],2,mean,na.rm=T),type='l',lwd=2,lty=2)
lines(16:33,apply(output.arr.ATT.3[,13,],2,mean,na.rm=T),type='l',lwd=2,lty=3)
abline(v=6)
t(time.test(output.arr.ATT[,13,],output.arr.ATT.2[,13,],0.05))

plot(16:33,apply(output.arr.ATT[,14,],2,mean,na.rm=T),type='l',lwd=2,ylim=c(0,50),main='income',xlab='Age')
lines(16:33,apply(output.arr.ATT.2[,14,],2,mean,na.rm=T),type='l',lwd=2,lty=2)
lines(16:33,apply(output.arr.ATT.3[,14,],2,mean,na.rm=T),type='l',lwd=2,lty=3)
abline(v=6)
t(time.test(output.arr.ATT[,14,],output.arr.ATT.2[,14,],0.05))

plot(16:33,apply(output.arr.ATT[,15,],2,mean,na.rm=T),type='l',lwd=2,ylim=c(40,65),main='household income',xlab='Age')
lines(16:33,apply(output.arr.ATT.2[,15,],2,mean,na.rm=T),type='l',lwd=2,lty=2)
lines(16:33,apply(output.arr.ATT.3[,15,],2,mean,na.rm=T),type='l',lwd=2,lty=3)
abline(v=6)
t(time.test(output.arr.ATT[,15,],output.arr.ATT.2[,15,],0.05))

plot(16:33,apply(output.arr.ATT[,16,],2,mean,na.rm=T),type='l',lwd=2,main='house live with parents',xlab='Age')
lines(16:33,apply(output.arr.ATT.2[,16,],2,mean,na.rm=T),type='l',lwd=2,lty=2)
abline(v=6)
t(time.test(output.arr.ATT[,16,],output.arr.ATT.2[,16,],0.05))

plot(16:33,apply(output.arr.ATT[,17,],2,mean,na.rm=T),type='l',lwd=2,ylim=c(0.45,0.75),main='house owner',xlab='Age')
lines(16:33,apply(output.arr.ATT.2[,17,],2,mean,na.rm=T),type='l',lwd=2,lty=2)
abline(v=6)
t(time.test(output.arr.ATT[,17,],output.arr.ATT.2[,17,],0.05))

plot(16:33,apply(output.arr.ATT[,18,],2,mean,na.rm=T),type='l',lwd=2,ylim=c(0.25,0.45),main='house tenant',xlab='Age')
lines(16:33,apply(output.arr.ATT.2[,18,],2,mean,na.rm=T),type='l',lwd=2,lty=2)
abline(v=6)
t(time.test(output.arr.ATT[,18,],output.arr.ATT.2[,18,],0.05))

plot(16:33,apply(output.arr.ATT[,19,],2,mean,na.rm=T),type='l',lwd=2,ylim=c(0.045,0.06),main='house other',xlab='Age')
lines(16:33,apply(output.arr.ATT.2[,19,],2,mean,na.rm=T),type='l',lwd=2,lty=2)
abline(v=6)
t(time.test(output.arr.ATT[,19,],output.arr.ATT.2[,19,],0.05))


## effect by age for education, enrolment, employment, and income
# by age
# once just for higher tert, and then lower and higher tert combined
plot(16:33,apply(output.arr.ATT.2[,2,] - output.arr.ATT[,2,],2,mean,na.rm=T),
     type='l',lwd=2,ylim=c(-0.05,0.05),main='edu prime',xlab='Age',ylab='Effect')
lines(16:33,apply(output.arr.ATT.2[,2,] - output.arr.ATT[,2,],2,quantile,probs=0.975,na.rm=T),
      type='l',lwd=2,lty=2)
lines(16:33,apply(output.arr.ATT.2[,2,] - output.arr.ATT[,2,],2,quantile,probs=0.025,na.rm=T),
      type='l',lwd=2,lty=2)

plot(16:33,apply(output.arr.ATT.2[,3,] - output.arr.ATT[,3,],2,mean,na.rm=T),
     type='l',lwd=2,ylim=c(-0.05,0.05),main='edu second',xlab='Age',ylab='Effect')
lines(16:33,apply(output.arr.ATT.2[,3,] - output.arr.ATT[,3,],2,quantile,probs=0.975,na.rm=T),
      type='l',lwd=2,lty=2)
lines(16:33,apply(output.arr.ATT.2[,3,] - output.arr.ATT[,3,],2,quantile,probs=0.025,na.rm=T),
      type='l',lwd=2,lty=2)

plot(16:33,apply(output.arr.ATT.2[,5,] - output.arr.ATT[,5,],2,mean,na.rm=T),
     type='l',lwd=2,ylim=c(-0.05,0.05),main='edu tert low',xlab='Age',ylab='Effect')
lines(16:33,apply(output.arr.ATT.2[,5,] - output.arr.ATT[,5,],2,quantile,probs=0.975,na.rm=T),
      type='l',lwd=2,lty=2)
lines(16:33,apply(output.arr.ATT.2[,5,] - output.arr.ATT[,5,],2,quantile,probs=0.025,na.rm=T),
      type='l',lwd=2,lty=2)

plot(16:33,apply(output.arr.ATT.2[,6,] - output.arr.ATT[,6,],2,mean,na.rm=T),
     type='l',lwd=2,ylim=c(-0.05,0.05),main='edu tert high',xlab='Age',ylab='Effect')
lines(16:33,apply(output.arr.ATT.2[,6,] - output.arr.ATT[,6,],2,quantile,probs=0.975,na.rm=T),
      type='l',lwd=2,lty=2)
lines(16:33,apply(output.arr.ATT.2[,6,] - output.arr.ATT[,6,],2,quantile,probs=0.025,na.rm=T),
      type='l',lwd=2,lty=2)

# combine lower tert and higher tert into combined tert
tertcomb.arr.ATT.2 <- output.arr.ATT.2[,5,] + output.arr.ATT.2[,6,]
tertcomb.arr.ATT <- output.arr.ATT[,5,] + output.arr.ATT[,6,]

plot(16:33,apply(tertcomb.arr.ATT.2 - tertcomb.arr.ATT,2,mean,na.rm=T),
     type='l',lwd=2,ylim=c(-0.05,0.05),main='edu tert combined',xlab='Age',ylab='Effect')
lines(16:33,apply(tertcomb.arr.ATT.2 - tertcomb.arr.ATT,2,quantile,probs=0.975,na.rm=T),
      type='l',lwd=2,lty=2)
lines(16:33,apply(tertcomb.arr.ATT.2 - tertcomb.arr.ATT,2,quantile,probs=0.025,na.rm=T),
      type='l',lwd=2,lty=2)

plot(16:33,apply(output.arr.ATT.2[,7,] - output.arr.ATT[,7,],2,mean,na.rm=T),
     type='l',lwd=2,ylim=c(-0.05,0.05),main='enrolment',xlab='Age',ylab='Effect')
lines(16:33,apply(output.arr.ATT.2[,7,] - output.arr.ATT[,7,],2,quantile,probs=0.975,na.rm=T),
      type='l',lwd=2,lty=2)
lines(16:33,apply(output.arr.ATT.2[,7,] - output.arr.ATT[,7,],2,quantile,probs=0.025,na.rm=T),
      type='l',lwd=2,lty=2)

plot(16:33,apply(output.arr.ATT.2[,12,] - output.arr.ATT[,12,],2,mean,na.rm=T),
     type='l',lwd=2,ylim=c(-0.05,0.10),main='employment',xlab='Age',ylab='Effect')
lines(16:33,apply(output.arr.ATT.2[,12,] - output.arr.ATT[,12,],2,quantile,probs=0.975,na.rm=T),
      type='l',lwd=2,lty=2)
lines(16:33,apply(output.arr.ATT.2[,12,] - output.arr.ATT[,12,],2,quantile,probs=0.025,na.rm=T),
      type='l',lwd=2,lty=2)

plot(16:33,apply(output.arr.ATT.2[,14,] - output.arr.ATT[,14,],2,mean,na.rm=T),
     type='l',lwd=2,ylim=c(-0.1,1.5),main='income',xlab='Age',ylab='Effect')
lines(16:33,apply(output.arr.ATT.2[,14,] - output.arr.ATT[,14,],2,quantile,probs=0.975,na.rm=T),
      type='l',lwd=2,lty=2)
lines(16:33,apply(output.arr.ATT.2[,14,] - output.arr.ATT[,14,],2,quantile,probs=0.025,na.rm=T),
      type='l',lwd=2,lty=2)

## put the above in a table



## ATT with mediation
## effect by age for education, enrolment, employment, and income
# by age
# once just for higher tert, and then lower and higher tert combined
plot(16:33,apply(output.arr.ATT.2[,2,] - output.arr.ATT[,2,],2,mean,na.rm=T),
     type='l',lwd=2,ylim=c(-0.05,0.05),main='edu prime',xlab='Age',ylab='Effect')
lines(16:33,apply(output.arr.ATT.3[,2,]  - output.arr.ATT[,2,],2,mean))

plot(16:33,apply(output.arr.ATT.2[,3,] - output.arr.ATT[,3,],2,mean,na.rm=T),
     type='l',lwd=2,ylim=c(-0.05,0.05),main='edu second',xlab='Age',ylab='Effect')
lines(16:33,apply(output.arr.ATT.3[,3,]  - output.arr.ATT[,3,],2,mean))

plot(16:33,apply(output.arr.ATT.2[,5,] - output.arr.ATT[,5,],2,mean,na.rm=T),
     type='l',lwd=2,ylim=c(-0.05,0.05),main='edu tert low',xlab='Age',ylab='Effect')
lines(16:33,apply(output.arr.ATT.3[,5,]  - output.arr.ATT[,3,],2,mean)) ### ????

plot(16:33,apply(output.arr.ATT.2[,6,] - output.arr.ATT[,6,],2,mean,na.rm=T),
     type='l',lwd=2,ylim=c(-0.05,0.05),main='edu tert high',xlab='Age',ylab='Effect')
lines(16:33,apply(output.arr.ATT.3[,6,]  - output.arr.ATT[,6,],2,mean)) 

# combine lower tert and higher tert into combined tert
tertcomb.arr.ATT.2 <- output.arr.ATT.2[,5,] + output.arr.ATT.2[,6,]
tertcomb.arr.ATT <- output.arr.ATT[,5,] + output.arr.ATT[,6,]

plot(16:33,apply(tertcomb.arr.ATT.2 - tertcomb.arr.ATT,2,mean,na.rm=T),
     type='l',lwd=2,ylim=c(-0.05,0.05),main='edu tert combined',xlab='Age',ylab='Effect')
lines(16:33,apply(tertcomb.arr.ATT.2 - tertcomb.arr.ATT,2,quantile,probs=0.975,na.rm=T),
      type='l',lwd=2,lty=2)
lines(16:33,apply(tertcomb.arr.ATT.2 - tertcomb.arr.ATT,2,quantile,probs=0.025,na.rm=T),
      type='l',lwd=2,lty=2)

plot(16:33,apply(output.arr.ATT.2[,7,] - output.arr.ATT[,7,],2,mean,na.rm=T),
     type='l',lwd=2,ylim=c(-0.05,0.05),main='enrolment',xlab='Age',ylab='Effect')
lines(16:33,apply(output.arr.ATT.3[,7,]  - output.arr.ATT[,7,],2,mean)) 

# the two outcomes that we actuall care about
# employment
plot(16:33,apply(output.arr.ATT.2[,12,] - output.arr.ATT[,12,],2,mean,na.rm=T),
     type='l',lwd=2,ylim=c(-0.05,0.10),main='employment',xlab='Age',ylab='Effect')
lines(16:33,apply(output.arr.ATT.3[,12,]  - output.arr.ATT[,12,],2,mean)) 

# income
plot(16:33,apply(output.arr.ATT.2[,14,] - output.arr.ATT[,14,],2,mean,na.rm=T),
     type='l',lwd=2,ylim=c(-0.1,1.5),main='income',xlab='Age',ylab='Effect')
lines(16:33,apply(output.arr.ATT.3[,14,]  - output.arr.ATT[,14,],2,mean)) 

# TE, DE, CDE for employment
emp.TE <- apply(output.arr.ATT.2[,12,] - output.arr.ATT[,12,],2,mean,na.rm=T)
emp.DE <- apply(output.arr.ATT.3[,12,]  - output.arr.ATT[,12,],2,mean)
emp.IE <- emp.TE - emp.DE

# 95% CIs
emp.TE.95 <- apply(output.arr.ATT.2[,12,] - output.arr.ATT[,12,],2,quantile,probs=c(0.025,0.975),na.rm=T)
emp.DE.95 <- apply(output.arr.ATT.3[,12,]  - output.arr.ATT[,12,],2,quantile,probs=c(0.025,0.975),na.rm=T)
emp.IE.95 <- apply((output.arr.ATT.2[,12,] - output.arr.ATT[,12,]) - 
                     (output.arr.ATT.3[,12,]  - output.arr.ATT[,12,]),2,quantile,probs=c(0.025,0.975),na.rm=T)

# TE, DE, CDE for income
inc.TE <- apply(output.arr.ATT.2[,14,] - output.arr.ATT[,14,],2,mean,na.rm=T)
inc.DE <- apply(output.arr.ATT.3[,14,]  - output.arr.ATT[,14,],2,mean)
inc.IE <- inc.TE - inc.DE

# 95% CIs
inc.TE.95 <- apply(output.arr.ATT.2[,14,] - output.arr.ATT[,14,],2,quantile,probs=c(0.025,0.975),na.rm=T)
inc.DE.95 <- apply(output.arr.ATT.3[,14,]  - output.arr.ATT[,14,],2,quantile,probs=c(0.025,0.975),na.rm=T)
inc.IE.95 <- apply((output.arr.ATT.2[,14,] - output.arr.ATT[,14,]) - 
                     (output.arr.ATT.3[,14,]  - output.arr.ATT[,14,]),2,quantile,probs=c(0.025,0.975),na.rm=T)

## and now the PA ones
# income
plot(16:33,apply(output.arr.PA.2[,14,] - output.arr.PA[,14,],2,mean,na.rm=T),
     type='l',lwd=2,ylim=c(-0.1,1.5),main='income',xlab='Age',ylab='Effect')
lines(16:33,apply(output.arr.PA.3[,14,]  - output.arr.PA[,14,],2,mean)) 

# TE, DE, CDE for employment
emp.TE.PA <- apply(output.arr.PA.2[,12,] - output.arr.PA[,12,],2,mean,na.rm=T)
emp.DE.PA <- apply(output.arr.PA.3[,12,] - output.arr.PA[,12,],2,mean)
emp.IE.PA <- emp.TE.PA - emp.DE.PA

# 95% CIs
emp.TE.95.PA <- apply(output.arr.PA.2[,12,] - output.arr.PA[,12,],2,quantile,probs=c(0.025,0.975),na.rm=T)
emp.DE.95.PA <- apply(output.arr.PA.3[,12,]  - output.arr.PA[,12,],2,quantile,probs=c(0.025,0.975),na.rm=T)
emp.IE.95.PA <- apply((output.arr.PA.2[,12,] - output.arr.PA[,12,]) - 
                        (output.arr.PA.3[,12,]  - output.arr.PA[,12,]),2,quantile,probs=c(0.025,0.975),na.rm=T)

# TE, DE, CDE for income
inc.TE.PA <- apply(output.arr.PA.2[,14,] - output.arr.PA[,14,],2,mean,na.rm=T)
inc.DE.PA <- apply(output.arr.PA.3[,14,]  - output.arr.PA[,14,],2,mean)
inc.IE.PA <- inc.TE.PA - inc.DE.PA

# 95% CIs
inc.TE.95.PA <- apply(output.arr.PA.2[,14,] - output.arr.PA[,14,],2,quantile,probs=c(0.025,0.975),na.rm=T)
inc.DE.95.PA <- apply(output.arr.PA.3[,14,]  - output.arr.PA[,14,],2,quantile,probs=c(0.025,0.975),na.rm=T)
inc.IE.95.PA <- apply((output.arr.PA.2[,14,] - output.arr.PA[,14,]) - 
                        (output.arr.PA.3[,14,]  - output.arr.PA[,14,]),2,quantile,probs=c(0.025,0.975),na.rm=T)

# append everything and put in .CSV file
# TE, DE, CDE for employment
x <- rbind(
  16:33,
  emp.TE,
  emp.TE.95,
  emp.DE,
  emp.DE.95,
  emp.IE,
  emp.IE.95,
  inc.TE,
  inc.TE.95,
  inc.DE,
  inc.DE.95,
  inc.IE,
  inc.IE.95)

# write.csv(x,file='mediation_females_ATT.csv')
# write.csv(x,file='mediation_males_ATT.csv')

x <- rbind(
  16:33,
  emp.TE.PA,
  emp.TE.95.PA,
  emp.DE.PA,
  emp.DE.95.PA,
  emp.IE.PA,
  emp.IE.95.PA,
  inc.TE.PA,
  inc.TE.95.PA,
  inc.DE.PA,
  inc.DE.95.PA,
  inc.IE.PA,
  inc.IE.95.PA)

# write.csv(x,file='mediation_females_PA.csv')
# write.csv(x,file='mediation_males_PA.csv')







## Pop averaged TOTAL EFFECT results
variable <- c('edu prime','95%CI upper','95%CI lower',
              'edu second','95%CI upper','95%CI lower',
              'edu tert low','95%CI upper','95%CI lower',
              'edu tert high','95%CI upper','95%CI lower',
              'edu tert combined','95%CI upper','95%CI lower',
              'enrolment','95%CI upper','95%CI lower',
              'employment','95%CI upper','95%CI lower',
              'income','95%CI upper','95%CI lower')
mat.fem.PAE <- matrix(rep(NA,length(variable)*18),ncol=18)
mat.fem.PAE <- data.frame(mat.fem.PAE)

mat.fem.PAE[1,] <- apply(output.arr.PA.2[,2,] - output.arr.PA[,2,],2,mean,na.rm=T)
mat.fem.PAE[2,] <- apply(output.arr.PA.2[,2,] - output.arr.PA[,2,],2,quantile,probs=0.975,na.rm=T)
mat.fem.PAE[3,] <- apply(output.arr.PA.2[,2,] - output.arr.PA[,2,],2,quantile,probs=0.025,na.rm=T)

mat.fem.PAE[4,] <- apply(output.arr.PA.2[,3,] - output.arr.PA[,3,],2,mean,na.rm=T)
mat.fem.PAE[5,] <- apply(output.arr.PA.2[,3,] - output.arr.PA[,3,],2,quantile,probs=0.975,na.rm=T)
mat.fem.PAE[6,] <- apply(output.arr.PA.2[,3,] - output.arr.PA[,3,],2,quantile,probs=0.025,na.rm=T)

mat.fem.PAE[7,] <- apply(output.arr.PA.2[,5,] - output.arr.PA[,5,],2,mean,na.rm=T)
mat.fem.PAE[8,] <- apply(output.arr.PA.2[,5,] - output.arr.PA[,5,],2,quantile,probs=0.975,na.rm=T)
mat.fem.PAE[9,] <- apply(output.arr.PA.2[,5,] - output.arr.PA[,5,],2,quantile,probs=0.025,na.rm=T)

mat.fem.PAE[10,] <- apply(output.arr.PA.2[,6,] - output.arr.PA[,6,],2,mean,na.rm=T)
mat.fem.PAE[11,] <- apply(output.arr.PA.2[,6,] - output.arr.PA[,6,],2,quantile,probs=0.975,na.rm=T)
mat.fem.PAE[12,] <- apply(output.arr.PA.2[,6,] - output.arr.PA[,6,],2,quantile,probs=0.025,na.rm=T)

# combine lower tert and higher tert into combined tert
tertcomb.arr.PA.2 <- output.arr.PA.2[,5,] + output.arr.PA.2[,6,]
tertcomb.arr.PA <- output.arr.PA[,5,] + output.arr.PA[,6,]

mat.fem.PAE[13,] <- apply(tertcomb.arr.PA.2 - tertcomb.arr.PA,2,mean,na.rm=T)
mat.fem.PAE[14,] <- apply(tertcomb.arr.PA.2 - tertcomb.arr.PA,2,quantile,probs=0.975,na.rm=T)
mat.fem.PAE[15,] <- apply(tertcomb.arr.PA.2 - tertcomb.arr.PA,2,quantile,probs=0.025,na.rm=T)

mat.fem.PAE[16,] <- apply(output.arr.PA.2[,7,] - output.arr.PA[,7,],2,mean,na.rm=T)
mat.fem.PAE[17,] <- apply(output.arr.PA.2[,7,] - output.arr.PA[,7,],2,quantile,probs=0.975,na.rm=T)
mat.fem.PAE[18,] <- apply(output.arr.PA.2[,7,] - output.arr.PA[,7,],2,quantile,probs=0.025,na.rm=T)

mat.fem.PAE[19,] <- apply(output.arr.PA.2[,12,] - output.arr.PA[,12,],2,mean,na.rm=T)
mat.fem.PAE[20,] <- apply(output.arr.PA.2[,12,] - output.arr.PA[,12,],2,quantile,probs=0.975,na.rm=T)
mat.fem.PAE[21,] <- apply(output.arr.PA.2[,12,] - output.arr.PA[,12,],2,quantile,probs=0.025,na.rm=T)

mat.fem.PAE[22,] <- apply(output.arr.PA.2[,14,] - output.arr.PA[,14,],2,mean,na.rm=T)
mat.fem.PAE[23,] <- apply(output.arr.PA.2[,14,] - output.arr.PA[,14,],2,quantile,probs=0.975,na.rm=T)
mat.fem.PAE[24,] <- apply(output.arr.PA.2[,14,] - output.arr.PA[,14,],2,quantile,probs=0.025,na.rm=T)

names(mat.fem.PAE) <- as.character(16:33)

mat.fem.PAE <- cbind(variable,mat.fem.PAE)
write.csv(mat.fem.PAE,file='mat.fem.labour.PAE.total.csv')




## Pop averaged DIRECT EFFECT results
variable <- c('edu prime','95%CI upper','95%CI lower',
              'edu second','95%CI upper','95%CI lower',
              'edu tert low','95%CI upper','95%CI lower',
              'edu tert high','95%CI upper','95%CI lower',
              'edu tert combined','95%CI upper','95%CI lower',
              'enrolment','95%CI upper','95%CI lower',
              'employment','95%CI upper','95%CI lower',
              'income','95%CI upper','95%CI lower')
mat.fem.PAE <- matrix(rep(NA,length(variable)*18),ncol=18)
mat.fem.PAE <- data.frame(mat.fem.PAE)

mat.fem.PAE[1,] <- apply(output.arr.PA.3[,2,] - output.arr.PA[,2,],2,mean,na.rm=T)
mat.fem.PAE[2,] <- apply(output.arr.PA.3[,2,] - output.arr.PA[,2,],2,quantile,probs=0.975,na.rm=T)
mat.fem.PAE[3,] <- apply(output.arr.PA.3[,2,] - output.arr.PA[,2,],2,quantile,probs=0.025,na.rm=T)

mat.fem.PAE[4,] <- apply(output.arr.PA.3[,3,] - output.arr.PA[,3,],2,mean,na.rm=T)
mat.fem.PAE[5,] <- apply(output.arr.PA.3[,3,] - output.arr.PA[,3,],2,quantile,probs=0.975,na.rm=T)
mat.fem.PAE[6,] <- apply(output.arr.PA.3[,3,] - output.arr.PA[,3,],2,quantile,probs=0.025,na.rm=T)

mat.fem.PAE[7,] <- apply(output.arr.PA.3[,5,] - output.arr.PA[,5,],2,mean,na.rm=T)
mat.fem.PAE[8,] <- apply(output.arr.PA.3[,5,] - output.arr.PA[,5,],2,quantile,probs=0.975,na.rm=T)
mat.fem.PAE[9,] <- apply(output.arr.PA.3[,5,] - output.arr.PA[,5,],2,quantile,probs=0.025,na.rm=T)

mat.fem.PAE[10,] <- apply(output.arr.PA.3[,6,] - output.arr.PA[,6,],2,mean,na.rm=T)
mat.fem.PAE[11,] <- apply(output.arr.PA.3[,6,] - output.arr.PA[,6,],2,quantile,probs=0.975,na.rm=T)
mat.fem.PAE[12,] <- apply(output.arr.PA.3[,6,] - output.arr.PA[,6,],2,quantile,probs=0.025,na.rm=T)

# combine lower tert and higher tert into combined tert
tertcomb.arr.PA.3 <- output.arr.PA.3[,5,] + output.arr.PA.3[,6,]
tertcomb.arr.PA <- output.arr.PA[,5,] + output.arr.PA[,6,]

mat.fem.PAE[13,] <- apply(tertcomb.arr.PA.3 - tertcomb.arr.PA,2,mean,na.rm=T)
mat.fem.PAE[14,] <- apply(tertcomb.arr.PA.3 - tertcomb.arr.PA,2,quantile,probs=0.975,na.rm=T)
mat.fem.PAE[15,] <- apply(tertcomb.arr.PA.3 - tertcomb.arr.PA,2,quantile,probs=0.025,na.rm=T)

mat.fem.PAE[16,] <- apply(output.arr.PA.3[,7,] - output.arr.PA[,7,],2,mean,na.rm=T)
mat.fem.PAE[17,] <- apply(output.arr.PA.3[,7,] - output.arr.PA[,7,],2,quantile,probs=0.975,na.rm=T)
mat.fem.PAE[18,] <- apply(output.arr.PA.3[,7,] - output.arr.PA[,7,],2,quantile,probs=0.025,na.rm=T)

mat.fem.PAE[19,] <- apply(output.arr.PA.3[,12,] - output.arr.PA[,12,],2,mean,na.rm=T)
mat.fem.PAE[20,] <- apply(output.arr.PA.3[,12,] - output.arr.PA[,12,],2,quantile,probs=0.975,na.rm=T)
mat.fem.PAE[21,] <- apply(output.arr.PA.3[,12,] - output.arr.PA[,12,],2,quantile,probs=0.025,na.rm=T)

mat.fem.PAE[22,] <- apply(output.arr.PA.3[,14,] - output.arr.PA[,14,],2,mean,na.rm=T)
mat.fem.PAE[23,] <- apply(output.arr.PA.3[,14,] - output.arr.PA[,14,],2,quantile,probs=0.975,na.rm=T)
mat.fem.PAE[24,] <- apply(output.arr.PA.3[,14,] - output.arr.PA[,14,],2,quantile,probs=0.025,na.rm=T)

names(mat.fem.PAE) <- as.character(16:33)

mat.fem.PAE <- cbind(variable,mat.fem.PAE)
write.csv(mat.fem.PAE,file='mat.fem.labour.PAE.direct.csv')



## Pop averaged INDIRECT EFFECT results
variable <- c('edu prime','95%CI upper','95%CI lower',
              'edu second','95%CI upper','95%CI lower',
              'edu tert low','95%CI upper','95%CI lower',
              'edu tert high','95%CI upper','95%CI lower',
              'edu tert combined','95%CI upper','95%CI lower',
              'enrolment','95%CI upper','95%CI lower',
              'employment','95%CI upper','95%CI lower',
              'income','95%CI upper','95%CI lower')
mat.fem.PAE <- matrix(rep(NA,length(variable)*18),ncol=18)
mat.fem.PAE <- data.frame(mat.fem.PAE)

mat.fem.PAE[1,] <- apply(output.arr.PA.2[,2,] - output.arr.PA.3[,2,],2,mean,na.rm=T)
mat.fem.PAE[2,] <- apply(output.arr.PA.2[,2,] - output.arr.PA.3[,2,],2,quantile,probs=0.975,na.rm=T)
mat.fem.PAE[3,] <- apply(output.arr.PA.2[,2,] - output.arr.PA.3[,2,],2,quantile,probs=0.025,na.rm=T)

mat.fem.PAE[4,] <- apply(output.arr.PA.2[,3,] - output.arr.PA.3[,3,],2,mean,na.rm=T)
mat.fem.PAE[5,] <- apply(output.arr.PA.2[,3,] - output.arr.PA.3[,3,],2,quantile,probs=0.975,na.rm=T)
mat.fem.PAE[6,] <- apply(output.arr.PA.2[,3,] - output.arr.PA.3[,3,],2,quantile,probs=0.025,na.rm=T)

mat.fem.PAE[7,] <- apply(output.arr.PA.2[,5,] - output.arr.PA.3[,5,],2,mean,na.rm=T)
mat.fem.PAE[8,] <- apply(output.arr.PA.2[,5,] - output.arr.PA.3[,5,],2,quantile,probs=0.975,na.rm=T)
mat.fem.PAE[9,] <- apply(output.arr.PA.2[,5,] - output.arr.PA.3[,5,],2,quantile,probs=0.025,na.rm=T)

mat.fem.PAE[10,] <- apply(output.arr.PA.2[,6,] - output.arr.PA.3[,6,],2,mean,na.rm=T)
mat.fem.PAE[11,] <- apply(output.arr.PA.2[,6,] - output.arr.PA.3[,6,],2,quantile,probs=0.975,na.rm=T)
mat.fem.PAE[12,] <- apply(output.arr.PA.2[,6,] - output.arr.PA.3[,6,],2,quantile,probs=0.025,na.rm=T)

# combine lower tert and higher tert into combined tert
tertcomb.arr.PA.3 <- output.arr.PA.3[,5,] + output.arr.PA.3[,6,]
tertcomb.arr.PA.2 <- output.arr.PA.2[,5,] + output.arr.PA.2[,6,]

mat.fem.PAE[13,] <- apply(tertcomb.arr.PA.2 - tertcomb.arr.PA.3,2,mean,na.rm=T)
mat.fem.PAE[14,] <- apply(tertcomb.arr.PA.2 - tertcomb.arr.PA.3,2,quantile,probs=0.975,na.rm=T)
mat.fem.PAE[15,] <- apply(tertcomb.arr.PA.2 - tertcomb.arr.PA.3,2,quantile,probs=0.025,na.rm=T)

mat.fem.PAE[16,] <- apply(output.arr.PA.2[,7,] - output.arr.PA.3[,7,],2,mean,na.rm=T)
mat.fem.PAE[17,] <- apply(output.arr.PA.2[,7,] - output.arr.PA.3[,7,],2,quantile,probs=0.975,na.rm=T)
mat.fem.PAE[18,] <- apply(output.arr.PA.2[,7,] - output.arr.PA.3[,7,],2,quantile,probs=0.025,na.rm=T)

mat.fem.PAE[19,] <- apply(output.arr.PA.2[,12,] - output.arr.PA.3[,12,],2,mean,na.rm=T)
mat.fem.PAE[20,] <- apply(output.arr.PA.2[,12,] - output.arr.PA.3[,12,],2,quantile,probs=0.975,na.rm=T)
mat.fem.PAE[21,] <- apply(output.arr.PA.2[,12,] - output.arr.PA.3[,12,],2,quantile,probs=0.025,na.rm=T)

mat.fem.PAE[22,] <- apply(output.arr.PA.2[,14,] - output.arr.PA.3[,14,],2,mean,na.rm=T)
mat.fem.PAE[23,] <- apply(output.arr.PA.2[,14,] - output.arr.PA.3[,14,],2,quantile,probs=0.975,na.rm=T)
mat.fem.PAE[24,] <- apply(output.arr.PA.2[,14,] - output.arr.PA.3[,14,],2,quantile,probs=0.025,na.rm=T)

names(mat.fem.PAE) <- as.character(16:33)

mat.fem.PAE <- cbind(variable,mat.fem.PAE)
write.csv(mat.fem.PAE,file='mat.fem.labour.PAE.indirect.csv')





# population averaged both curves + empirical
# produce empirical curves
## comparison to empirical data
birth.emp <- edu.pri.emp <- edu.sec.emp <-  edu.tertlowst.emp <- 
  edu.tertlow.emp <- edu.terthi.emp <- enrol.emp <-
  enrolc.emp <- partner.single.emp <- partner.cohab.emp <-
  partner.married.emp <- empl.emp  <- unempl.emp <-
  inc.emp <- hhinc.emp <-  house.parents.emp <-
  house.owner.emp <- house.tenant.emp  <- house.other.emp <-
  cens.emp <- 
  NULL

for(a in sort(unique(par.dat$age))) {
  birth.emp[a-15] <- mean(par.dat$birth[par.dat$age==a],na.rm=T)
  edu.pri.emp[a-15] <- mean(par.dat$edu.pri[par.dat$age==a],na.rm=T)
  edu.sec.emp[a-15] <- mean(par.dat$edu.sec[par.dat$age==a],na.rm=T) 
  edu.tertlowst.emp[a-15] <- mean(par.dat$edu.tertlowst[par.dat$age==a],na.rm=T) 
  edu.tertlow.emp[a-15] <- mean(par.dat$edu.tertlow[par.dat$age==a],na.rm=T) 
  edu.terthi.emp[a-15] <- mean(par.dat$edu.terthi[par.dat$age==a],na.rm=T) 
  enrol.emp[a-15] <- mean(par.dat$enrol[par.dat$age==a],na.rm=T) 
  enrolc.emp[a-15] <- mean(par.dat$enrolc[par.dat$age==a],na.rm=T)
  partner.single.emp[a-15] <- mean(par.dat$partner.single[par.dat$age==a],na.rm=T) 
  partner.cohab.emp[a-15] <- mean(par.dat$partner.cohab[par.dat$age==a],na.rm=T) 
  partner.married.emp[a-15] <- mean(par.dat$partner.married[par.dat$age==a],na.rm=T) 
  empl.emp[a-15] <- mean(par.dat$empl[par.dat$age==a],na.rm=T) 
  unempl.emp[a-15] <- mean(par.dat$unempl[par.dat$age==a],na.rm=T) 
  inc.emp[a-15] <- mean(par.dat$inc[par.dat$age==a],na.rm=T) 
  hhinc.emp[a-15] <- mean(par.dat$inc.res[par.dat$age==a],na.rm=T) 
  house.parents.emp[a-15] <- mean(par.dat$house.parents[par.dat$age==a],na.rm=T) 
  house.owner.emp[a-15] <- mean(par.dat$house.owner[par.dat$age==a],na.rm=T) 
  house.tenant.emp[a-15] <- mean(par.dat$house.tenant[par.dat$age==a],na.rm=T) 
  house.other.emp[a-15] <- mean(par.dat$house.other[par.dat$age==a],na.rm=T)
  cens.emp[a-15] <- mean(par.dat$cens[par.dat$age==a],na.rm=T) 
}

variable <- c('birth','edu prime','edu second','edu tert','enrolment','employment','income')
variable2 <- rep(variable,each=4)
CFind <- rep(c('EMP','NC','CF','CF direct'),length(variable))
mat.fem.PA <- matrix(rep(NA,length(variable2)*17),ncol=17)
mat.fem.PA <- data.frame(mat.fem.PA)

# birth
mat.fem.PA[1,] <- birth.emp
mat.fem.PA[2,] <- apply(output.arr.PA[,1,],2,mean,na.rm=T)[-18]
mat.fem.PA[3,] <- apply(output.arr.PA.2[,1,],2,mean,na.rm=T)[-18]
mat.fem.PA[4,] <- apply(output.arr.PA.3[,1,],2,mean,na.rm=T)[-18]

# edu prime
mat.fem.PA[5,] <- edu.pri.emp
mat.fem.PA[6,] <- apply(output.arr.PA[,2,],2,mean,na.rm=T)[-18]
mat.fem.PA[7,] <- apply(output.arr.PA.2[,2,],2,mean,na.rm=T)[-18]
mat.fem.PA[8,] <- apply(output.arr.PA.3[,2,],2,mean,na.rm=T)[-18]

# edu second
mat.fem.PA[9,] <- edu.sec.emp
mat.fem.PA[10,] <- apply(output.arr.PA[,3,],2,mean,na.rm=T)[-18]
mat.fem.PA[11,] <- apply(output.arr.PA.2[,3,],2,mean,na.rm=T)[-18]
mat.fem.PA[12,] <- apply(output.arr.PA.3[,3,],2,mean,na.rm=T)[-18]

# combine lower tert and higher tert into combined tert
tertcomb.arr.PA <- output.arr.PA[,5,] + output.arr.PA[,6,]
tertcomb.arr.PA.2 <- output.arr.PA.2[,5,] + output.arr.PA.2[,6,]
tertcomb.arr.PA.3 <- output.arr.PA.3[,5,] + output.arr.PA.3[,6,]

# edu tert low + high
mat.fem.PA[13,] <- edu.tertlow.emp + edu.terthi.emp
mat.fem.PA[14,] <- apply(tertcomb.arr.PA,2,mean,na.rm=T)[-18]
mat.fem.PA[15,] <- apply(tertcomb.arr.PA.2,2,mean,na.rm=T)[-18]
mat.fem.PA[16,] <- apply(tertcomb.arr.PA.3,2,mean,na.rm=T)[-18]

# enrolment
mat.fem.PA[17,] <- enrol.emp
mat.fem.PA[18,] <- apply(output.arr.PA[,7,],2,mean,na.rm=T)[-18]
mat.fem.PA[19,] <- apply(output.arr.PA.2[,7,],2,mean,na.rm=T)[-18]
mat.fem.PA[20,] <- apply(output.arr.PA.3[,7,],2,mean,na.rm=T)[-18]

# employment
mat.fem.PA[21,] <- empl.emp
mat.fem.PA[22,] <- apply(output.arr.PA[,12,],2,mean,na.rm=T)[-18]
mat.fem.PA[23,] <- apply(output.arr.PA.2[,12,],2,mean,na.rm=T)[-18]
mat.fem.PA[24,] <- apply(output.arr.PA.3[,12,],2,mean,na.rm=T)[-18]

# income
mat.fem.PA[25,] <- inc.emp
mat.fem.PA[26,] <- apply(output.arr.PA[,14,],2,mean,na.rm=T)[-18]
mat.fem.PA[27,] <- apply(output.arr.PA.2[,14,],2,mean,na.rm=T)[-18]
mat.fem.PA[28,] <- apply(output.arr.PA.3[,14,],2,mean,na.rm=T)[-18]

mat.fem.PA <- cbind(variable2,CFind,mat.fem.PA)
write.csv(mat.fem.PA,file='mat.fem.labour.PA.csv')



## ATT Curves
variable <- c('birth','edu prime','edu second','edu tert','enrolment','employment','income')
variable2 <- rep(variable,each=3)
CFind <- rep(c('Mean','Lower 95% bound','Upper 95% bound'),length(variable))
mat.fem.ATT <- matrix(rep(NA,length(variable2)*17),ncol=17)
mat.fem.ATT <- data.frame(mat.fem.ATT)

# birth
mat.fem.ATT[1,] <- apply(output.arr.ATT.2[,1,] - output.arr.ATT[,1,],2,mean,na.rm=T)[-18]
mat.fem.ATT[2,] <- apply(output.arr.ATT.2[,1,] - output.arr.ATT[,1,],2,quantile,probs=c(0.025),na.rm=T)[-18]
mat.fem.ATT[3,] <- apply(output.arr.ATT.2[,1,] - output.arr.ATT[,1,],2,quantile,probs=c(0.975),na.rm=T)[-18]

# edu prime
mat.fem.ATT[4,] <- apply(output.arr.ATT.2[,2,] - output.arr.ATT[,2,],2,mean,na.rm=T)[-18]
mat.fem.ATT[5,] <- apply(output.arr.ATT.2[,2,] - output.arr.ATT[,2,],2,quantile,probs=c(0.025),na.rm=T)[-18]
mat.fem.ATT[6,] <- apply(output.arr.ATT.2[,2,] - output.arr.ATT[,2,],2,quantile,probs=c(0.975),na.rm=T)[-18]

# edu second
mat.fem.ATT[7,] <- apply(output.arr.ATT.2[,3,] - output.arr.ATT[,3,],2,mean,na.rm=T)[-18]
mat.fem.ATT[8,] <- apply(output.arr.ATT.2[,3,] - output.arr.ATT[,3,],2,quantile,probs=c(0.025),na.rm=T)[-18]
mat.fem.ATT[9,] <- apply(output.arr.ATT.2[,3,] - output.arr.ATT[,3,],2,quantile,probs=c(0.975),na.rm=T)[-18]

# combine lower tert and higher tert into combined tert
tertcomb.arr.ATT <- output.arr.ATT[,5,] + output.arr.ATT[,6,]
tertcomb.arr.ATT.2 <- output.arr.ATT.2[,5,] + output.arr.ATT.2[,6,]
tertcomb.arr.ATT.3 <- output.arr.ATT.3[,5,] + output.arr.ATT.3[,6,]

# edu tert low + high
mat.fem.ATT[10,] <- apply(tertcomb.arr.ATT.2 - tertcomb.arr.ATT,2,mean,na.rm=T)[-18]
mat.fem.ATT[11,] <- apply(tertcomb.arr.ATT.2 - tertcomb.arr.ATT,2,quantile,probs=c(0.025),na.rm=T)[-18]
mat.fem.ATT[12,] <- apply(tertcomb.arr.ATT.2 - tertcomb.arr.ATT,2,quantile,probs=c(0.975),na.rm=T)[-18]

# enrolment
mat.fem.ATT[13,] <- apply(output.arr.ATT.2[,7,] - output.arr.ATT[,7,],2,mean,na.rm=T)[-18]
mat.fem.ATT[14,] <- apply(output.arr.ATT.2[,7,] - output.arr.ATT[,7,],2,quantile,probs=c(0.025),na.rm=T)[-18]
mat.fem.ATT[15,] <- apply(output.arr.ATT.2[,7,] - output.arr.ATT[,7,],2,quantile,probs=c(0.975),na.rm=T)[-18]

# employment
mat.fem.ATT[16,] <- apply(output.arr.ATT.2[,12,] - output.arr.ATT[,12,],2,mean,na.rm=T)[-18]
mat.fem.ATT[17,] <- apply(output.arr.ATT.2[,12,] - output.arr.ATT[,12,],2,quantile,probs=c(0.025),na.rm=T)[-18]
mat.fem.ATT[18,] <- apply(output.arr.ATT.2[,12,] - output.arr.ATT[,12,],2,quantile,probs=c(0.975),na.rm=T)[-18]

# income
mat.fem.ATT[19,] <- apply(output.arr.ATT.2[,14,] - output.arr.ATT[,14,],2,mean,na.rm=T)[-18]
mat.fem.ATT[20,] <- apply(output.arr.ATT.2[,14,] - output.arr.ATT[,14,],2,quantile,probs=c(0.025),na.rm=T)[-18]
mat.fem.ATT[21,] <- apply(output.arr.ATT.2[,14,] - output.arr.ATT[,14,],2,quantile,probs=c(0.975),na.rm=T)[-18]

mat.fem.ATT <- cbind(variable2,CFind,mat.fem.ATT)
write.csv(mat.fem.ATT,file='mat.fem.ATT.csv')





variable <- c('birth','edu prime','edu second','edu tert','enrolment','employment','income')
variable3 <- rep(variable,each=3)
CIind <- rep(c('estimate','CIlow','CIup'),length(variable))
mat.fem.ef <- matrix(rep(NA,length(variable3)*18),ncol=18)
mat.fem.ef <- data.frame(mat.fem.ef)


## DIRECT effects for youngparents
# birth
mat.fem.ef[1,] <- apply(youngparent.monte.arr.mediation[,1,],2,mean,na.rm=T)
mat.fem.ef[2,] <- apply(youngparent.monte.arr.mediation[,1,],2,mean,na.rm=T)
mat.fem.ef[3,] <- apply(youngparent.monte.arr.mediation[,1,],2,mean,na.rm=T)

# edu prime
mat.fem.ef[4,] <- apply(youngparent.monte.arr.mediation[,2,],2,mean,na.rm=T)
mat.fem.ef[5,] <- apply(youngparent.monte.arr.mediation[,2,],2,quantile,probs=c(0.025),na.rm=T)
mat.fem.ef[6,] <- apply(youngparent.monte.arr.mediation[,2,],2,quantile,probs=c(0.975),na.rm=T)

# edu second
mat.fem.ef[7,] <- apply(youngparent.monte.arr.mediation[,3,],2,mean,na.rm=T)
mat.fem.ef[8,] <- apply(youngparent.monte.arr.mediation[,3,],2,quantile,probs=c(0.025),na.rm=T)
mat.fem.ef[9,] <- apply(youngparent.monte.arr.mediation[,3,],2,quantile,probs=c(0.975),na.rm=T)

# edu tert low + high
mat.fem.ef[10,] <- apply(youngparent.monte.arr.mediation[,5,] + youngparent.monte.arr.mediation[,6,],2,mean,na.rm=T)
mat.fem.ef[11,] <- apply(youngparent.monte.arr.mediation[,5,] + youngparent.monte.arr.mediation[,6,],2,quantile,probs=c(0.025),na.rm=T)
mat.fem.ef[12,] <- apply(youngparent.monte.arr.mediation[,5,] + youngparent.monte.arr.mediation[,6,],2,quantile,probs=c(0.975),na.rm=T)

# enrolment
mat.fem.ef[13,] <- apply(youngparent.monte.arr.mediation[,7,],2,mean,na.rm=T)
mat.fem.ef[14,] <- apply(youngparent.monte.arr.mediation[,7,],2,quantile,probs=c(0.025),na.rm=T)
mat.fem.ef[15,] <- apply(youngparent.monte.arr.mediation[,7,],2,quantile,probs=c(0.975),na.rm=T)

# employment
mat.fem.ef[16,] <- apply(youngparent.monte.arr.mediation[,12,],2,mean,na.rm=T)
mat.fem.ef[17,] <- apply(youngparent.monte.arr.mediation[,12,],2,quantile,probs=c(0.025),na.rm=T)
mat.fem.ef[18,] <- apply(youngparent.monte.arr.mediation[,12,],2,quantile,probs=c(0.975),na.rm=T)

# income
mat.fem.ef[19,] <- apply(youngparent.monte.arr.mediation[,14,],2,mean,na.rm=T)
mat.fem.ef[20,] <- apply(youngparent.monte.arr.mediation[,14,],2,quantile,probs=c(0.025),na.rm=T)
mat.fem.ef[21,] <- apply(youngparent.monte.arr.mediation[,14,],2,quantile,probs=c(0.975),na.rm=T)

mat.fem.ef <- cbind(variable3,CIind,mat.fem.ef)
write.csv(mat.fem.ef,file='mat.fem.labour.tsfb.direct.csv')


## INDIRECT Effect
## DIRECT effects for youngparents
# birth
variable <- c('birth','edu prime','edu second','edu tert','enrolment','employment','income')
variable3 <- rep(variable,each=3)
CIind <- rep(c('estimate','CIlow','CIup'),length(variable))
mat.fem.ef <- matrix(rep(NA,length(variable3)*18),ncol=18)
mat.fem.ef <- data.frame(mat.fem.ef)

mat.fem.ef[1,] <- apply(youngparent.monte.arr[,1,] - youngparent.monte.arr.mediation[,1,],2,mean,na.rm=T)
mat.fem.ef[2,] <- apply(youngparent.monte.arr[,1,] - youngparent.monte.arr.mediation[,1,],2,mean,na.rm=T)
mat.fem.ef[3,] <- apply(youngparent.monte.arr[,1,] - youngparent.monte.arr.mediation[,1,],2,mean,na.rm=T)

# edu prime
mat.fem.ef[4,] <- apply(youngparent.monte.arr[,2,] - youngparent.monte.arr.mediation[,2,],2,mean,na.rm=T)
mat.fem.ef[5,] <- apply(youngparent.monte.arr[,2,] - youngparent.monte.arr.mediation[,2,],2,quantile,probs=c(0.025),na.rm=T)
mat.fem.ef[6,] <- apply(youngparent.monte.arr[,2,] - youngparent.monte.arr.mediation[,2,],2,quantile,probs=c(0.975),na.rm=T)

# edu second
mat.fem.ef[7,] <- apply(youngparent.monte.arr[,3,] - youngparent.monte.arr.mediation[,3,],2,mean,na.rm=T)
mat.fem.ef[8,] <- apply(youngparent.monte.arr[,3,] - youngparent.monte.arr.mediation[,3,],2,quantile,probs=c(0.025),na.rm=T)
mat.fem.ef[9,] <- apply(youngparent.monte.arr[,3,] - youngparent.monte.arr.mediation[,3,],2,quantile,probs=c(0.975),na.rm=T)

# edu tert low + high
mat.fem.ef[10,] <- apply((youngparent.monte.arr[,5,] + youngparent.monte.arr[,6,]) - (youngparent.monte.arr.mediation[,5,] + youngparent.monte.arr.mediation[,6,]),2,mean,na.rm=T)
mat.fem.ef[11,] <- apply((youngparent.monte.arr[,5,] + youngparent.monte.arr[,6,]) - (youngparent.monte.arr.mediation[,5,] + youngparent.monte.arr.mediation[,6,]),2,quantile,probs=c(0.025),na.rm=T)
mat.fem.ef[12,] <- apply((youngparent.monte.arr[,5,] + youngparent.monte.arr[,6,]) - (youngparent.monte.arr.mediation[,5,] + youngparent.monte.arr.mediation[,6,]),2,quantile,probs=c(0.975),na.rm=T)

# enrolment
mat.fem.ef[13,] <- apply(youngparent.monte.arr[,7,] - youngparent.monte.arr.mediation[,7,],2,mean,na.rm=T)
mat.fem.ef[14,] <- apply(youngparent.monte.arr[,7,] - youngparent.monte.arr.mediation[,7,],2,quantile,probs=c(0.025),na.rm=T)
mat.fem.ef[15,] <- apply(youngparent.monte.arr[,7,] - youngparent.monte.arr.mediation[,7,],2,quantile,probs=c(0.975),na.rm=T)

# employment
mat.fem.ef[16,] <- apply(youngparent.monte.arr[,12,] - youngparent.monte.arr.mediation[,12,],2,mean,na.rm=T)
mat.fem.ef[17,] <- apply(youngparent.monte.arr[,12,] - youngparent.monte.arr.mediation[,12,],2,quantile,probs=c(0.025),na.rm=T)
mat.fem.ef[18,] <- apply(youngparent.monte.arr[,12,] - youngparent.monte.arr.mediation[,12,],2,quantile,probs=c(0.975),na.rm=T)

# income
mat.fem.ef[19,] <- apply(youngparent.monte.arr[,14,] - youngparent.monte.arr.mediation[,14,],2,mean,na.rm=T)
mat.fem.ef[20,] <- apply(youngparent.monte.arr[,14,] - youngparent.monte.arr.mediation[,14,],2,quantile,probs=c(0.025),na.rm=T)
mat.fem.ef[21,] <- apply(youngparent.monte.arr[,14,] - youngparent.monte.arr.mediation[,14,],2,quantile,probs=c(0.975),na.rm=T)

mat.fem.ef <- cbind(variable3,CIind,mat.fem.ef)
write.csv(mat.fem.ef,file='mat.fem.labour.tsfb.indirect.csv')



## timetest total effect

# education joined
vec <- matrix(NA,ncol=3)
vec[,1] <- mean(youngparent.timetest[,5] + youngparent.timetest[,6])
vec[,2:3] <- quantile(youngparent.timetest[,5] + youngparent.timetest[,6],probs=c(0.025,0.975))
vec <- data.frame(vec)

timetest.total <- cbind(apply(youngparent.timetest,MARGIN=2,mean,na.rm=T),
                         apply(youngparent.timetest,MARGIN=2,quantile,probs=c(0.025)),
                         apply(youngparent.timetest,MARGIN=2,quantile,probs=c(0.975)))
timetest <- data.frame(timetest.total)
timetest[20,] <- vec
names(timetest) <- c("effect","95% CI low","95% CI up")
timetest$sig05 <- !(timetest[,2] <= 0 & timetest[,3] >= 0)
timetest
outcome <- c("birth","edu prime","edu sec","edu tert low","edu tert high",
             "enrolment","cumulative enrolment",
             "partner single","partner cohab","partner married",
             "employment","unemployment",
             "income","household income",
             "house with parents","house owner",
             "house tenant","house other","edu tert comb")
timetest <- cbind(outcome,timetest[-4,])
timetest
write.csv(timetest,'timetest_total_male.csv')


## timetest direct effect LABOUR
vec <- matrix(NA,ncol=3)
vec[,1] <- mean(youngparent.timetest.mediation[,5] + youngparent.timetest.mediation[,6])
vec[,2:3] <- quantile(youngparent.timetest.mediation[,5] + youngparent.timetest.mediation[,6],probs=c(0.025,0.975))
vec <- data.frame(vec)

timetest.mediation.total <- cbind(apply(youngparent.timetest.mediation,MARGIN=2,mean,na.rm=T),
                        apply(youngparent.timetest.mediation,MARGIN=2,quantile,probs=c(0.025)),
                        apply(youngparent.timetest.mediation,MARGIN=2,quantile,probs=c(0.975)))
timetest.mediation <- data.frame(timetest.mediation.total)
timetest.mediation[20,] <- vec
names(timetest.mediation) <- c("effect","95% CI low","95% CI up")
timetest.mediation$sig05 <- !(timetest.mediation[,2] <= 0 & timetest.mediation[,3] >= 0)
timetest.mediation
outcome <- c("birth","edu prime","edu sec","edu tert low","edu tert high",
             "enrolment","cumulative enrolment",
             "partner single","partner cohab","partner married",
             "employment","unemployment",
             "income","household income",
             "house with parents","house owner",
             "house tenant","house other","edu tert comb")
timetest.mediation <- cbind(outcome,timetest.mediation[-4,])
timetest.mediation
write.csv(timetest.mediation,'timetest_labour_direct_fem.csv')




# timetest indirect effects
vec <- matrix(NA,ncol=3)
vec[,1] <- mean((youngparent.timetest[,5] + youngparent.timetest[,6]) - (youngparent.timetest.mediation[,5] + youngparent.timetest.mediation[,6]))
vec[,2:3] <- quantile((youngparent.timetest[,5] + youngparent.timetest[,6]) - (youngparent.timetest.mediation[,5] + youngparent.timetest.mediation[,6]),probs=c(0.025,0.975))
vec <- data.frame(vec)

timetest.mediation.total <- cbind(apply(youngparent.timetest - youngparent.timetest.mediation,MARGIN=2,mean,na.rm=T),
                                  apply(youngparent.timetest - youngparent.timetest.mediation,MARGIN=2,quantile,probs=c(0.025)),
                                  apply(youngparent.timetest - youngparent.timetest.mediation,MARGIN=2,quantile,probs=c(0.975)))
timetest.mediation <- data.frame(timetest.mediation.total)
timetest.mediation[20,] <- vec
names(timetest.mediation) <- c("effect","95% CI low","95% CI up")
timetest.mediation$sig05 <- !(timetest.mediation[,2] <= 0 & timetest.mediation[,3] >= 0)
timetest.mediation
outcome <- c("birth","edu prime","edu sec","edu tert low","edu tert high",
             "enrolment","cumulative enrolment",
             "partner single","partner cohab","partner married",
             "employment","unemployment",
             "income","household income",
             "house with parents","house owner",
             "house tenant","house other","edu tert comb")
timetest.mediation <- cbind(outcome,timetest.mediation[-4,])
timetest.mediation
write.csv(timetest.mediation,'timetest_labour_indirect_fem.csv')
