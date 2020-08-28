# ======== Script 2: Loss Optimisation Procedure for specific Transition Rates (P_PP)
# This script implements the loss optimisation procedure presented in the accompanying academic article.
# It does so in two steps: 
#   1) generate a simple portfolio of amortising loans; 
#   2) assess the overall portfolio loss across given default thresholds, using several delinquency measures

# --- The following information is ancillary to this particular script
# a) Portfolio simulated using: MARKOVIAN DEFAULTS
# b) (k,g)-truncation is implemented, but not used
# c) This script is used to produce various results



# ====== 0. PARAMETRISATION

# -- R package setup
require(data.table)
require(dplyr)
require(ggplot2)
require(ggthemes)
require(scales)
require(extrafont)
require(RColorBrewer)
require(doBy)
require(foreach)
require(doParallel)
require(ETLUtils)
require(ffbase)
require(ff)
options(scipen=999)

# -- Compile Delinquency Calculation Functions (CD, MD/DoD)
source('DelinqM.R')

# -- Basic simulation parameters
period <- 60; # max term
n <- 10000; # number of loans to be simulated in portfolio
vec.Term <- rep(period, n); # a vector of contractual terms (for ease of calculation later); we assume all loans are equal-termed

# -- Loan parameters (assumed to be the same for every loan in this portfolio)
i.rate <- 0.2; # client interest rate (effective rate)
i.alt <- 0.07; # risk-free rate (effective rate)
RepayAmount <- 100; # monthly instalment

# -- Interest rate conversions
i_p.alt <- ((1+i.alt)^(1/12) - 1)*12; # risk-free rate (nominal rate)
i_p.rate <- ((1+i.rate)^(1/12) - 1)*12; # client interest rate (nominal rate)
vec.delta_pp <- (exp(log(1 + i.rate)/12) - 1) #used in MD/DoD delinquency calculation

# -- Portfolio loss rates
Arrears.LossRate <- 0.7; # loss rate on accumulated arrears balance
Outstanding.LossRate <- 0.4; # loss rate on outstanding expected balance (theoretical)

# -- General calculations & data structures
principal <- RepayAmount*(1-(1+i.rate)^(-period/12)) / ((1+i.rate)^(1/12)-1);
vec.Principal <- rep(principal,n); # a vector of loan principals (for ease of calculation later)
vec.Instal <- rep(RepayAmount,n); # a vector of fixed instalments (for ease of calculation later)
vec.IntRates <- rep(i_p.rate,n); # a vector of fixed interest rates (for ease of calculation later)
vec.Maturity <- rep(period,n); # a vector of loan ages (for generalization purposes), though we assume all loans are completely observed

# -- Parameters used in calculating delinquency measures
sc.Thres <- 0.9; # repayment ratio - g1
sc.DelinqSens <- 1; # delinquency sensitivity - g3
sc.maxLoan <- 5000; # maximum loan size offered by lender - g3
vec.DoD.lambda <- sc.DelinqSens * (1-((sc.maxLoan-vec.Principal)/sc.maxLoan)); # implements Eq. 22 in article

# -- Loss assessment parameters
num.thresholds <- period*(2/3) + 1 ; # number of default thresholds to be assessed, using g_1-measure's domain
vec.k.CD <- seq(0, (num.thresholds-1))  # Select thresholds for g1-measure



# ====== 1. PORTFOLIO GENERATION: Markovian Technique

# -- parallelized function for generating a portfolio using a Markovian technique accordingly to input probability parameters, which
# include: transition rates [p.trans] and state spaces [states].
# The rest of the input arguments are simply copies of those objects within the outer data scope passed to within
simJob_MarkovDefaults_p <- function(seed.value, p.trans, states, n, vec.Term, 
                                    vec.Instal, period, sc.Thres, createOwnPar=F, cpu.threads=6) {
  
  # seed.value <- 1234
  #ptm <- proc.time() #IGNORE: for computation time calculation
  
  # parallelized inner function that iterates once for every account
  # t: starting point, t.max: ending point, del: last delinquency state (or "initial state" when t=0)
  # n.states: number of states, seedval: seed value
  innerJob <- function(i, t, t.max, ins, p.trans, del, n.states, seedval) {
    
    # i=it; t=0; t.max=vec.Term[it]; ins=vec.Instal[it]; p.trans=p.trans; del=0; n.states=NROW(states); seedval=seed.value;
    
    # prepare outcome vector to be predicted by markov chain
    pred.outcomes <- rep(0, (t.max-t))
    # prepare receipt vector to be simulated
    rec.receipts <- copy(pred.outcomes) 
    
    if (t.max-t > 0) {
      
      # sample initial states
      last.observed <- case_when(
        del == 0 ~ 1, # "Active/CD0"
        del == 1 ~ 2, # CD1+,
        del == 2 ~ 3, # write-off/truncation
      )
      
      # simulate "initial" outcome based on last observed state
      set.seed(seedval+i)
      pred.outcomes[1] <- sample(x=1:n.states, size=1, replace=T, prob=p.trans[last.observed,])
      
      # simulate first receipt based on "initial" outcome
      if (pred.outcomes[1] == 1) {
        rec.receipts[1] <- ins # paid outcome
      } else if (pred.outcomes[1] > 1 & pred.outcomes[1] < n.states) {
        rec.receipts[1] <- 0 # default outcome, so receipt is zero
      } else {
        rec.receipts[1:(t.max-t)] <- 0 # write-off outcome, so receipt becomes truncated throughout the remaining history (shouldn't happen initially)
      }
      
      
      # complete the chain
      # and map chain outcomes to payment outcomes in the same loop - unless truncation/write-off is triggered from the "initial" outcome
      if (t.max-t > 1 & pred.outcomes[1] < n.states) {
        
        for (tt in 2:(t.max-t) ) {
          
          set.seed(seedval+i+(tt-1))
          pred.outcomes[tt] <- sample(x=1:n.states, size=1, replace=T, prob=p.trans[pred.outcomes[tt-1],])
          
          # cater for truncation/woff (last state, i.e., n.states), if it was sampled above
          if (pred.outcomes[tt] == n.states) {
            
            # fill all remaining outcomes with truncation outcome (zero payments) and exit loop
            pred.outcomes[tt:(t.max-t)] <- n.states
            rec.receipts[tt:(t.max-t)] <- 0 # no-payments
            break
            
          } else {
            
            trans <- diff(pred.outcomes[(tt-1):tt])
            
            if (trans < 0) { # CD decreased
              
              # therefore, at least 2 full instalments were paid
              rec.receipts[tt] <- -(trans - 1) * ins
              
            } else if (trans == 0) { # CD stayed the same 
              
              if (pred.outcomes[tt] == (n.states-1)) {
                
                # remember that penultimate state is semi-absorbing
                # this actually means that delinquency can continue to increase as long as the account remains in this state
                # Even if there are a few overpayments in reality (though not enough to cure completely), we assume the net receipt still to be 0
                rec.receipts[tt] <- 0
                
              } else {
                
                # a cure event, which roughly means (disregarding interest and fees for now) that we must have a receipt value big enough 
                # so that all missed payments are repaid
                rec.receipts[tt] <- -(trans - 1) * ins
                
              }
            } else { # CD increased by 1 (can only ever be an increment of one)
              
              # therefore, a zero is effectively simulated
              rec.receipts[tt] <- 0
              
            }
          }
        }
      }
    }
    
    # stitch together the generated receipts with write-off / truncated elements (assuming no historical receipt history)
    if (period - t.max <0) {cat("ERROR")}
    
    prep <- c(as.vector(rec.receipts),
              as.vector(rep(NA, period - t.max))
    )
    
    return(prep)    
  }
  
  
  #ptm <- proc.time() #IGNORE: for computation time calculation
  if (createOwnPar==T) {
    cl.port <- makeCluster(cpu.threads)
    registerDoParallel(cl.port)    
  }
  
  # using foreach() from foreach package for advanced looping (using %do%), including parallelization (using %dopar%)
  mat.Receipt.output <- foreach(it=1:n, .combine='cbind', .verbose=F, .inorder=T, .packages=c('dplyr','data.table')) %dopar%
    {
      #it <- 1
      # pass a maturity value (t) of 0, since we have no historical receipts and need to generate complete histories
      # pass a starting delinquency value of 0, since we are iterating from t=0 in generating a complete portfolio
      receipt.temp <- innerJob(i=it, t=0, t.max=vec.Term[it], ins=vec.Instal[it],
                               p.trans=p.trans, del=0, n.states=NROW(states), seedval=seed.value)
    }  
  
  if (createOwnPar==T) {
    stopCluster(cl.port)
  }
  
  #proc.time() - ptm
  
  return (mat.Receipt.output)
  
}




# ====== 2. LOSS ASSESSMENT: Iterative function definitions (to be run in parallel)

# - ancillary function that returns the first starting point of the first default episode within a given loan's history,
# given each loans's term (to restrict search space)
default.start.first.v2 <- function(i, thres.d, del.mat, t) {
  # testing purposes
  # i <- 57; del.mat <- mat.CD.Use; thres.d <- d.CD; i <- 129; i <- 7; t.offset<-0
  
  # ------ tests:
  #--times-------------------------------0,1,2, 3,4, 5,6,7,8,9,  10,11,12,13,  14,15,16,17,18,19,20
  # test case (assume CD): test.del <- c(3,3,2, 3,2, 4,5,2,1,2,   3, 4, 3, 2,   4, 5, 6, 5, 4, 3, 2)
  #   with d=3, first default episode ought to be time=0
  #--times---------------------------------0,1,2,3,4,5, 6,7,8
  # test case 2 (assume CD): test.del <- c(0,0,1,2,1,0, 0,1,0)
  #   with d=1, first default episode ought to be time=2
  
  # find positions (times) in delinquency matrix where threshold is/was reached to be (g,d)-defaulting at t
  #   - this should include from time t=0 (at origination)
  vec.found <- which(del.mat[1:(t[i]+1),i] >= thres.d)
  # test case: vec.found <- which(test.del >= 3)
  # test case 2: vec.found <- which(test.del >= 1)
  
  if(length(vec.found) == 1) {
    # only one index found
    episodes.start <- vec.found
  } else {
    # 1. Find positions in these positions where the lagged difference is greater than 1.
    #   - these indicate 'breaks' between episodes 
    # 2. Add 1 to these found positions to move to the 'initial starting points' of the next episode in succession
    # 3. Pre-fix this vector with '1' to re-include the first 'episode' that was deselected previously
    # 4. Given this vector of indices, return starting positions again
    episodes.start <-  vec.found[c(1, which(diff(vec.found) > 1) + 1 )]
  }
  
  # return starting period of first episode (if it exists)
  #   - if it doesn't exist, return -1
  #   - subtract 1 to account for a skewed index that includes in its range t=0 as index=1, 
  #     as a result of using the entire range of {del.mat}
  first.start <- ifelse(length(vec.found) == 0, -1, episodes.start[1] - 1)
  return(first.start)
  #rm(del.mat);
}


# - ancillary function to return the number of default episodes given a loan's history and a particular (g,d)-configuration
analyze.default.episodes <- function(i, thres.d, del.mat, t, method) {
  
  # --- tests
  # del.mat <- mat.CD.Use; thres.d <- d.CD; t <- vec.Term
  #i <- 4428
  
  # ------ tests:
  #--times-------------------------------0,1,2, 3,4, 5,6,7,8,9,  10,11,12,13,  14,15,16,17,18,19,20
  #--times-------------------------------1,2,3, 4,5, 6,7,8,9,10, 11,12,13,14,  15,16,17,18,19,20,21
  # test case (assume CD): test.del <- c(3,3,2, 3,2, 4,5,2,1,2,   3, 4, 3, 2,   4, 5, 6, 5, 4, 3, 2)
  #   with d=3, first default episode ought to be time=0
  #--times---------------------------------0,1,2,3,4,5, 6,7,8
  # test case 2 (assume CD): test.del <- c(0,0,1,2,1,0, 0,1,0)
  #   with d=1, first default episode ought to be time=2
  
  # find positions (times) in delinquency matrix where threshold is/was reached to be (g,d)-defaulting at t
  #   - this should include from time t=0 (at origination)
  vec.found <- which(del.mat[1:(t[i]+1),i] >= thres.d)
  # test case 1: vec.found <- which(test.del >= 3)
  # test case 2: vec.found <- which(test.del >= 1)
  
  if(length(vec.found) == 1) {
    # only one index found
    episodes.start <- vec.found
  } else {
    # 1. Find positions in these positions where the lagged difference is greater than 1.
    #   - these indicate 'breaks' between episodes 
    # 2. Add 1 to these found positions to move to the 'initial starting points' of the next episode in succession
    # 3. Pre-fix this vector with '1' to re-include the first 'episode' that was deselected previously
    # 4. Given this vector of indices, return starting positions again
    episodes.start <-  vec.found[c(1, which(diff(vec.found) > 1) + 1 )]
    
  } 
  
  # return number of episodes
  episodes.number <- length(episodes.start)
  
  if (!any(is.na(episodes.start))) {
    # for each episodic start, find the corresponding end: useful in calculating descriptive stats on episode length
    episodes.end <- rep(0, episodes.number)
    for (j in 1:episodes.number) {
      # j <- 1
      #episodes.end[j] <- Position(function(x) x < 3, test.del[episodes.start[j]:length(test.del)], nomatch=NA ) -1  + ifelse(j>1, episodes.start[j]-1, 0)
      episodes.end[j] <- Position(function(x) x < thres.d, del.mat[episodes.start[j]:(t[i]+1),i], nomatch=NA ) -1  + episodes.start[j]-1
    }
    
    # treat NAs in episodes.end
    episodes.end[is.na(episodes.end)] <- t[i] + 1
    
    # return episode length
    episodes.length <- episodes.end - episodes.start 
  } else {
    episodes.number <- 0
    episodes.length <- 0
  }
  
  # return an aggregation, based on specification
  if (method == "count") {
    ret <- episodes.number
  } else if (method == "mean") {
    ret <- mean(episodes.length, na.rm=T)
  } else if (method == "median") {
    ret <- median(episodes.length, na.rm=T)
  } else if (method == "var") {
    ret <- var(episodes.length, na.rm=T)
  } else if (method == "raw") {
    ret <- episodes.length
  }
  
  return( ret )
}



# - main function for assessing the portfolio loss at a specified threshold (using various delinquency measures)
coreJob_CD <- function(mat.Receipt.Use, vec.Instal, vec.IntRates, sc.Thres, period, n, vec.Principal, vec.Term,  
                       it, num.thresholds, d.CD, d.MD, d.DoD, mat.CD.Use, mat.MD.Use, mat.DoD.Use, vec.DoD.lambda, 
                       i_p.alt, Arrears.LossRate, Outstanding.LossRate) {
  
  # -- test
  # it <- 18; d.CD <- vec.k.CD[it]
  
  cat(paste0("\n1) Threshold [",it," of ",num.thresholds,"]: (", Sys.time(),") Assessing portfolio loss at threshold .. "),
      file="assesslog.txt", append=TRUE)
  
  # ---- Total Loss across given threshold (d.CD)
  
  # - get default start times of first episode (if multiple exist), given threshold d, otherwise return -1 to indicate a performing loan
  vec.default.start_first.CD <- sapply(1:n, default.start.first.v2, thres.d=d.CD, del.mat=mat.CD.Use, t=vec.Term)
  vec.default.start_first.MD <- sapply(1:n, default.start.first.v2, thres.d=d.MD, del.mat=mat.MD.Use, t=vec.Term)
  vec.default.start_first.DoD <- sapply(1:n, default.start.first.v2, thres.d=d.DoD, del.mat=mat.DoD.Use, t=vec.Term)
  
  # various default episode aggregations (using only g1-measure)
  def.epi.count <- sapply(1:n, analyze.default.episodes, thres.d=d.CD, del.mat=mat.CD.Use, t=vec.Term, method="count")
  def.epi.count.d3 <- sapply(1:n, analyze.default.episodes, thres.d=3, del.mat=mat.CD.Use, t=vec.Term, method="count") # experiment
  def.epis <- unlist(sapply(1:n, analyze.default.episodes, thres.d=d.CD, del.mat=mat.CD.Use, t=vec.Term, method="raw")) #raw episode lengths
  def.epi.mean <- sapply(1:n, analyze.default.episodes, thres.d=d.CD, del.mat=mat.CD.Use, t=vec.Term, method="mean")
  def.epi.mean.d3 <- sapply(1:n, analyze.default.episodes, thres.d=3, del.mat=mat.CD.Use, t=vec.Term, method="mean")# experiment
  def.epi.median <- sapply(1:n, analyze.default.episodes, thres.d=d.CD, del.mat=mat.CD.Use, t=vec.Term, method="median")
  def.epi.var <- sapply(1:n, analyze.default.episodes, thres.d=d.CD, del.mat=mat.CD.Use, t=vec.Term, method="var")
  
  
  # - get (g,d)-defaulting account indices across measure, given current thresholds
  def.CD <- which(vec.default.start_first.CD >= 0)
  def.MD <- which(vec.default.start_first.MD >= 0)
  def.DoD <- which(vec.default.start_first.DoD >= 0)
  
  # - get (g,d)-performing account indices across measure, given current thresholds
  perf.CD <- which(vec.default.start_first.CD < 0)
  perf.MD <- which(vec.default.start_first.MD < 0)
  perf.DoD <- which(vec.default.start_first.DoD < 0)
  
  # - get default start times of first episode (if multiple exist), given threshold d=3, otherwise return -1 to indicate a performing loan
  # Note: this is ancillary and only used for a particular experiment
  vec.default.start_first.CD.d3 <- sapply(1:n, default.start.first.v2, thres.d=3, del.mat=mat.CD.Use, t=vec.Term)
  def.CD.d3 <- which(vec.default.start_first.CD.d3 >= 0)
  
  
  # - calculate final maturity as either contractual term / maturity or default time, given (g,d)-default 
  # for use in discounting and other loss calculations
  vec.maturity.CD <- copy(vec.Term)
  vec.maturity.CD[def.CD] <- vec.default.start_first.CD[def.CD]
  vec.maturity.MD <- copy(vec.Term)
  vec.maturity.MD[def.MD] <- vec.default.start_first.MD[def.MD]
  vec.maturity.DoD <- copy(vec.Term)
  vec.maturity.DoD[def.DoD] <- vec.default.start_first.DoD[def.DoD]
  
  
  # - Calculate NPV of receipts, given maturity and relevant receipts
  vec.ReceiptsPV.CD <- sapply(1:n, function(i,r,t) {
    if (t[i] > 0) {
      val <- sum( r[1:t[i], i] * (1+i_p.alt/12)^(-1*1:(t[i]) ) )
    } else {
      val <- 0
    }
    return (val)
  }, r=mat.Receipt.Use, t=vec.maturity.CD)
  vec.ReceiptsPV.MD <- sapply(1:n, function(i,r,t) {
    if (t[i] > 0) {
      val <- sum( r[1:t[i], i] * (1+i_p.alt/12)^(-1*1:(t[i]) ) )
    } else {
      val <- 0
    }
    return (val)
  }, r=mat.Receipt.Use, t=vec.maturity.MD)
  vec.ReceiptsPV.DoD <- sapply(1:n, function(i,r,t) {
    if (t[i] > 0) {
      val <- sum( r[1:t[i], i] * (1+i_p.alt/12)^(-1*1:(t[i]) ) )
    } else {
      val <- 0
    }
    return (val)
  }, r=mat.Receipt.Use, t=vec.maturity.DoD)
  
  
  # - calculate NPV of arrears, given maturity, relevant instalments and relevant receipts
  vec.ArrearsPV.CD <- sapply(1:n, function(i,ins,r,t) {
    if (t[i] > 0) {
      val <- sum( ins[i] * (1+i_p.alt/12)^(-1*1:(t[i]) ) ) - r[i]
    } else {
      val <- 0
    }
    return (val)
  }, ins=vec.Instal, r=vec.ReceiptsPV.CD, t=vec.maturity.CD)
  vec.ArrearsPV.MD <- sapply(1:n, function(i,ins,r,t) {
    if (t[i] > 0) {
      val <- sum( ins[i] * (1+i_p.alt/12)^(-1*1:(t[i]) ) ) - r[i]
    } else {
      val <- 0
    }
    return (val)
  }, ins=vec.Instal, r=vec.ReceiptsPV.MD, t=vec.maturity.MD)
  vec.ArrearsPV.DoD <- sapply(1:n, function(i,ins,r,t) {
    if (t[i] > 0) {
      val <- sum( ins[i] * (1+i_p.alt/12)^(-1*1:(t[i]) ) ) - r[i]
    } else {
      val <- 0
    }
    return (val)
  }, ins=vec.Instal, r=vec.ReceiptsPV.DoD, t=vec.maturity.DoD)
  
  
  # - calculate expected balance, given tenure at (g,d)-default, resulting remaining tenure, instalments, and interest rates
  vec.ExpBalance.CD <- sapply(1:n, function(i,ins,intr,t,tt) {
    if (t[i] < tt[i]) {
      val <- sum( ins[i] * (1+intr[i]/12)^(-1*1:(tt[i] - t[i]) ) ) ;
    } else {
      val <- 0
    }
    # discount to origination
    val <- val *  (1+i_p.alt/12)^(-1*t[i] )
    return (val)
  }, ins=vec.Instal, intr=vec.IntRates, t=vec.maturity.CD, tt=vec.Term)
  vec.ExpBalance.MD <- sapply(1:n, function(i,ins,intr,t,tt) {
    if (t[i] < tt[i]) {
      val <- sum( ins[i] * (1+intr[i]/12)^(-1*1:(tt[i] - t[i]) ) ) ;
    } else {
      val <- 0
    }
    # discount to origination
    val <- val *  (1+i_p.alt/12)^(-1*t[i] )
    return (val)
  }, ins=vec.Instal, intr=vec.IntRates, t=vec.maturity.MD, tt=vec.Term)
  vec.ExpBalance.DoD <- sapply(1:n, function(i,ins,intr,t,tt) {
    if (t[i] < tt[i]) {
      val <- sum( ins[i] * (1+intr[i]/12)^(-1*1:(tt[i] - t[i]) ) ) ;
    } else {
      val <- 0
    }
    # discount to origination
    val <- val *  (1+i_p.alt/12)^(-1*t[i] )
    return (val)
  }, ins=vec.Instal, intr=vec.IntRates, t=vec.maturity.DoD, tt=vec.Term)
  
  
  # - calculate losses as weighted combination between arrears and expected balance, and associated loss rates
  vec.Losses.CD <- pmax(vec.ArrearsPV.CD*Arrears.LossRate + vec.ExpBalance.CD*Outstanding.LossRate, 0)
  vec.Losses.MD <- pmax(vec.ArrearsPV.MD*Arrears.LossRate + vec.ExpBalance.MD*Outstanding.LossRate, 0)
  vec.Losses.DoD <- pmax(vec.ArrearsPV.DoD*Arrears.LossRate + vec.ExpBalance.DoD*Outstanding.LossRate, 0)
  
  # - calculate actual balance [ancillary information]
  vec.bal.CD <- pmax(vec.ArrearsPV.CD + vec.ExpBalance.CD, 0)
  vec.bal.MD <- pmax(vec.ArrearsPV.MD + vec.ExpBalance.MD, 0)
  vec.bal.DoD <- pmax(vec.ArrearsPV.DoD + vec.ExpBalance.DoD, 0)
  
  # ---------- Concatenate relevant information, including profit/loss calculations for optimisation
  dat.EL.core <- rbind(data.table(Iteration=it, Measure="CD", MeasureName ="g1: CD", Threshold=d.CD,
                                  Vol_Perf=length(perf.CD),Vol_Def=length(def.CD),
                                  Bal_Perf = sum(vec.bal.CD[perf.CD], na.rm = T), Bal_Def = sum(vec.bal.CD[def.CD], na.rm = T),
                                  Vol_Def_d3 = length(def.CD.d3), Bal_Def_d3 = sum(vec.bal.CD[def.CD.d3], na.rm = T), # experimental
                                  Loss=sum(vec.Losses.CD, na.rm = T),
                                  Episode_Number_Sum = sum(def.epi.count, na.rm=T),
                                  Episode_Number_Mean = mean(def.epi.count, na.rm=T), #-used as denominator (main)
                                  Episode_Number_Mean_d3 = mean(def.epi.count.d3, na.rm=T), #-used as denominator (experimental)
                                  Episode_Length_Mean_Grand = mean(def.epis, na.rm=T),
                                  Episode_Length_Median_Grand = median(def.epis, na.rm=T),
                                  Episode_Length_Mean_AccountMean = mean(def.epi.mean, na.rm=T), #-used as numerator (main)
                                  Episode_Length_Mean_AccountMean_d3 = mean(def.epi.mean.d3, na.rm=T), #-used as numerator (experimental)
                                  Episode_Length_Median_AccountMedian = median(def.epi.median, na.rm=T),
                                  Episode_Length_Var_Grand = var(def.epis, na.rm=T),
                                  Episode_Length_Var_AccountMeanVar = mean(def.epi.var, na.rm=T)
  ),
  data.table(Iteration=it, Measure="MD", MeasureName ="g2: MD", Threshold=d.MD,
             Vol_Perf=length(perf.MD),Vol_Def=length(def.MD),
             Bal_Perf = sum(vec.bal.MD[perf.MD], na.rm = T), Bal_Def = sum(vec.bal.MD[def.MD], na.rm = T),
             Vol_Def_d3 = NA, Bal_Def_d3 = NA, 
             Loss=sum(vec.Losses.MD, na.rm = T),
             Episode_Number_Sum = NA, Episode_Number_Mean = NA, Episode_Number_Mean_d3 = NA, 
             Episode_Length_Mean_Grand = NA, Episode_Length_Median_Grand = NA, 
             Episode_Length_Mean_AccountMean = NA, Episode_Length_Mean_AccountMean_d3 = NA, 
             Episode_Length_Median_AccountMedian = NA, Episode_Length_Var_Grand = NA,
             Episode_Length_Var_AccountMeanVar = NA
  ),
  data.table(Iteration=it, Measure="DoD", MeasureName ="g3: DoD", Threshold=d.DoD,
             Vol_Perf=length(perf.DoD),Vol_Def=length(def.DoD),
             Bal_Perf = sum(vec.bal.DoD[perf.DoD], na.rm = T), Bal_Def = sum(vec.bal.DoD[def.DoD], na.rm = T),
             Vol_Def_d3 = NA, Bal_Def_d3 = NA, 
             Loss=sum(vec.Losses.DoD, na.rm = T),
             Episode_Number_Sum = NA, Episode_Number_Mean = NA, Episode_Number_Mean_d3 = NA, 
             Episode_Length_Mean_Grand = NA, Episode_Length_Median_Grand = NA, 
             Episode_Length_Mean_AccountMean = NA, Episode_Length_Mean_AccountMean_d3 = NA, 
             Episode_Length_Median_AccountMedian = NA, Episode_Length_Var_Grand = NA,
             Episode_Length_Var_AccountMeanVar = NA
  )                       
  )
  
  cat(paste0("\n2) Threshold [",it," of ",num.thresholds,"]: (", Sys.time(),") Loss assessed! "),
      file="assesslog.txt", append=TRUE)
  
  return (dat.EL.core)
}


# - function to truncating a receipt matrix, given starting times and term
truncationJob <- function(mat.Receipt.Use, vec.truncstart, vec.Term, createOwnPar=F, cpu.threads=6) {
  
  innerJob2 <- function(i, t, t.max, Receipt.v) {
    if (t <0) {
      prep.v <- Receipt.v
    } else {
      prep.v <- c(Receipt.v[1:t-1], rep(0,t.max-t+1))
    }
    
    return(prep.v)
  }
  
  #ptm <- proc.time() #IGNORE: for computation time calculation
  if (createOwnPar==T) {
    cl.port <- makeCluster(cpu.threads)
    registerDoParallel(cl.port)    
  }
  
  # using foreach() from foreach package for advanced looping (using %do%), including parallelization (using %dopar%)
  mat.Receipt.output <- foreach(it=1:n, .combine='cbind', .verbose=F, .inorder=T, .packages=c('dplyr','data.table')) %dopar%
    {
      #it <- 1
      receipt.temp <- innerJob2(i=it, t=vec.truncstart[it], t.max=vec.Term[it], Receipt.v=mat.Receipt.Use[,it])
    }  
  
  if (createOwnPar==T) {
    stopCluster(cl.port)
  }
  
  #proc.time() - ptm
  
  return (mat.Receipt.output)  
}




# ====== 3. OUTER FUNCTION TO BE CALLED WITHIN MONTE CARLO FRAMEWORK 

# - Outer function for:
# 1) generating a portfolio using a Markovian technique according to probability input parameters [p.trans], [states], [seed.value]
# 2) assessing generated portfolio using loss procedure
outerJob <- function(states, p.trans, seed.value, n, vec.Instal, period, sc.Thres,
                     vec.IntRates, vec.Principal, vec.Term, vec.DoD.lambda, num.thresholds, vec.k.CD,
                     i_p.alt, Arrears.LossRate, Outstanding.LossRate, k.trunc=0, g.trunc=1) {
  
  ptm <- proc.time() #IGNORE: for computation time calculation
  
  # outer.iter <- 9; seed.value <- 1
  
  # ===================  Initialization

  # error check
  if (any( abs( rowSums(p.trans)-1L) > 0.0001 )) {
    cat(paste0("\n\n0) Transition matrix flawed: ", outer.iter, ". ", c(p.trans)), file="procedure_log.txt", append=TRUE)
  }
  
  
  # ===================  Generate loan portfolio
  
  cat(paste0("\n\n1) (", Sys.time(),"). Generating portfolio .. "),
      file="procedure_log.txt", append=TRUE)
  
  # --- Markovian Defaults
  mat.Receipt.Use <- simJob_MarkovDefaults_p(seed.value=seed.value, p.trans=p.trans, states=states, n=n, 
                                             vec.Term = vec.Term, vec.Instal=vec.Instal, period=period, sc.Thres)
  
  # ---- Calculate g1-measure up to full contractual term using forecasts
  mat.CD.Use <- calculate.CD(vec.Instal, mat.Receipt.Use, sc.Thres, period, n, method="base")
  
  # -- Calculate MD/DoD (g2/g3: Macaulay Duration Index (MD) Measure | Degree of Delinquency (DoD) Measure)
  calc.results <- calculate.MDoD(vec.Instal, mat.Receipt.Use, vec.Principal, period, n, vec.IntRates, vec.DoD.lambda)
  mat.MD.Use <- calc.results$MD
  mat.DoD.Use <- calc.results$DoD
  rm(calc.results) #an optimization, reduces memory usage  
  
  
  # ---- Apply (k,g)-truncation if specified ()
  if (k.trunc > 0) {
    
    cat(paste0("\n\n1b) (", Sys.time(),"). Truncating portfolio (k=", k.trunc, ", g=", g.trunc,") .. "),
        file="procedure_log.txt", append=TRUE)    
    if (g.trunc == 1) {
      mat.del.use <- mat.CD.Use
    } else if (g.trunc == 2){
      mat.del.use <- mat.MD.Use
    } else if (g.trunc == 3) {
      mat.del.use <- mat.DoD.Use
    } else { # default to CD g1-measure
      mat.del.use <- mat.CD.Use
    }
    # - get start times of truncation (using existing machinery in default.start.first.v2() ), given threshold k.trunc, otherwise return -1.
    vec.truncstart <- sapply(1:n, default.start.first.v2, thres.d=k.trunc, del.mat=mat.del.use, t=vec.Term)
    
    # - call truncation job and replace receipts
    mat.Receipt.Use <- truncationJob(mat.Receipt.Use, vec.truncstart, vec.Term)
    
    # - Recalculate delinquency accordingly
    mat.CD.Use <- calculate.CD(vec.Instal, mat.Receipt.Use, sc.Thres, period, n, method="base")
    
    # - Recalculate MD/DoD (g2/g3: Macaulay Duration Index (MD) Measure | Degree of Delinquency (DoD) Measure)
    calc.results <- calculate.MDoD(vec.Instal, mat.Receipt.Use, vec.Principal, period, n, vec.IntRates, vec.DoD.lambda)
    mat.MD.Use <- calc.results$MD
    mat.DoD.Use <- calc.results$DoD
    rm(calc.results) #an optimization, reduces memory usage      
  }
  
  # ==== Select default threshold vectors (d) for g2 and g3 measures, based on measured delinquency
  # -- MD
  max.thres <- max(quantile(mat.MD.Use[!is.na(mat.MD.Use)], 0.95)) + 1
  vec.k.MD <- seq(1, ifelse(is.na(max.thres), 5, max(max.thres, 5)),length.out = num.thresholds)
  cut.1 <- 10; len.1 <- 20; cut.2 <- 40
  vec.k.MD <- c(seq(1, cut.1, length.out = len.1),seq(cut.1+0.1, cut.2, length.out = num.thresholds-len.1)) # thresholds found by trial-and-error
  
  # -- DoD
  max.thres <- max(quantile(mat.DoD.Use[!is.na(mat.DoD.Use)], 0.95)) + 1
  vec.k.DoD <- seq(1, ifelse(is.na(max.thres), 5, max(max.thres, 5)),length.out = num.thresholds) 
  cut.1 <- 10; len.1 <- 20; cut.2 <- 40
  vec.k.DoD <-  c(seq(1, cut.1, length.out = len.1),seq(cut.1+0.1, cut.2, length.out = num.thresholds-len.1)) # thresholds found by trial-and-error
  
  
  # ===================  Portfolio loss assessment across thresholds: parallelized loop
  
  cat(paste0("\n2) (", Sys.time(),"): Portfolio generated and delinquency assessed. Assessing portfolio losses across thresholds .. "),
      file="procedure_log.txt", append=TRUE)
  
  cat(paste("New Job (", Sys.time(),"): Assessing delinquency and profitability of given portfolio across various thresholds",sep=''),
      file="assesslog.txt", append=FALSE)
  
  
  # using foreach() from foreach package for distributing loop iterations across registered threads: remarkable improvement in run time
  # Note: need to import all custom functions used within the loss assessment.
  dat.EL.par <- foreach(it=1:num.thresholds, .combine='rbind', .verbose=F, .inorder=F, .packages ='data.table',
                        .export=c('default.start.first.v2', 'analyze.default.episodes','coreJob_CD')) %dopar%
    {
      
      dat.EL.core <- coreJob_CD(mat.Receipt.Use=mat.Receipt.Use, vec.Instal=vec.Instal,
                                vec.IntRates=vec.IntRates, sc.Thres=sc.Thres, period=period, n=n, 
                                vec.Principal=vec.Principal, vec.Term=vec.Term, it=it, num.thresholds=num.thresholds, 
                                d.CD=vec.k.CD[it], d.MD=vec.k.MD[it], d.DoD=vec.k.DoD[it], 
                                mat.CD.Use=mat.CD.Use, mat.MD.Use=mat.MD.Use, mat.DoD.Use=mat.DoD.Use, vec.DoD.lambda=vec.DoD.lambda, 
                                i_p.alt=i_p.alt, Arrears.LossRate=Arrears.LossRate, Outstanding.LossRate=Outstanding.LossRate)
    }
  
  elapsed <- proc.time() - ptm
  
  cat(paste0("\n3) (", Sys.time(),"): Done! Elapsed time: ", round(elapsed['elapsed'],digits=0), " seconds"),
      file="procedure_log.txt", append=TRUE)
  
  # - last data preparation
  dat.EL.par[, Iteration := outer.iter]
  setDT(dat.EL.par, key=c("Iteration","Measure","Threshold"))
  
  # add transition probabilities to data
  dat.EL.par[, P_PP := p.trans[1,1]]
  dat.EL.par[, P_DD := p.trans[2,2]]
  
  return(dat.EL.par)
  
}





# ====== 4. SIMULATION FRAMEWORK

# ---- Parametrisation History
# 1a(i): P_PP=0.33, P_DD=0.33, with 0.1% and 1% write-off state-dependent probabilities; no truncation
# 1a(ii): P_PP=0.33, P_DD=0.33, with 0.1% and 1% write-off state-dependent probabilities; (15,g3)-truncation; cut.1 <- 15; len.1 <- 25; cut.2 <- 40
# 1a(iii): P_PP=0.33, P_DD=0.9, with 0.1% and 1% write-off state-dependent probabilities; no truncation; cut.1 <- 15; len.1 <- 25; cut.2 <- 40
# 1a(iv): P_PP=0.10, P_DD=0.90, with 0.1% and 1% write-off state-dependent probabilities; no truncation; cut.1 <- 7; len.1 <- 20; cut.2 <- 40
# 1a(v): P_PP=0.10, P_DD=0.10, with 0.1% and 1% write-off state-dependent probabilities; no truncation; cut.1 <- 7; len.1 <- 20; cut.2 <- 40
# 1a(vi): P_PP=0.95, P_DD=0.95, with 0.1% and 1% write-off state-dependent probabilities; no truncation; cut.1 <- 4; len.1 <- 15; cut.2 <- 40
# 1a(vii): P_PP=0.9, P_DD=0.1, with 0.1% and 1% write-off state-dependent probabilities; no truncation; cut.1 <- 10; len.1 <- 15; cut.2 <- 40
# 1a(viii): P_PP=0.75, P_DD=0.75, with 0.1% and 1% write-off state-dependent probabilities; no truncation; cut.1 <- 10; len.1 <- 15; cut.2 <- 40
# 1a(ix): P_PP=0.85, P_DD=0.8, with 0.1% and 1% write-off state-dependent probabilities; no truncation; cut.1 <- 15; len.1 <- 20; cut.2 <- 40
# 1a(x): P_PP=0.20, P_DD=0.05, with 0.1% and 1% write-off state-dependent probabilities; no truncation; cut.1 <- 10; len.1 <- 20; cut.2 <- 40
# 1a(xi): P_PP=0.20, P_DD=0.50, with 0.1% and 1% write-off state-dependent probabilities; no truncation; cut.1 <- 10; len.1 <- 20; cut.2 <- 40
# 1a(xii): P_PP=0.10, P_DD=0.05, with 0.1% and 1% write-off state-dependent probabilities; no truncation; cut.1 <- 10; len.1 <- 20; cut.2 <- 40
# 1a(xiii): P_PP=0.10, P_DD=0.50, with 0.1% and 1% write-off state-dependent probabilities; no truncation; cut.1 <- 10; len.1 <- 20; cut.2 <- 40
# 1a(xiv): P_PP=0.90, P_DD=0.05, with 0.1% and 1% write-off state-dependent probabilities; no truncation; cut.1 <- 10; len.1 <- 25; cut.2 <- 40
# 1a(xv): P_PP=0.90, P_DD=0.95, with 0.1% and 1% write-off state-dependent probabilities; no truncation; cut.1 <- 5; len.1 <- 20; cut.2 <- 40
# 1a(xvi): P_PP=0.95, P_DD=0.98, with 0.1% and 1% write-off state-dependent probabilities; no truncation; cut.1 <- 5; len.1 <- 20; cut.2 <- 40\
# 1a(xvii): P_PP=0.40, P_DD=0.025, with 0.1% and 1% write-off state-dependent probabilities; no truncation; cut.1 <- 10; len.1 <- 20; cut.2 <- 40
# 1a(xviii): P_PP=0.40, P_DD=0.575, with 0.1% and 1% write-off state-dependent probabilities; no truncation; cut.1 <- 10; len.1 <- 25; cut.2 <- 40


# ---- Parametrisation
cpu.threads <- 6

# -- Markov chain's state space
states <- c("P","D", "W") #Performing, Delinquency, Write-off / truncate
# create a preliminary transition matrix, detailing the write-off state's transition probability at each state
# and enforcing the write-off state to be absorbing.
p.trans <- matrix(c(rep(0,length(states)-1), 0.001, # base probability for write-off given performing state
                    rep(0,length(states)-1), 0.010, # base probability for write-off given delinquency state
                    rep(0,length(states)-1), 1.000), ncol=3, byrow=T)
colnames(p.trans) <- states; rownames(p.trans) <- states

# now assign specific probabilities between [0, 1-p.trans[1,3]] and [0, 1-p.trans[2,3]]
p.PP <- 0.4
p.DD <- 0.95

# 1.  PP=67%, DD=67%; gives weird cup-like shape in g1
# 2.  PP=90%, DD=67%; gives somewhat cup-like shape in g1, but flat lines in g2/g3
# 3.  PP=67%, DD=90%; gives similar shapes as a(iii) [PP=33%, DD=9%]
# 4.  PP=90%, DD=90%; gives promising optima at early thresholds across all measures, but g1 is too squiggly
# 5.  SAVED | PP=95%; DD=95%, gives optima with less squiggly g1 curve than 4.
# 6.  PP=90%, DD=33%; gives similar curve shape as a(vii) [PP=90%, DD=10%], but g2/g3 is flatter with no significant optima
# 7.  PP=95%, DD=33%; gives similar curve shape as a(vii) [PP=90%, DD=10%], but g2/g3 is flatter with no significant optima, and g1 is flatter also (tending to L-shape)
# 8.  PP=50%, DD=33%; big optima in g1, smooth L-shapes in g2/g3
# 9.  PP=33%, DD=50%; bigger optima (cup-shape) in g1, smooth L-shapes in g2/g3 with slight optima, some maxima at early thresholds
# 10. PP=33%, DD=90%; no optima, monotonic increasing curves
# 11. PP=33%, DD=67%; no optima, but wonkier nonlinear g1-curve than in 10. g2/g3 curves similar as in 10.
# 12. PP=67%, DD=33%; slight optima in g1, L-shapes for g2/g3, similar to 8. 
# 13. PP=75%, DD=50%; significant optima in g1, still L-shapes for g2/g3, similar to 8, 12
# 14. SAVED | PP=75%, DD=75%; optima across all measures, very interesting nonlinear shape in g1
# 15. PP=85%, DD=40% 45% 50% 55% 60% 65% 70% 75%; [40,70]: L-shapes in g2/g3 remains fairly unchanged. optima-threshold for g1 moves rightward.
#     DD=75%: while L-shapes remain, the optima curve in g1 becomes wonkier
#     DD=80%: L-shapes collapse into slight optima for g2/g3, g1's optima wonky. interesting case.
#     DD=[85,95]: increasingly similar to 5, less interesting.
# 16. SAVED | PP=85%, DD=80%; optima across all measures (more so for g1), though g1 has some wonkiness. originally found in 15.
# 17. PP=20%, DD= 5% 10%; 5% has nice optima across all measures, 10% less so
#     DD=15% 20% 25% 30% 35% 40% [15,40]: g2/g3 becomes L-shaped, optima in g1 moves rightwards and becomes increasingly "optimized"
#     DD=45% 50% [45, 50%]: g2/g3 becomes less L-shaped and starts exhibiting optima again, g1's optima deepens and continues to move rightwards
#     DD=55%: still optima across all measures, but funky shapes in g2/g3.
#     DD=60% 65% 70% 75% 80% 85% 90% 95% [60, 95]: optima only at d=0, curvy shapes for g2/g3, nonlinear wonky shape in g1, though gets straighter for larger DD
# 18. SAVED | PP=20%, DD=05%; found from 17; optima across all measures
# 19. SAVED | PP=20%, DD=50%; found from 17; optima across all measures, deep optima for g1
# 20. PP=10%, DD= 5% 10% 15% 20% 25% 30% 35%; optima across all measures, deepest in g1; g2/g3 becomes L-shaped from DD=10% onward; optima in g1 moves rightwards as DD increases
#     DD=40% 45% 50%: L-shapes in g2/g3 starts collapsing back into optima, good shapes after DD=50%
#     DD=55%: funky shapes in g2/g3, deep optima still in g1
#     DD=60% 65% 70% 75% 80% 85% 90%: optima only at d=0, nonlinear curve in g1, though it straightens out after 65%
# 21. SAVED | PP=10%, DD=05%; found from 17; optima across all measures, similar to 18.
# 22. SAVED | PP=10%, DD=50%; found from 17; optima across all measures; deep optima in g1, similar to 19.
# 23. SAVED | PP=90%, DD=05%; optima across all measures, though all measures somewhat L-shaped. optima only sleight.
# 24. PP=90%, DD=15% 20% 25% 30% 35%; slight optima across all measures, though very similar shape to 23 (xiv), g1's optima moves rightward
#     D=40% 45% 50% 55% 60% 65% 70% 75% 80%; the slight optima in g2/g3 collapses into L-shapes, optima in g1 continues to move rightward as DD increases
#     D=85%: optima in g1 becomes wonky, the L-shapes of g2/3 become funky, though still largely L-shaped
#     D=90%: previously produced in 4, gives optima, but g1 is squiggly
# 25. SAVED | PP=90%, DD=95%; optima found at early threshold across all measures, checkmark-shape.
# 26. PP=95%, DD=2.5% 5% 10% 15% 20% 25% 30; slight optima across all measures, but still largely L-shaped
#     DD=35% 40% 50% 55% 60% 65% 70% 75% 80%: optima in g2/g3 collapses into L-shapes, optima in g1 moves rightward, and deeper from D=55% onwards
#     DD=85%: optima in g1 becomes wonkier and jagged
#     DD=90%: g1, while still technically having optima, is very wonky and jagged. g2/g3 have optima at early threshold, though flatlines thereafter
#     DD=95%: previously produced in 8 (vi), gives optima at early threshold, but g1 squiggly
# 27. SAVED | PP=95%, DD=98%; optima at early thresholds across all measures, similar to 25. (xv), checkmark-shaped. found from 26.
# 28. PP=40%, DD=2.5%; nice optima across all thresholds, though slightly flattens from 5%, while g1's optima moving to the right as DD increases
#     DD=15% 20% 25% 30% 35% 40% 45%; optima of g2/g3 collapses into L-shapes, while g1's otpima moves ever rightward and deeper
#     DD=50% 55% 57.5%; g2/g3 rematerialize optima and deepends from DD=55%, g1's optima quite deep and continues to shift rightwards
#     DD=60%; weird wiggly optima across all measures (g2/g3 at d=1, while g1 has a very deep minimum)
#     DD=65% 70% 75% 80% 85% 90% 95% 98%; optima at early thresholds across all measures, g1 nonliner, though continues to straighten as DD increases

# 29. SAVED | PP=40%, DD=02.5%; deep and proper optima across all measures, though resembling a light L-shape; uncovered from 28
# 30. SAVED | PP=40%, DD=57.5%; deep and proper optima across all measures, threshold-locations very different across measures; uncovered from 28

# calculate the remaining elements
p.trans[1, 1] <- p.PP
p.trans[1, 2] <- 1-p.trans[1,3]-p.PP
p.trans[2, 1] <- 1-p.trans[2,3]-p.DD
p.trans[2, 2] <- p.DD
p.trans

# -- Truncation settings
k.trunc <- 0
g.trunc <- 3

# -- File name settings for storing results
it.name <- "v1_1a(xix)"

# ---- Main Loop

ptm <- proc.time() #IGNORE: for computation time calculation

cl.port <- makeCluster(cpu.threads) # number of threads to register in the OS for this procedure (used in outerJob)
registerDoParallel(cl.port)

cat(paste("New Job (", Sys.time(),"): Experiment series ", it.name, ".",sep=''),
    file="procedure_log.txt", append=FALSE)



# - main call
dat.EL <- outerJob(states=states, p.trans=p.trans, seed.value=1, n=n, vec.Instal=vec.Instal, period=period,
                   sc.Thres=sc.Thres, vec.IntRates=vec.IntRates, vec.Principal=vec.Principal, vec.Term=vec.Term,
                   vec.DoD.lambda=vec.DoD.lambda, num.thresholds=num.thresholds, vec.k.CD=vec.k.CD,
                   i_p.alt=i_p.alt, Arrears.LossRate=Arrears.LossRate, Outstanding.LossRate=Outstanding.LossRate, 
                   k.trunc=k.trunc, g.trunc=g.trunc
)

# - last data preparation
setDT(dat.EL, key=c("Iteration", "Measure", "Threshold"))
# - zip and save optimisation results to disk
pack.ffdf(paste0("SpecLossProc-",it.name),dat.EL)

elapsed <- proc.time() - ptm
elapsed

stopCluster(cl.port) # release threads back to the OS

cat(paste0("\n\nEnd of Job (", Sys.time(),"): done. Elapsed time: ", round(elapsed['elapsed'],digits=0), " seconds"),
    file="procedure_log.txt", append=TRUE)




# =========== Loss Plots
# Note these loss plots are only experimental. There is a much more manicured version that produces the graphs actually used in the research article.

# -- structure final results for plotting purposes
plot.data <- copy(dat.EL)
plot.data[, Loss := Loss/ sum(vec.Principal)] # convert into loss %

# -- plot
ggplot(plot.data, aes(x=Threshold, y=Loss)) + 
  geom_point(aes(x=Threshold,y=Loss, color=Measure, shape=Measure), size=1.75) +
  geom_line(aes(x=Threshold, y=Loss, color=Measure), size = 0.5) + 
  labs(y="Loss (%)",x=bquote(Default~thresholds~italic(d))) + theme_minimal() + 
  theme(text=element_text(family="Times New Roman", size=12),
        legend.position="bottom") + 
  scale_color_economist(name="Delinquency Measure",guide = guide_legend(nrow=1)) +
  scale_shape_manual(values=c(1,16,8), 
                     name="Delinquency Measure",guide = guide_legend(nrow=1)) +
  scale_y_continuous(breaks= pretty_breaks(), labels=percent)

