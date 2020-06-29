# ======== Script 1: Loss Optimisation Procedure
# This script implements the loss optimisation procedure presented in the accompanying academic article.
# It does so in two steps: 
#   1) generate a simple portfolio of amortising loans; 
#   2) assess the overall portfolio loss across given default thresholds, using several delinquency measures

# --- The following information is ancillary to this particular script
# a) Portfolio simulated using: RANDOM DEFAULTS
# b) (k,g)-truncation is implemented
# c) This script is used to produce two results: (4,g1)-truncation (fig. 3a) and (6,g3)-truncation (fig. 3b)
script.opt <- 'b' # valid values may include either 'a' (fig. 3a) or 'b' (fig. 3b). Set to anything else to ignore specific hard-coded logic that reproduces graphs in article exactly.


# ====== 0. PARAMETRISATION

# -- R package setup
require(data.table)
require(ggplot2)
require(ggthemes)
require(scales)
require(extrafont)

# -- Compile Delinquency Calculation Functions (CD, MD/DoD)
source('DelinqM.R')

# -- Basic simulation parameters
period <- 60; # term - assumed to be the same for every loan in simulated portfolio
n <- 10000; # number of loans to be simulated in portfolio
vec.Term <- rep(period, n); # a vector of contractual terms (for ease of calculation later)
if (script.opt == 'a') { # for fig 3a
  given.filename <- "1a-Data-RandomDefaults-4g1"
} else if (script.opt == 'b') { # for fig 3b
  given.filename <- "1b-Data-RandomDefaults-6g3"
}

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

# -- General calculations
principal <- RepayAmount*(1-(1+i.rate)^(-period/12)) / ((1+i.rate)^(1/12)-1);

# -- Parameters for Receipt Generation
ProbPay <- 0.8; # probability of full payment

# -- (k,g)-truncation parameters
k <- 4; # truncation parameter 
g.trunc <- 'g1'; # given measure on whose scale to operate and subsequently truncate

### Specific hard-coded parametrisations to reproduce figures in article exactly
if (script.opt == 'a') { # for fig. 3a
  k <- 4;
  g.trunc <- 'g1'
} else if (script.opt == 'b') { # for fig.3b
  k <- 6;
  g.trunc <- 'g3'
}

# -- Parameters used in calculating delinquency measures
sc.Thres <- 0.9; # repayment ratio - g1
sc.DelinqSens <- 1; # delinquency sensitivity - g3
sc.maxLoan <- 5000; # maximum loan size offered by lender - g3
num.thresholds <- period +2; # number of default thresholds, based on maximum theoretically attainable CD value (which is the contractual term)


# ====== 0. INITIALIZATION

ptm <- proc.time()

# -- General Data Structures
mat.Receipt <- matrix(0, nrow=period, ncol=n); #matrix which contains simulated receipts (to be filled later)
vec.Principal <- rep(principal,n); # a vector of loan principals (for ease of calculation later)
vec.Instal <- rep(RepayAmount,n); # a vector of fixed instalments (for ease of calculation later)
vec.IntRates <- rep(i_p.rate,n); # a vector of fixed interest rates (for ease of calculation later)

# -- Data Structures for delinquency measures
mat.CD <- matrix(-1, nrow=period+1, ncol=n); #include time 0
mat.MD <-  matrix(-1.00, nrow=period+1, ncol=n);
mat.DoD <- mat.MD;
vec.DoD.lambda <- sc.DelinqSens * (1-((sc.maxLoan-vec.Principal)/sc.maxLoan)); # implements Eq. 22 in article

# -- Empty vectors of default thresholds
vec.d.CD <- rep(0,num.thresholds);
vec.d.MD <- rep(0,num.thresholds);
vec.d.DoD <- rep(0,num.thresholds);



# ====== 0. PORTFOLIO GENERATION (Receipts)

# ==== Generate Receipts (no truncation applied yet)
for (Loan in 1:n) {
  Month <- 0;
  while (Month < period) {
    Month <- Month + 1;
    p <- runif(1);
    if (p < ProbPay) {
      mat.Receipt[Month,Loan] <- vec.Instal[Loan];
    } else {
      mat.Receipt[Month,Loan] <- 0;
    }
  }
}

# ==== Calculate Delinquency Measures
# -- Calculate CD (g1: Contractual Delinquency)
mat.CD <- calculate.CD(vec.Instal, mat.Receipt, sc.Thres, period, n, method="base")

# -- Calculate MD/DoD (g2/g3: Macaulay Duration Index (MD) Measure | Degree of Delinquency (DoD) Measure)
calc.results <- calculate.MDoD(vec.Instal, mat.Receipt, vec.Principal, period, n, i.rate, vec.DoD.lambda)
mat.MD <- calc.results$MD
mat.DoD <- calc.results$DoD

# ==== Apply (k,g)-truncation, based on calculated delinquency and chosen measure with chosen truncation parameter
if (k > 0) { 
  
  # use appropriate Delinquency Measure values
  if (g.trunc == "g1") {
    mat.DM <- mat.CD
  } else if (g.trunc == "g2") {
    mat.DM <- mat.MD
  } else if (g.trunc == "g3") {
    mat.DM <- mat.DoD
  }
  
  for (i in 1:n) {
    # find t(g,w)_min - the starting period of truncation (if it exists, i.e., if sufficient delinquency was simulated to meet the truncation parameter)
    ind.truncable <- which(mat.DM[,i] >= k)
    if (NROW(ind.truncable) > 0) {
      cure.start <- min(ind.truncable)+1
      if (cure.start <= period) {
        mat.Receipt[cure.start:period,i] <- 0 #curate with defaults
      }
    }
  }
}

# ==== Recalculate Delinquency Measures, post truncation
if (k > 0) {
  # -- Calculate CD (g1: Contractual Delinquency)
  mat.CD <- calculate.CD(vec.Instal, mat.Receipt, sc.Thres, period, n, method="base")
  
  # -- Calculate MD/DoD (g2/g3: Macaulay Duration Index (MD) Measure | Degree of Delinquency (DoD) Measure)
  calc.results <- calculate.MDoD(vec.Instal, mat.Receipt, vec.Principal, period, n, i.rate, vec.DoD.lambda)
  mat.MD <- calc.results$MD
  mat.DoD <- calc.results$DoD 
}

# ==== Select default threshold vectors (d) for each Delinquency Measure
vec.d.CD <- seq(0, period+1, length.out = num.thresholds)
# -- MD
max.thres <- max(quantile(mat.MD[!is.na(mat.MD)], 1)) + 1
vec.d.MD <- seq(1, ifelse(is.na(max.thres), 5, max(max.thres, 5)),length.out = num.thresholds)

### specific hard-coded thresholds to reproduce article figures exactly
if (script.opt == 'a') {
  vec.d.MD <- c(seq(1, 3, length.out = 15),seq(3.1, 61, length.out = num.thresholds-15)) #for fig3a  
} else if (script.opt == 'b') {
  vec.d.MD <- c(seq(1, 6.5, length.out = 15),seq(6.6, 61, length.out = num.thresholds-15)) #for fig3b  
}

# -- DoD
max.thres <- max(quantile(mat.DoD[!is.na(mat.DoD)], 1)) + 1
vec.d.DoD <- seq(1, ifelse(is.na(max.thres), 5, max(max.thres, 5)),length.out = num.thresholds) 

### specific hard-coded thresholds to reproduce article figures exactly
if (script.opt == 'a') {
  vec.d.DoD <- c(seq(1, 3, length.out = 15),seq(3.1, 61, length.out = num.thresholds-15)) #for fig3a
} else if (script.opt == 'b') {
  vec.d.DoD <- c(seq(1, 5, length.out = 10),seq(5.5, 6.5, length.out = 10), seq(6.6, 61, length.out = num.thresholds-20)) #for fig3b
}



# ====== 2. LOSS ASSESSMENTs

# ---- Vectors for Loss Assessment across default thresholds d (a vector for each Delinquency Measure)
# -- total loss vectors across all thresholds d, for each delinquency measure
vec.TotLoss.CD <- rep(0,num.thresholds);
vec.TotLoss.MD <- rep(0,num.thresholds); 
vec.TotLoss.DoD <- rep(0,num.thresholds); 


# ---- Total Loss across Threshold (d)
for (d in 1:num.thresholds) {

  # - get current default threshold from vectors
  d.CD <- vec.d.CD[d]
  d.MD <- vec.d.MD[d]
  d.DoD <- vec.d.DoD[d]
  
  # - get default start times of first default episode (if multiple exist), given threshold d, otherwise return -1 to indicate a performing loan
  # g1: CD
  vec.default.start_first.CD <- sapply(1:n, function(i, thres.d, del.mat, t) {
    # find positions/indexes (corresponding to periods during loan life) in the delinquency matrix where account is (g,d)-defaulting at time t>=0
    vec.found <- which(del.mat[1:(t[i]+1),i] >= thres.d)
    
    if(length(vec.found) == 1) {
      # only one index found, so return that
      episodes.start <- vec.found
    } else {
      # 1. Find positions in these positions where the lagged difference is greater than 1 - these incidate 'breaks' between episodes.
      # 2. Add 1 to these found positions to move to the 'initial starting points' of the next episode in succession
      # 3. Pre-fix this vector with '1' to re-include the first 'episode' that was deselected previously
      # 4. Given this vector of indices, return starting positions again
      episodes.start <-  vec.found[c(1, which(diff(vec.found) > 1) + 1 )]
    }
    # Return starting period of first episode (if it exists, otherwise return -1)
    # Also subtract 1 to account for row 1 indicating t=0
    first.start <- ifelse(length(vec.found) == 0, -1, episodes.start[1] - 1 )
    return(first.start)
  }, thres.d=d.CD, del.mat=mat.CD, t=vec.Term)
  # g2: MD
  vec.default.start_first.MD <- sapply(1:n, function(i, thres.d, del.mat, t) {
    # find positions/indexes (corresponding to periods during loan life) in the delinquency matrix where account is (g,d)-defaulting at time t>=0
    vec.found <- which(del.mat[1:(t[i]+1),i] >= thres.d)
    
    if(length(vec.found) == 1) {
      # only one index found, so return that
      episodes.start <- vec.found
    } else {
      # 1. Find positions in these positions where the lagged difference is greater than 1 - these incidate 'breaks' between episodes.
      # 2. Add 1 to these found positions to move to the 'initial starting points' of the next episode in succession
      # 3. Pre-fix this vector with '1' to re-include the first 'episode' that was deselected previously
      # 4. Given this vector of indices, return starting positions again
      episodes.start <-  vec.found[c(1, which(diff(vec.found) > 1) + 1 )]
    }
    # Return starting period of first episode (if it exists, otherwise return -1)
    # Also subtract 1 to account for row 1 indicating t=0
    first.start <- ifelse(length(vec.found) == 0, -1, episodes.start[1] - 1)
    return(first.start)
  }, thres.d=d.MD, del.mat=mat.MD, t=vec.Term)  
  # g3: DoD
  vec.default.start_first.DoD <- sapply(1:n, function(i, thres.d, del.mat, t) {
    # find positions/indexes (corresponding to periods during loan life) in the delinquency matrix where account is (g,d)-defaulting at time t>=0
    vec.found <- which(del.mat[1:(t[i]+1),i] >= thres.d)
    
    if(length(vec.found) == 1) {
      # only one index found, so return that
      episodes.start <- vec.found
    } else {
      # 1. Find positions in these positions where the lagged difference is greater than 1 - these incidate 'breaks' between episodes.
      # 2. Add 1 to these found positions to move to the 'initial starting points' of the next episode in succession
      # 3. Pre-fix this vector with '1' to re-include the first 'episode' that was deselected previously
      # 4. Given this vector of indices, return starting positions again
      episodes.start <-  vec.found[c(1, which(diff(vec.found) > 1) + 1 )]
    }
    # Return starting period of first episode (if it exists, otherwise return -1)
    # Also subtract 1 to account for row 1 indicating t=0
    first.start <- ifelse(length(vec.found) == 0, -1, episodes.start[1] - 1)
    return(first.start)
  }, thres.d=d.DoD, del.mat=mat.DoD, t=vec.Term)  
  
  # - get (g,d)-defualting account indices across measure, given current thresholds
  def.CD <- which(vec.default.start_first.CD >= 0)
  def.MD <- which(vec.default.start_first.MD >= 0)
  def.DoD <- which(vec.default.start_first.DoD >= 0)
  
  # - get (g,d)-performing account indices across measure, given current thresholds
  perf.CD <- which(vec.default.start_first.CD < 0)
  perf.MD <- which(vec.default.start_first.MD < 0)
  perf.DoD <- which(vec.default.start_first.DoD < 0)
  
  # - deduce the final maturity at which to conduct loss assessment as either contractual term or default time
  # this is for discounting purposes
  vec.maturity.CD <- copy(vec.Term)
  vec.maturity.CD[def.CD] <- vec.default.start_first.CD[def.CD]
  vec.maturity.MD <- copy(vec.Term)
  vec.maturity.MD[def.MD] <- vec.default.start_first.MD[def.MD]
  vec.maturity.DoD <- copy(vec.Term)
  vec.maturity.DoD[def.DoD] <- vec.default.start_first.DoD[def.DoD]  
  
  
  # - Calculate NPV of receipts, given maturity and receipts
  # g1: CD
  vec.ReceiptsPV.CD <- sapply(1:n, function(i,r,t) {
    if (t[i] > 0) {
      val <- sum( r[1:t[i], i] * (1+i_p.alt/12)^(-1*1:(t[i]) ) )
    } else {
      val <- 0
    }
    return (val)
  }, r=mat.Receipt, t=vec.maturity.CD)
  # g2: MD
  vec.ReceiptsPV.MD <- sapply(1:n, function(i,r,t) {
    if (t[i] > 0) {
      val <- sum( r[1:t[i], i] * (1+i_p.alt/12)^(-1*1:(t[i]) ) )
    } else {
      val <- 0
    }
    return (val)
  }, r=mat.Receipt, t=vec.maturity.MD)
  # g3: DoD
  vec.ReceiptsPV.DoD <- sapply(1:n, function(i,r,t) {
    if (t[i] > 0) {
      val <- sum( r[1:t[i], i] * (1+i_p.alt/12)^(-1*1:(t[i]) ) )
    } else {
      val <- 0
    }
    return (val)
  }, r=mat.Receipt, t=vec.maturity.DoD)
  
  
  # - calculate NPV of arrears, given maturity, instalments and receipts
  # g1: CD
  vec.ArrearsPV.CD <- sapply(1:n, function(i,ins,r,t) {
    if (t[i] > 0) {
      val <- sum( ins[i] * (1+i_p.alt/12)^(-1*1:(t[i]) ) ) - r[i]
    } else {
      val <- 0
    }
    return (val)
  }, ins=vec.Instal, r=vec.ReceiptsPV.CD, t=vec.maturity.CD)
  # g2: MD
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
  
  
  # - calculate expected balance, given maturity and remaining time
  # g1: CD
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
  # g2: MD
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
  # g3: DoD
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
  
  
  # - calculate loss vectors, one for each delinquency measure
  vec.Losses.CD <- pmax(vec.ArrearsPV.CD*Arrears.LossRate + vec.ExpBalance.CD*Outstanding.LossRate, 0)
  vec.Losses.MD <- pmax(vec.ArrearsPV.MD*Arrears.LossRate + vec.ExpBalance.MD*Outstanding.LossRate, 0)
  vec.Losses.DoD <- pmax(vec.ArrearsPV.DoD*Arrears.LossRate + vec.ExpBalance.DoD*Outstanding.LossRate, 0)
  
  # - calculate actual balance vectors (ancillary), one for each delinquency measure
  vec.bal.CD <- pmax(vec.ArrearsPV.CD + vec.ExpBalance.CD, 0)
  vec.bal.MD <- pmax(vec.ArrearsPV.MD + vec.ExpBalance.MD, 0)
  vec.bal.DoD <- pmax(vec.ArrearsPV.DoD + vec.ExpBalance.DoD, 0)
  
  # ============ PROFIT/LOSS AGGREGATION
  vec.TotLoss.CD[d] <- sum(vec.Losses.CD, na.rm = T);
  vec.TotLoss.MD[d] <- sum(vec.Losses.MD, na.rm = T);
  vec.TotLoss.DoD[d] <- sum(vec.Losses.DoD, na.rm = T);      
  
  # ---------- Concatenate results
  dat.EL.interim <- rbind(data.table(Measure="CD",Threshold=d.CD,
                                     Vol_Perf=length(perf.CD), Vol_Def=length(def.CD),
                                     Bal_Perf = sum(vec.bal.CD[perf.CD], na.rm = T), Bal_Def = sum(vec.bal.CD[def.CD], na.rm = T),
                                     Loss = vec.TotLoss.CD[d]),
                          data.table(Measure="MD",Threshold=d.MD,
                                     Vol_Perf=length(perf.MD), Vol_Def=length(def.MD),
                                     Bal_Perf = sum(vec.bal.MD[perf.MD], na.rm = T), Bal_Def = sum(vec.bal.MD[def.MD], na.rm = T),
                                     Loss = vec.TotLoss.MD[d]),
                          data.table(Measure="DoD",Threshold=d.DoD,
                                     Vol_Perf=length(perf.DoD), Vol_Def=length(def.DoD),
                                     Bal_Perf = sum(vec.bal.DoD[perf.DoD], na.rm = T), Bal_Def = sum(vec.bal.DoD[def.DoD], na.rm = T),
                                     Loss = vec.TotLoss.DoD[d])
  ) 
  
  # --- concatenate EL estimates
  if (d == 1) {
    dat.EL <- dat.EL.interim
  }else {
    dat.EL <- rbind(dat.EL, dat.EL.interim)
  }
  
}

# - last data preparation
setDT(dat.EL, key=c("Measure","Threshold"))
dat.EL[, Loss := Loss/ sum(vec.Principal)] # convert into loss %
write.csv(x=dat.EL, file=paste0(given.filename,'.csv'),row.names=F)
save.image(file = paste0(given.filename, '.RData'))




# =========== Loss Plots
# Note these loss plots are only experimental. There is a much more manicured version that produces the graphs actually used in the research article.

# -- structure final results for plotting purposes
plot.data <- rbind( data.table(Measure="g1: CD", Threshold=vec.d.CD, Loss=vec.TotLoss.CD),
                    data.table(Measure="g2: MD", Threshold=vec.d.MD, Loss=vec.TotLoss.MD),
                    data.table(Measure="g3: DoD", Threshold=vec.d.DoD, Loss=vec.TotLoss.DoD) )
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



minima <- function() {
  cat("CD: Minimum Loss at threshold d =", vec.d.CD[Position(function(x) x==min(vec.TotLoss.CD),vec.TotLoss.CD)], " at position",Position(function(x) x==min(vec.TotLoss.CD),vec.TotLoss.CD), "in threshold vector")
  cat("\nMD: Minimum Loss at threshold d =", vec.d.MD[Position(function(x) x==min(vec.TotLoss.MD),vec.TotLoss.MD)], " at position", Position(function(x) x==min(vec.TotLoss.MD),vec.TotLoss.MD), "in threshold vector")
  cat("\nDoD: Minimum Loss at threshold d =", vec.d.DoD[Position(function(x) x==min(vec.TotLoss.DoD),vec.TotLoss.DoD)], " at position",Position(function(x) x==min(vec.TotLoss.DoD),vec.TotLoss.DoD), "in threshold vector") 
}
minima()

proc.time() - ptm #IGNORE: computation time taken