# ================= Graphs for showcasing Markovian technique at specific parametrisations
# This script creates the detailed and manicured graphs used for the research article

# ---- library setup
require(ggplot2)
require(hexbin)
require(gganimate)
require(transformr) # necessary for gganimate
require(RColorBrewer)
require(ggthemes)
require(scales)
require(extrafont)
require(data.table)
require(ETLUtils)
require(ffbase)
require(ff)


# ---- Plotting data loading & Preparation
# --- stitch the result sets from specific experiments together into one dataset for plotting
it.name <- "v1_1a(xvii)"; iter <- 1
unpack.ffdf(paste0("SpecLossProc-", it.name))
plot.data <- data.table(Setup=paste0('"("*',toupper(letters[iter]),'*")"~italic(P)[PP] == ',unique(dat.EL$P_PP)[1]*100, '*"%, "~italic(P)[DD] == ',
                                     unique(dat.EL$P_DD)[1]*100, '*"%"'), ExperimentNum=iter, dat.EL)
it.name <- "v1_1a(xviii)"; iter <- 2
unpack.ffdf(paste0("SpecLossProc-", it.name))
plot.data <- rbind(plot.data,
                   data.table(Setup=paste0('"("*',toupper(letters[iter]),'*")"~italic(P)[PP] == ',unique(dat.EL$P_PP)[1]*100, '*"%, "~italic(P)[DD] == ',
                                            unique(dat.EL$P_DD)[1]*100, '*"%"'), ExperimentNum=iter, dat.EL)
                   )
it.name <- "v1_1a(xv)"; iter <- 3
unpack.ffdf(paste0("SpecLossProc-", it.name))
plot.data <- rbind(plot.data,
                   data.table(Setup=paste0('"("*',toupper(letters[iter]),'*")"~italic(P)[PP] == ',unique(dat.EL$P_PP)[1]*100, '*"%, "~italic(P)[DD] == ',
                                           unique(dat.EL$P_DD)[1]*100, '*"%"'), ExperimentNum=iter, dat.EL)
)


# -- Notes on discarded experiments:
# i) too boring, only g1 has optima
# ii) i) made more interesting using truncation. But will raise question of why truncate only this one and not the others? rather leave out.
# iii) too similar to iv) while the latter has better synergy (P=90%) with other chosen experiments
# iv) similar to xv, though the latter has a checkmark-shape, which is preferably to the straight-line shape of g1 of the former
# v) too similar to i), which makes it also boring; only g1 has optima, while g2/g3 are largely L-shaped
# vi) PP=95%, DD=95%; too similar to newer xv, which has straighter g1 curve.
# vii) all measures are L-shaped, g1 very slight optima (too faint), but rather opt for xiv (PP=90%, DD=5%)
# viii) PP=75%, DD=75%; though interesting, g1's wonky shape may raise more questions than worth answering. we simply want to show optima exist across all measures
# ix) PP=85%, DD=80%; g1 has wonky optima, while g2/g3 has very slight optima (not as clear as those from other experiments). will raise more questions than it's worth
# x) too similar to xii, but using PP=20% instead of 10%; breaks synergy
# xii) similar to xvii, though the latter has deeper optima
# xiii) very similar to xviii, though the latter uses a completely different parametrisation, which adds diversity
# xi) too similar to xiii, but using PP=20% instead of 10%; breaks synergy
# xvi) PP=95% DD=98%; too similar to xv), though shifted to lower loss ranges on y-axis. however, deeper optima that may be visually appealing





# =========== Main Loss Plot (facetted)
# Note: the graph produced here is used in the main text

# ---- Plotting data preparation

# -- Some light feature engineering for plotting purposes
plot.data[, LossRate := Loss/ sum(vec.Principal)] # convert into loss %
# prepare legend entry labels
plot.data[, Measure_Annotate := case_when(Measure == "CD"~paste0('italic(g)[1]'),
                                          Measure == "MD"~paste0('italic(g)[2]'),
                                          Measure == "DoD"~paste0('italic(g)[3]'))]
# obtain minima and associated thresholds
plot.data[, Min_Loss := min(LossRate, na.rm=T), by=list(Setup, Measure)]
plot.data[, Min_Threshold := .SD[LossRate == Min_Loss, Threshold][1], by=list(Setup, Measure)]


# -- subsetting for geom_point to avoid overplotting
# - Experiment 1
treat.data <- subset(plot.data, ExperimentNum==1 & Measure == "MD" & Threshold <=10)
treat.data2 <- subset(plot.data, ExperimentNum==1 & Measure == "DoD" & Threshold <=10)
plot.data2 <- rbind(treat.data[Threshold<=2,], treat.data[seq(2,nrow(treat.data),by=3),], 
                    treat.data2[Threshold<=3,], treat.data2[seq(3,nrow(treat.data2),by=3),], 
                    subset(plot.data, ExperimentNum==1 & !(Measure %in% c("MD","DoD") & Threshold <=10)) # add rest of data back in
                    )
# - Experiment 2
treat.data <- subset(plot.data, ExperimentNum==2 & Measure == "MD" & Threshold <=10)
treat.data2 <- subset(plot.data, ExperimentNum==2 & Measure == "DoD" & Threshold <=10)
dod.min <- subset(plot.data, ExperimentNum==2 & Measure == "DoD" & LossRate==Min_Loss)$Min_Threshold
plot.data2 <- rbind(plot.data2,
                    treat.data[Threshold<=2.5,], treat.data[seq(2.5,nrow(treat.data)-1,by=3),], 
                    treat.data2[Threshold<=3,], treat.data2[seq(3,dod.min,by=3),], treat.data2[seq(dod.min+1,nrow(treat.data2)-1,by=3),], 
                    subset(plot.data, ExperimentNum==2 & !(Measure %in% c("MD","DoD") & Threshold <=10)) # add rest of data back in
)
# - Experiment 3
#plot.data2 <- rbind(plot.data2, subset(plot.data, ExperimentNum==3))
treat.data <- subset(plot.data, ExperimentNum==3 & Measure == "MD" & Threshold <=5)
treat.data2 <- subset(plot.data, ExperimentNum==3 & Measure == "DoD" & Threshold <=5)
plot.data2 <- rbind(plot.data2,
                    treat.data[Threshold<=1.5,], treat.data[seq(1.5,nrow(treat.data)-1,by=3),],
                    treat.data2[Threshold<=1.5,], treat.data2[seq(1.5,nrow(treat.data2)-1,by=3),],
                    subset(plot.data, ExperimentNum==3 & !(Measure %in% c("MD","DoD") & Threshold <=5)) # add rest of data back in
)

# legend entries named list to be parsed (plotmath)
label.v <- as.list(sort(unique(plot.data$Measure_Annotate)))
label.v <- lapply(label.v, FUN = function(x) parse(text=x)[[1]])


# - ancillary plotting vectors
chosenFont <- "Times New Roman"



# ---- full plot
(plot.full <- ggplot(plot.data, aes(x=Threshold, y=LossRate, group=MeasureName)) + 
  geom_point(aes(x=Threshold,y=LossRate, color=MeasureName, shape=MeasureName), size=1.75, data=plot.data2) +
  geom_line(aes(x=Threshold, y=LossRate, color=MeasureName), size = 0.5) + 
  # minimum point
  geom_point(aes(x=Min_Threshold,y=Min_Loss), size=6, colour="gray15", shape=1) +   
  facet_wrap(~Setup, labeller = label_parsed, scales="free") +
  labs(y="Loss (%)",x=bquote(Thresholds~italic(d))) + theme_minimal() + 
  theme(text=element_text(family=chosenFont, size=12),legend.position="bottom",
        strip.text.x = element_text(colour="gray30", face="italic", size=10), 
        strip.background = element_rect(fill="gray90", colour=NA)) + 
  scale_color_economist(name="Delinquency Measure",guide = guide_legend(nrow=1), labels=label.v) +
  scale_shape_manual(values=c(1,16,8), 
                     name="Delinquency Measure",guide = guide_legend(nrow=1), labels=label.v) +
  scale_y_continuous(breaks= pretty_breaks(), labels=percent)
)

# - save plot
dpi <- 150
ggsave(plot.full, file=paste0("LossThresh_Markovian_Experiments.png"),width=1200/dpi, height=500/dpi,dpi=dpi)

