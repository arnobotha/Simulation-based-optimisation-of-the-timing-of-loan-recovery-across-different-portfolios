# ================= Graphs for showcasing Markovian technique
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
require(plot3D)
require(plot3Drgl)
require(plotly)

# ---- Plotting data loading & Preparation
outer.iter.max <- 256
it.name <- "v1_1d(i)"
unpack.ffdf(paste0("LossProc",outer.iter.max, "-", it.name))

plot.data <- copy(dat.EL) #subset(dat.EL, Iteration %in% c(seq(from=1, by=7, length.out=7), seq(from=2, by=7, length.out=7)))

# - dependent on simulation parameters
port.sum <- sum(vec.Principal, na.rm=T)
plot.data[, Loss_Rate := Loss / port.sum]
plot.data[, Default_Rate := Bal_Def / port.sum ]
plot.data[, DefaultVol_Rate := Vol_Def / n ]
plot.data[, Iteration_T := paste0("P_PP: ", round(P_PP*100,digits=1), "%, P_DD: ", round(P_DD*100,digits=1), "%")]
plot.data[, P_PP_T := round(P_PP*100,digits=1)]
plot.data[, P_DD_T := round(P_DD*100,digits=1)]
# create group indices
pp.states <- sort(unique(plot.data$P_PP_T))
dd.states <- sort(unique(plot.data$P_DD_T))
plot.data[, P_PP_Ind := which(P_PP_T==pp.states), by=list(P_PP_T)]
plot.data[, P_DD_Ind := which(P_DD_T==dd.states), by=list(P_DD_T)]
# create explicitly numbered and annotated states for plotting purposes
plot.data[, P_PP_T2 := paste0(letters[P_PP_Ind],". ", P_PP_T, "%")] # for cycling correctly
plot.data[, P_DD_Annotate := paste0(letters[P_DD_Ind],'.~italic(P)[DD]  ==  ', P_DD_T, '*"%"')]
# create annotated variables for plotting purposes
plot.data[, Min_Loss := min(Loss_Rate, na.rm=T), by=list(Iteration)]
plot.data[, Min_Threshold := .SD[Loss_Rate == Min_Loss, Threshold], by=list(Iteration)]
plot.data[, Annotate_text := .SD[, ifelse(Loss_Rate == Min_Loss, 
                                          paste0("italic(P)[PP] == ", P_PP_T[1],"*'%,'~~minimum~loss~at~italic(d) == ", Min_Threshold),
                                          NA)], by=list(Iteration)]

# - feature engineering | requires "(i)"-type of experiments that include statistical information on default episodes
plot.data[, Mean_Duration_Episode := Episode_Length_Mean_AccountMean / Episode_Number_Mean]

# perform the following data preparation only for "#(iii)" series of experiments, e.g., "v1_1d(iii)"
# Note: This is for testing out a set of statistics calculated from all portfolios when deliberately imposing d=3 as threshold,
# simply for comparison or translation to real-world
if (substr(it.name, nchar(it.name, keepNA=T)-4, nchar(it.name, keepNA=T)) == "(iii)") {
  plot.data[, Mean_Duration_Episode_d3 := Episode_Length_Mean_AccountMean_d3 / Episode_Number_Mean_d3]
  plot.data[, DefaultVol_Rate_d3 := Vol_Def_d3 / n ]
  use.d3 <- T # simple switch for logical evaluation later in script
} else {
  use.d3 <- F # simple switch for logical evaluation later in script
}




# =========== Plotting code: Animated graph
chosenFont <- "Times New Roman"

# -- full plot
plot.full <- ggplot(plot.data, aes(x=Threshold, y=Loss_Rate)) + theme_minimal() + 
  geom_point(aes(x=Threshold,y=Loss_Rate), size=1.75, colour="#2c7fb8") +
  geom_line(aes(x=Threshold,y=Loss_Rate), size=0.5, colour="#2c7fb8") +
  # minimum point
  geom_point(aes(x=Min_Threshold,y=Min_Loss), size=8, colour="gray15", shape=1) + 
  geom_text(aes(x=Min_Threshold,y=Min_Loss,label= round(Min_Threshold, digits=0)),size=5, colour="gray15", family=chosenFont) + 
  geom_text(x=16, y=0.78, aes(label=Annotate_text), size=5, colour="gray15", parse=T, family=chosenFont) + 
  # vertical + horizontal lines connecting to minimum point
  geom_segment(aes(x=0, xend=Min_Threshold, y=Min_Loss, yend=Min_Loss), linetype="dashed") + 
  geom_segment(aes(x=Min_Threshold,xend=Min_Threshold, y=0, yend=Min_Loss), linetype="dashed") + 
  facet_wrap(~P_DD_Annotate, labeller = label_parsed) +
  labs(y="Loss (%)", x = expression(paste("Thresholds", {~italic(d)}, " for ", {italic(g)[1]})),#bquote({Default~thresholds~italic(d)~on~italic(g)[1]}),
       title="Loss curves (Markovian technique)", subtitle=paste0("Probability of staying in performing state: {closest_state}%") ) + 
  theme(text=element_text(family=chosenFont, size=13),
        legend.position="bottom", strip.text.x = element_text(colour="gray30", face="italic", size=14), 
        strip.background = element_rect(fill="gray90", colour=NA),
        plot.title = element_text(size=16, face="bold"),
        plot.subtitle = element_text(face="italic", size=14)) + 
  scale_y_continuous(breaks=pretty_breaks(), labels=percent) + 
  transition_states(states=P_PP_T2, transition_length=1, state_length=1) + ease_aes('linear')

# - Historical animation settings per value of outer.iter.max | change manually for best performance
# 49: 10 fps
# 81: 16 fps
# 144: 18 fps
# 256: 20 fps
animate(plot.full, nframes=2*outer.iter.max-1, fps=20, width=1200, height=1150)

# - save animated plot as gif
anim_save(filename=paste0("LossProc-", outer.iter.max,"-", it.name, ".gif"))




# =========== Plotting code: Static graph: Loss vs Threshold by P_PP series, faceted by P_DD
# Note: the graph produced here is used in the main text

# ---- Plotting data preparation
# - select a few series
(selected.PP <- c(1,5,9,11,12,13,14,15,16))
(selected.DD <- c(1,2,3,4,6,8,10,12,16) )
plot.data2 <- subset(plot.data, P_PP_Ind %in% selected.PP & P_DD_Ind %in% selected.DD)
# - recreate group indices
pp.states2 <- sort(unique(plot.data2$P_PP_T))
dd.states2<- sort(unique(plot.data2$P_DD_T))
plot.data2[, P_PP_Ind2 := which(P_PP_T==pp.states2), by=list(P_PP_T)]
plot.data2[, P_DD_Ind2 := which(P_DD_T==dd.states2), by=list(P_DD_T)]
# - create explicitly numbered and annotated states for plotting purposes
plot.data2[, P_PP_T3 := paste0(letters[P_PP_Ind2],". ", P_PP_T, "%")] # for cycling correctly
plot.data2[, P_DD_Annotate2 := paste0(letters[P_DD_Ind2],'.~italic(P)[DD]  ==  ', P_DD_T, '*"%"')]
# - create another subselection for points only (otherwise overplotting occurs)
plot.data3 <- subset(plot.data2, Threshold %in% seq(from=0,to=max(plot.data$Threshold), by=2) | Threshold==Min_Threshold)

# - ancillary plotting vectors
shape.v <- c(15,16,17,0,1,2, 4,7,13) # shape vector
col.v <- (brewer.pal(n=length(shape.v), name="Paired")) # colour vector

# -- full plot
(plot.full2 <- ggplot(plot.data2, aes(x=Threshold, y=Loss_Rate)) + theme_minimal() + 
  geom_point(aes(x=Threshold, y=Loss_Rate, colour=P_PP_T3, shape=P_PP_T3), size=1.75, data=plot.data3) +
  geom_line(aes(x=Threshold, y=Loss_Rate, colour=P_PP_T3), size=0.5) +
  # minimum point
  geom_point(aes(x=Min_Threshold,y=Min_Loss), size=4, colour="gray15", shape=1) + 
  facet_wrap(~P_DD_Annotate2, labeller = label_parsed, scales="free") +
  labs(y="Loss (%)", x = expression(paste("Thresholds", {~italic(d)}, " for ", {italic(g)[1]})) ) + 
  theme(text=element_text(family=chosenFont, size=13),
        legend.position="bottom", strip.text.x = element_text(colour="gray30", face="italic", size=10), 
        strip.background = element_rect(fill="gray90", colour=NA)) + 
  scale_y_continuous(breaks=pretty_breaks(), labels=percent) +
  scale_shape_manual(name=bquote(italic(P)[PP]), values=shape.v) + 
  scale_colour_manual(name=bquote(italic(P)[PP]), values=col.v)
)

# - save plot
dpi <- 150
ggsave(plot.full2, file=paste0("LossThresh_CD_Markovian_", outer.iter.max, "-", it.name,".png"),width=1200/dpi, height=1100/dpi,dpi=dpi)






# =========== Plotting code: Static graph -  Mean_Duration_Episode vs P-PP, facetted by P-DD, coloured by d^g (optimal threshold)

# - aggregate to minimum loss-level
plot.data2 <- subset(plot.data, Threshold==Min_Threshold)
plot.data2[, list(Count =.N),by=list(Iteration)][Count>1,] # should be empty

# - recreate group indices
pp.states2 <- sort(unique(plot.data2$P_PP_T))
dd.states2<- sort(unique(plot.data2$P_DD_T))
plot.data2[, P_PP_Ind2 := which(P_PP_T==pp.states2), by=list(P_PP_T)]
plot.data2[, P_DD_Ind2 := which(P_DD_T==dd.states2), by=list(P_DD_T)]

# - create explicitly numbered and annotated states for plotting purposes
plot.data2[, P_DD_Annotate2 := paste0(letters[P_DD_Ind2],'.~italic(P)[DD]  ==  ', P_DD_T, '*"%"')]
# create point-wise labels
plot.data2[, Default_Label_Pos := Default_Rate - (max(Default_Rate)-min(Default_Rate))*.1, by=list(P_DD_Annotate2)]

# - ancillary plotting vectors
shape.v <- c(15,16,17,0,1,2, 4,7,13) # shape vector
col.v <- brewer.pal(n=9, name="Paired") # colour vector

# -- full plot
(plot.full3 <- 
    ggplot(plot.data2) + theme_minimal() + 
    geom_point(aes(x=P_PP, y=Mean_Duration_Episode, colour=Threshold), size=1.75) +
    geom_line(aes(x=P_PP, y=Mean_Duration_Episode, colour=Threshold), size=0.5) +
    geom_text(aes(x=P_PP, y=Mean_Duration_Episode, label=Threshold), colour="gray30", size=2.5) + 
    facet_wrap(~P_DD_Annotate2, labeller = label_parsed, scales="free") +
    labs(y="Mean duration / mean episode count (ratio)", x=expression(paste({italic(P)[PP]}," (%)")) ) + 
    theme(text=element_text(family=chosenFont, size=12),
          legend.position="bottom", strip.text.x = element_text(colour="gray30", face="italic", size=10), 
          strip.background = element_rect(fill="gray90", colour=NA)) + 
    scale_y_continuous(breaks=pretty_breaks(), labels=comma) + 
    scale_x_continuous(breaks=pretty_breaks(), labels=percent) + 
    scale_colour_gradient(name=bquote(italic(d)^{(italic(g)[1])}), low=col.v[1], high=col.v[2])
)

# - save plot
dpi <- 150
ggsave(plot.full3, file=paste0("MeanDuration-Ppp_d_CD_Markovian_", outer.iter.max, "-", it.name, ".png"),width=1200/dpi, height=1100/dpi,dpi=dpi)






# =========== Plotting code: Static graph - Default rate vs P-PP, facetted by P-DD, coloured by d^g (optimal threshold)

# - aggregate to minimum loss-level
plot.data2 <- subset(plot.data, Threshold==Min_Threshold)
plot.data2[, list(Count =.N),by=list(Iteration)][Count>1,] # should be empty

# - recreate group indices
pp.states2 <- sort(unique(plot.data2$P_PP_T))
dd.states2<- sort(unique(plot.data2$P_DD_T))
plot.data2[, P_PP_Ind2 := which(P_PP_T==pp.states2), by=list(P_PP_T)]
plot.data2[, P_DD_Ind2 := which(P_DD_T==dd.states2), by=list(P_DD_T)]

# - create explicitly numbered and annotated states for plotting purposes
plot.data2[, P_DD_Annotate2 := paste0(letters[P_DD_Ind2],'.~italic(P)[DD]  ==  ', P_DD_T, '*"%"')]
# create point-wise labels
plot.data2[, Default_Label_Pos := Default_Rate - (max(Default_Rate)-min(Default_Rate))*.1, by=list(P_DD_Annotate2)]

# - ancillary plotting vectors
shape.v <- c(15,16,17,0,1,2, 4,7,13) # shape vector
col.v <- brewer.pal(n=9, name="Paired") # colour vector

# -- full plot
(plot.full4 <- 
    ggplot(plot.data2) + theme_minimal() + 
    geom_point(aes(x=P_PP, y=Default_Rate, colour=Threshold), size=1.75) +
    geom_line(aes(x=P_PP, y=Default_Rate, colour=Threshold), size=0.5) +
    geom_text(aes(x=P_PP, y=Default_Label_Pos, label=Threshold), colour="gray30", size=2.5) + 
    facet_wrap(~P_DD_Annotate2, labeller = label_parsed, scales="free") +
    labs(y="Default rate (%)", x=expression(paste({italic(P)[PP]}," (%)")) ) + 
    theme(text=element_text(family=chosenFont, size=12),
          legend.position="bottom", strip.text.x = element_text(colour="gray30", face="italic", size=10), 
          strip.background = element_rect(fill="gray90", colour=NA)) + 
    scale_y_continuous(breaks=pretty_breaks(), labels=percent) + 
    scale_x_continuous(breaks=pretty_breaks(), labels=percent) + 
    scale_colour_gradient(name=bquote(italic(d)^{(italic(g)[1])}), low=col.v[1], high=col.v[2])
)

# - save plot
dpi <- 150
ggsave(plot.full4, file=paste0("DefaultRate-Ppp_d_CD_Markovian_", outer.iter.max, "-",it.name, ".png"),width=1200/dpi, height=1100/dpi,dpi=dpi)





# =========== Plotting code: Static graph - Default rate vs mean duration per episode, colour by optimal threshold
# Note: the graph produced here is used in the main text

# - aggregate to minimum loss-level
plot.data2 <- subset(plot.data, Threshold==Min_Threshold)
plot.data2[, list(Count =.N),by=list(Iteration)][Count>1,] # should be empty
plot.data2[, Default_Label_Pos := DefaultVol_Rate - (max(DefaultVol_Rate)-min(DefaultVol_Rate))*.02]
plot.data3 <- subset(plot.data2, Mean_Duration_Episode < 31)

col.f <- colorRampPalette((brewer.pal(name="Spectral", n=4))) # function to interpolate to given number of colours

# fit curve as visual aid
poly_reg1 <- loess(formula = DefaultVol_Rate ~ Mean_Duration_Episode, data=plot.data3, family = "symmetric",
                   degree = 2, span = 0.7, normalize=F)
summary(poly_reg1)

#retrieve fitted points for droplines to surface
fitpoints <- predict(poly_reg1, newdata=plot.data3) 
plot.data3[, DefaultVol_Rate_Fitted := fitpoints]

# -- full plot
(plot.full5 <-
    ggplot(plot.data3) + theme_minimal() +
    geom_line(aes(x=Mean_Duration_Episode, y=DefaultVol_Rate_Fitted, colour=Threshold), size=1.25) + 
    geom_point(aes(x=Mean_Duration_Episode, y=DefaultVol_Rate, colour=Threshold)) +
    geom_text(aes(x=Mean_Duration_Episode, y=Default_Label_Pos, label=Threshold), colour="gray30", size=2.5) + 
    scale_colour_gradientn(colours=col.f(50), name=bquote("Optimal threshold "*italic(d)^{(italic(g)[1])})) + 
    labs(y=bquote("("*italic(g)*","*italic(d)*")-default rate (%)"), x=bquote("Mean duration per ("*italic(g)*","*italic(d)*")-default episode (months)")) + #("Mean duration per default episode (months)" ) + 
    theme(text=element_text(family=chosenFont, size=12),
          legend.position=c(0.3,0.08), legend.direction="horizontal", legend.justification=c(1,0)) +     
    scale_y_continuous(breaks=pretty_breaks(), labels=percent, lim=c(min(plot.data$DefaultVol_Rate), 0.75)) +
    scale_x_continuous(breaks=pretty_breaks(), labels=comma, lim=c(min(plot.data2$Mean_Duration_Episode),31)) + 
    guides(colour = guide_colourbar(barwidth=7, barheight=1, title.position="top", title.hjust=0.5))
)

# - save plot
dpi <- 200
ggsave(plot.full5, file=paste0("DefaultRate-MeanDuration_byThreshold_CD_Markovian_", outer.iter.max, "-",it.name, ".png"),width=1200/dpi, height=1100/dpi,dpi=dpi)


# --- static d=3 variant of this plot
# only for interest, not used in submission
if (use.d3 == T) {
  
  # - aggregate to minimum loss-level
  plot.data2 <- subset(plot.data, Threshold==Min_Threshold)
  plot.data2[, list(Count =.N),by=list(Iteration)][Count>1,] # should be empty
  plot.data2[, Default_Label_Pos := DefaultVol_Rate_d3 - (max(DefaultVol_Rate_d3)-min(DefaultVol_Rate_d3))*.02]
  plot.data3 <- subset(plot.data2, Mean_Duration_Episode_d3 < 28) #copy(plot.data2) #
  
  col.f <- colorRampPalette((brewer.pal(name="Spectral", n=4))) # function to interpolate to given number of colours
  
  # fit curve as visual aid
  poly_reg1 <- loess(formula = DefaultVol_Rate_d3 ~ DefaultVol_Rate, data=plot.data3, family = "symmetric",
                     degree = 2, span = 0.7, normalize=F)
  summary(poly_reg1)
  
  #retrieve fitted points for droplines to surface
  fitpoints <- predict(poly_reg1, newdata=plot.data3) 
  plot.data3[, DefaultVol_Rate_Fitted := fitpoints]
  
  # -- full plot
  (plot.full5a <-
      ggplot(plot.data3) + theme_minimal() +
      geom_line(aes(x=DefaultVol_Rate, y=DefaultVol_Rate_Fitted, colour=Threshold), size=1.25) + 
      geom_point(aes(x=DefaultVol_Rate, y=DefaultVol_Rate_d3, colour=Threshold)) +
      geom_text(aes(x=DefaultVol_Rate, y=Default_Label_Pos, label=Threshold), colour="gray30", size=2.5) + 
      scale_colour_gradientn(colours=col.f(50), name=bquote("Optimal threshold "*italic(d)^{(italic(g)[1])})) + 
      labs(y=bquote("("*italic(g)*",3)-default rate (%)"), 
           x=bquote("("*italic(g)*","*italic(d)*")-default rate (%)")
           #x=bquote("Mean duration per ("*italic(g)*", 3)-default episode (months)")
           ) + 
      theme(text=element_text(family=chosenFont, size=12),
            legend.position=c(0.3,0.08), legend.direction="horizontal", legend.justification=c(1,0)) +     
      scale_y_continuous(breaks=pretty_breaks(), labels=percent) +
      scale_x_continuous(breaks=pretty_breaks(), labels=percent, lim=c(0,0.4)) + 
      guides(colour = guide_colourbar(barwidth=7, barheight=1, title.position="top", title.hjust=0.5))
  )  
  
}



# =========== Plotting code: 3D graph
# Note: the graph produced here is used in the main text

# - aggregate to minimum loss-level
plot.data2 <- subset(plot.data, Threshold==Min_Threshold)
plot.data2[, list(Count =.N),by=list(Iteration)][Count>1,] # should be empty

# - further exclude outliers
plot.data3 <- subset(plot.data2, Mean_Duration_Episode <= 30) #not used

# quick plot
qplot(x=Mean_Duration_Episode, y=DefaultVol_Rate, data=plot.data2, colour=Threshold)
qplot(x=Mean_Duration_Episode, y=DefaultVol_Rate, data=plot.data3, colour=Threshold)

# fit a polynomial regression of certain degree
poly_reg <- lm(formula = Threshold ~ polym(Mean_Duration_Episode, DefaultVol_Rate, degree=3), data=plot.data2)
summary(poly_reg)

poly_reg2 <- lm(formula = Threshold ~ polym(Mean_Duration_Episode, DefaultVol_Rate, degree=4), data=plot.data3)
summary(poly_reg2)

poly_reg3 <- loess(formula = Threshold ~ Mean_Duration_Episode * DefaultVol_Rate, data=plot.data3, family = "symmetric",
                   degree = 1, span = 0.8, normalize=F)
summary(poly_reg3)

poly_reg4 <- loess(formula = Threshold ~ Mean_Duration_Episode * DefaultVol_Rate, data=plot.data2, family = "symmetric",
                   degree = 2, span = 0.75, normalize=F)
summary(poly_reg4)

# -- basic 3d plot
x <- plot.data2$DefaultVol_Rate
y <- plot.data2$Mean_Duration_Episode
z <- plot.data2$Threshold

# - predict values on regular xy grid
grid.lines <- 52
x.pred <- seq(min(x), max(x), length.out = grid.lines)
y.pred <- seq(min(y), max(y), length.out = grid.lines)
xy <- expand.grid(DefaultVol_Rate=x.pred, Mean_Duration_Episode=y.pred);
z.pred <- matrix(predict(poly_reg4, newdata = xy), nrow=grid.lines, ncol=grid.lines)
fitpoints <- predict(poly_reg4, newdata=plot.data2) #retrieve fitted points for droplines to surface

# - draw graph
png(filename=paste0("DefaultRate-MeanDuration-Threshold_CD_Markovian_", outer.iter.max, "-", it.name, ".png"), width=1000, height=1100, res=170)
par(family="Times New Roman", mar=c(1.15,0.5,0,0))
scatter3D(x*100, y, z, clab = c("Optimal threshold"), pch=19, cex=0.5, bty="b2", col=col.f(50),
          colkey=list(side=1, width=0.6, length=0.7, dist=-0.02, line.clab=1), ticktype="detailed", xlab = "(g,d)-default rate (%)",
          ylab = "Mean duration per (g,d)-default episode (months)", zlab = "Optimal threshold", #ticktype = "detailed",
          surf = list(x = x.pred*100, y = y.pred, z = z.pred, facets= NA, lighting=list(ambient=0.6, diffuse=0.6) #,fit=fitpoints
                      ), 
          theta=240, phi=30)
dev.off()


plotrgl()





# -- plotly version | not used
col.v <- brewer.pal(n=4, name="Spectral")
fig <- plot_ly(plot.data2, name="obj1", x=~Mean_Duration_Episode, y=~DefaultVol_Rate*100, z=~Threshold, color=~Threshold, 
               type="scatter3d", mode="markers", marker = list(size = 3, showscale=T, symbol="circle",
                                                               colorbar=list(outlinewidth=0, dtick=3,
                                                                             title=list(text="Threshold (d)"))), 
               colors=col.v, showlegend=T)
fig <- fig %>% add_trace(name="ob2", type="mesh3d", 
                         z=as.matrix(predict(poly_reg,newdata=plot.data2)), 
                         intensity=~Threshold, showscale=F, showlegend=F, opacity=0.6, 
                         contour=list(show=TRUE, color="white", width=10, lwd=10, opacity=1, hoverinfo="none"),
                         flatshading=F, lighting=list(ambient=0.7))
fig %>% layout(scene = list(xaxis = list(title="Mean duration / mean episode count (ratio)"), 
                      yaxis = list(title="Default rate (%)",  ticksuffix="%"),
                      zaxis = list(title="Optimal threshold (d)"),
                      aspectmode = "manual", aspectratio = list(x=0.8,y=1,z=0.45)),
               font = list(family="Times New Roman"))




