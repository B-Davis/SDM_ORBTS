rm(list = ls())
load("ORBTSmodelout.RData")

###################################### Relative importance Plot
# png("../plots/Priority%03d.png")
par(mar = c(4,16,1,1))
a <- barplot(rev(mod.out2[[2]]),horiz = T,col = "grey50",border = F)
axis(2,a,rev(mod.out2[[1]]),las = 2)
title(xlab = "Relative Priority",line = 2.4)
title(ylab = "Core Area",line = 14)
# dev.off()

####################################### 1-way Sensy Plot
rdr <- order(model(PV)$`relative priority`)
xyz <- sapply(c(2,1,3),function(x) seq(1,dim(oneway)[x],1))
par(mfrow = c(1,dim(oneway)[3]),mar = rep(.1,4),oma = c(3,15,3,.8))
xlb <- 24.8 # x label adj.
ylb <- 4.4
# png("../plots/SensitivityAll%03d.png")
for(z in xyz[[3]]) {
  # for each parameter
  plot.new()
  plot.window(range(oneway), range(xyz[[2]]))
  for(asd in seq(1.5,24.5,2)) polygon(par("usr")[c(1,2,2,1)],rep(c(asd-1,asd),each = 2),border = F,col = "grey90") # horizontal shading
  box(col = "grey70")
  for (i in xyz[[2]]) { # changed xyz[[2]] to rdr (reordering by score)
    # range for each core area
    segments(min(oneway[rdr[i], , z]),
             xyz[[2]][i],
             max(oneway[rdr[i], , z]),
             xyz[[2]][i],
             lwd = 1.4,
             lend = 2)
    if (z == 1) # y labels
      text(mean(range(oneway)) - ylb,
           xyz[[2]][i],
           labels = cores[rdr[i]],
           xpd = NA,
           adj = 1)
  }
  points(apply(oneway[rdr, , z], 1, median), xyz[[2]], pch = 3, col = "red",cex = .6) # medians
  text(mean(range(oneway)),xlb,names(nms)[z],xpd =NA)
  axis(1,pretty(c(range(oneway)[1], range(oneway)[2])),pretty(c(range(oneway)[1], range(oneway)[2])),las = 1)
}
################################## Tornados
# oneway

ca <- 15

owf <- function(ca){ 
if(dev.cur() > 1) dev.off()
tmp <- oneway[ca,,]
ii <- order(apply(tmp,2,function(x) abs(diff(x))))
par(mar = c(5.1, 4.1 + 4, 4.1, 2.1))
plot.new();plot.window(c(0,10),c(1,length(nms)))
for(i in seq_along(nms)){
xx <- range(tmp[,ii][,i])
if(i == 1) abline(v = model(PV)$`relative priority`[ca],lty = 2, col = 2)
segments(x0 = xx[1],y0 = i,x1 = xx[2],y1 = i,lwd = 7,lend = 1)
}
axis(2,at = seq_along(nms),names(nms[ii]),las = 2,col = "grey80")
axis(1,seq(0,10,1),col = "grey80",col.axis ="grey80")
title(main = dimnames(oneway)[[1]][ca])
par(mar = c(5.1, 4.1, 4.1, 2.1))
}


owf(12)


############################ TWO-WAY
dim(twoway)
dimnames(twoway)

tmp <- twoway[,,"abund landAuth","Clackamas"]
i <- 1:nrow(tmp)

image(tmp)
contour(i,i,tmp,xaxt = "n",yaxt = "n")
axis(1,seq(1,tail(i,1),length.out = 11),0:10)
axis(2,seq(1,tail(i,1),length.out = 11),0:10,las = 2)

###############
ls()
tmp <- twoway[,,"abund landAuth","Clackamas"]
i <- 1:nrow(tmp)
zxc <- dimnames(twoway)[[3]]
par(mfrow = c(1,1))

tmp <- twoway[,,2,"Clackamas"]
contour(i,i,tmp,xaxt = "n",yaxt = "n")
axis(1,seq(1,tail(i,1),length.out = 11),0:10)
axis(2,seq(1,tail(i,1),length.out = 11),0:10,las = 2)


tmp <- twoway[,,"abund landAuth","Clackamas"]
i <- 1:nrow(tmp)
for(j in 1:28){
  tmp <- twoway[,,j,"Clackamas"]
  contour(i,i,tmp,xaxt = "n",yaxt = "n")
  axis(1,seq(1,tail(i,1),length.out = 11),0:10)
  axis(2,seq(1,tail(i,1),length.out = 11),0:10,las = 2)
}
################# Response profile
# technicalities
col <- sprintf("grey%i",seq(100,0,-10)) # colors


#
asd <- lapply(1:28,function(x) apply(twoway[,,x,-c(13)],c(1,2),which.max))

tmp <- asd[[28]]
# tmp[,19:20] <- 20
# x and y labels
labs <- sapply(strsplit(dimnames(twoway)[[3]][28]," ")[[1]],function(x) names(nms[which(nms %in% x)]))
ii <- unique(as.vector(tmp))
ind <- lapply(ii,function(x) which(tmp == x,arr.ind = T))
par(mar = c(5.1, 4.1 + 2, 4.1, 2.1))
image(x = 1:nrow(tmp),1:nrow(tmp),z = tmp,col=col[seq_along(ii)],xlim = c(0,nrow(tmp)+1), ylim = c(0,nrow(tmp)+1),bty = "n",xaxt = "n",yaxt = "n",xlab = labs[1],ylab = labs[2])
invisible(lapply(1:length(ii), function(x) text(ind[[x]][,1],ind[[x]][,2],ii[x],cex = .7) ))
axis(2,seq(1,20,length.out = 6),seq(0,10,2),las = 2)
axis(1,seq(1,20,length.out = 6),seq(0,10,2))




# f <- function(mg,A=1,B=.04,C=3.785) ((A*mg*(B+C))/B)/1000
# f(50)
