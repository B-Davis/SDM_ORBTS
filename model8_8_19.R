
rm(list = ls())
source("functions.r")
library(scales)
library(readxl)
############
### Data ###
############  
# # # # # # # # Parameters

parm.values <- as.data.frame(read_excel("../ORBTS Proritization Parameters 6.18.2019.xlsx",sheet = "Parm Values",range = "A2:P25"))
# massage parameters
pers <- as.data.frame(read_excel("../ORBTS Proritization Parameters 6.18.2019.xlsx",sheet = "weighted BTVA ex",range = "E1:E24"))[[1]]/(-10) # Add Persistance!!
parm.values$ts <- rescale(parm.values$ts,to = c(0,10),from = c(.2,3.2))
# Add needed parameters
parm.values$gd <- parm.values$gd*10 # rescale gd
pers <- ifelse(pers < 0,0,pers) # should I be doing this? Yes I should
parm.values$pers <- pers
head(parm.values)
parm.values$landAuth <- with(parm.values,((cooperation/100*pri)+pub)/10)
# parm.values$landAuth <- with(parm.values,((cooperation/100*Private)+`Federal/State/Local/tribal`)*10)
# parm.values$lhd <-  rescale(parm.values$lhd,from = c(0,3),to = c(0,10)) # LIFE HISTORY DIVERSITY
tmp <- parm.values$lhd + 4
parm.values$lhd <- ifelse(tmp == 7,10,tmp)
parm.values$lhd[10] <- 0
# omit unneeded parms
head(parm.values)
Places <- parm.values[,1:2]
parm.values <- parm.values[,-c(1:4,5:7,14:16)]
names(parm.values)
###### Make sure relationships are direct, not inverse ###########
# Management cost and Adaptive Capacity are accounted for in the model
# "pers" is confusing to think about...should be called vulnerabilityn, not persistance...it is related to priority correctly
rm(pers)

# # # # # # # # Weights

i <- c(1,4,7,11,14,17) # simply ommiting unneeded rows from Excel worksheet
parm.weights <- as.data.frame(read_excel("../ORBTS Proritization Parameters 6.18.2019.xlsx",sheet = "Parm weights",range = "A7:B26"))[-i,]
parm.weights[,2] <- as.numeric(parm.weights[,2])
rownames(parm.weights) <- NULL
colnames(parm.weights) <- c("name","wt")
# model parameters
PV <- as.matrix(parm.values)
rownames(PV) <- Places$core
b <- setNames(parm.weights[,2], c("spc","fc","pers","adcap","lhd","gd","abund","pla","mc","vuln","threat","cr","mctat"))
rm(i)

#############
### Model ###
############# don't forget to invert based on relationship
model <- function(x) {
  # Biological
  adapCap <- 10 -((b["lhd"] * x[,"lhd"]) + (b["gd"] * x[,"gd"]) + (b["abund"] * x[,"abund"]))
  vul     <- (b["pers"] * x[,"pers"]) + (b["adcap"] * adapCap)
  CR      <- (b["threat"] * x[,"ts"]) + (b["vuln"] * vul)
   # transform CR based on alpha and beta parameter averaged from stakeholder priors
  CR <- dbeta(CR/10,5.814,5.126)/2.5866*10 # a = 5.814, b = 5.126 # 2.5866 = max(dbeta(seq(0,1,.001),5.814,5.126))*10
  # Human
  Mcost  <-  10-((b["fc"] * x[,"fc"]) + (b["spc"] * x[,"spc"]))
  MCP    <-  (b["pla"] * x[,"landAuth"]) + (b["mc"] * Mcost)
  #int <- (.25+.1)/5, sl <- (.74+.89+1+1+1)/5
  MCP <- .07 + (MCP*.926)
  # Relative Priority
  list("relative priority" = (b["mctat"] * MCP) + (b["cr"] * CR),"management capacity tats" = MCP,"conservation risk" = CR)
  # (b["mctat"] * MCP) + (b["cr"] * CR)
}

##############
### Output ###
##############
mod.out <- round(model(PV)[["relative priority"]],3)
mod.out <- data.frame("Core Area" = Places[[2]],"Relative Priority" = mod.out)
rnk <- order(mod.out[[2]],decreasing = T)
mod.out2 <- mod.out[rnk,]
rownames(mod.out) <- NULL
mod.out2

###################
### 1 Way Sensy ###
###################
# PV <- as.matrix(parm.values)
# rownames(PV) <- Places$core


cores <- Places[[2]]
nms <- colnames(PV)
names(nms) <- c("Political/Social\nCost","Financial\nCost","Threat\nScore","Abundance","Life History\nDiversity","Genetic\nDiversity","Persistance","% Land Authority")

ii <- 4 # variation parameter (don't exceed 5)
PVlow <- ifelse(PV - ii < 0,0,PV - ii)
PVhigh <- ifelse(PV + ii > 10,10,PV + ii) # Reconvert these high and lows back to scale so we can see how these parameters are varying
n <- 100
mm <- t(mapply(seq,from = PVlow,to = PVhigh,length.out = n))
mm <- lapply(split(1:184,rep(1:8,each = 23)),function(x) mm[x,])
mm <- array(unlist(mm),c(length(cores),n,length(nms)),dimnames = list(cores,1:n,nms))

f <- function(pms,n){
PV[,pms] <- mm[,n,pms]
model(PV)$`relative priority`
}
tmp <- sapply(nms,function(x) sapply(1:n,f,pms = x),simplify = "array")
low <- apply(tmp,c(1,3),min)
high <- apply(tmp,c(1,3),max)

oneway <- sapply(names(nms),function(x) cbind(low = low[,x],high = high[,x]),simplify = "array")


###################
### 2-way sensy ###
###################

# # # # # # # # dimensions: 1 rows, 2 = col (1 and 2 parm values),3 = parms (n = 2), 4 = core areas.

# set up indices
nmi <- t(combn(nms,2)) # x and y parms resectively
pnmi <- paste(nmi[,1],nmi[,2])
LO <- 10 # length out parameter

# Sequence array part 1
TWsq <- array(dim = c(length(cores),LO,length(nms)),dimnames = list(cores,1:LO,nms))
for(i in seq_along(nms)){
  TWsq[,,i] <- round(do.call(rbind,Map(seq,PVlow[,i],PVhigh[,i],length.out = LO)),3)
}
# TWsq[,,"lhd"]

# TWsq2 same,but rescaled back to original parameter scale for plotting purposes
TWsq2 <- array(dim = dim(TWsq),dimnames = dimnames(TWsq))
TWsq2[,,7:8] <- TWsq[,,7:8]*10 # % Land Authority & Persistance to %
TWsq2[,,6] <- TWsq[,,6]/10 # Genetic Div. scaled back to 0-1
TWsq2[,,3] <- rescale(TWsq[,,3],from = c(0,10),to = c(.2,3.2)) # Threat score
TWsq2[,,c(1:2,4)] <- TWsq[,,c(1:2,4)]
TWsq2[,,5] <- TWsq[,,5]

# Sequence array part 2
SQ <- array(dim = c(LO**2,2,nrow(nmi),length(cores)),dimnames = list(NULL,NULL,pnmi,cores))
for(z in seq_along(cores)){
for(i in 1:nrow(nmi)){ # populate parameter sq...expand grid for parameter variance
  SQ[,,i,z] <- as.matrix(expand.grid(TWsq[z,,nmi[i,]][,1],TWsq[z,,nmi[i,]][,2]))
}
}

g <- function(sq,prm,ca){
PV[ca,nmi[prm,]] <- SQ[,,pnmi[prm],ca][sq,]
model(PV)$`relative priority`[ca]
}
# g(1,1,1)
# empty 4d array
twoway <- array(dim = c(LO,LO,nrow(nmi),nrow(PV)),dimnames =  list(1:LO, 1:LO, pnmi,Places$core))
# build it
for(q in seq_along(cores)){
for(z in seq_along(pnmi)){
twoway[,,z,q] <- sapply(1:LO**2,g,prm = z,ca = q)
}
}

###########
save(oneway,twoway,model,mod.out,mod.out2,PV,b,nms,cores,rnk,LO,TWsq,SQ,file = "ORBTSmodelout.RData")
saveRDS(oneway,file = "ORBTS_app/objects/oneway.rds")
saveRDS(twoway,file = "ORBTS_app/objects/twoway.rds")
saveRDS(model,file = "ORBTS_app/objects/model.rds")
saveRDS(mod.out,file = "ORBTS_app/objects/mod.out.rds")
saveRDS(mod.out2,file = "ORBTS_app/objects/mod.out2.rds")
saveRDS(PV,file = "ORBTS_app/objects/PV.rds")
saveRDS(b,file = "ORBTS_app/objects/b.rds")
saveRDS(nms,file = "ORBTS_app/objects/nms.rds")
saveRDS(cores,file = "ORBTS_app/objects/cores.rds")
saveRDS(rnk,file = "ORBTS_app/objects/rnk.rds")
saveRDS(LO,file = "ORBTS_app/objects/LO.rds")
saveRDS(TWsq,file = "ORBTS_app/objects/TWsq.rds")
saveRDS(TWsq2,file = "ORBTS_app/objects/TWsq2.rds")
saveRDS(SQ,file = "ORBTS_app/objects/SQ.rds")
