
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
parm.values$lhd <-  rescale(parm.values$lhd,from = c(0,3),to = c(0,10)) # LIFE HISTORY DIVERSITY
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
  # Human
  Mcost  <-  10-((b["fc"] * x[,"fc"]) + (b["spc"] * x[,"spc"]))
  MCP    <-  (b["pla"] * x[,"landAuth"]) + (b["mc"] * Mcost)
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

cores <- Places[[2]]
nms <- colnames(PV)
names(nms) <- c("Political/Social\nCost","Financial\nCost","Threat\nScore","Abundance","Life History\nDiversity","Genetic\nDiversity","Persistance","% Land Authority")

ii <- 4 # variation parameter (don't exceed 5)
PVlow <- ifelse(PV - ii < 0,0,PV - ii)
PVhigh <- ifelse(PV + ii > 10,10,PV + ii)

f <- function(x){
PV[,x] <- PVlow[,x]
a <- model(PV)$`relative priority`
PV[,x] <- PVhigh[,x]
b <- model(PV)$`relative priority`
cbind(a,b)
}
tmp <- lapply(nms,f)
oneway <- array(unlist(tmp),dim =  c(length(cores),2,length(nms)),dimnames = list(cores,c("low","high"),nms))

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
TWsq[,,"lhd"]

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

save(oneway,twoway,model,mod.out,mod.out2,PV,b,nms,cores,rnk,LO,TWsq,SQ,file = "ORBTSmodelout.RData")
