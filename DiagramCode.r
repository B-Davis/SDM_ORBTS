library(DiagrammeR)
graph <- grViz("
digraph boxes_and_circles {
      
      # a 'graph' statement
      graph [overlap = true, fontsize = 10]
      
      # several 'node' statements
      node [shape = box,
      fontname = Helvetica]
      A[label = '% Private Land']; 
      B[label = '% Public Land'];
      //E[label = 'Working Group Constraints']
      K[label = 'Private Landowner Cooperation']

      node [shape = box,
      fontname = Helvetica] //,color = blue,style = filled
      F[label = 'Threat Score'];
      G[label = 'Persistance']; 
      H[label = 'Genetic Diversity']; 
      I[label = 'Life History Diversity'];
      C[label = 'Financial Cost']; 
      D[label = 'Political/Social Cost'];
      J[label = 'Abundance']
      
      node [shape = oval] // sets as circles
      1[label = '% Land Authority'];
      2[label = 'Management Cost'];

      node [shape = oval] // sets as circles
      3[label = 'Management Capacity\nto Adress Threats'];
      4[label = 'Conservation Risk'];
      5[label = 'Vulnerability']; 
      6[label = 'Adaptive Capacity']
      
      node [shape = diamond]
      Z[label = 'Relative Priority']
      
      # several 'edge' statements
      A->1 B->1 C->2 D->2 1->3 2->3 3->Z
      4->Z 5->4 F->4 G->5 6->5 H->6 I->6 J->6
      K->A
      # 1->D E->A 2->4 1->5 1->F
      # E->6 4->6 5->7 6->7 3->8
      }
      ")
graph
#############
### Donut ###
#############
require(scales)
source("funtions.r")
parm.values <- read.csv("parm values.csv",stringsAsFactors = F)[,-c(1,5)]
parm.values$pers <- rep(5,22)
parm.values$ts <- rescale(parm.values$ts,c(0,10),from = c(.2,3.2)) ## Rescale threatscore to between 0-10 - ts range = c(.2,3.2)
parm.wts <- read.csv("parm weights.csv")
b <- parm.wts$weight
names(b) <- parm.wts$parm
pv <- parm.values

X11()
donut(b)


b <- c(.3,.7,.2,.8,.5,.5,.1,.6,.3,.6,.4,.7,.3)
names(b) <- parm.wts$parm

ab <- beta.mom(.7,.1)
curve(dbeta(x,ab[1],ab[2]),xaxt = "n",xlab = "Value",ylab = "Density");axis(1,seq(0,1,.1),0:10)



