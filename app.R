# load("C:/Users/brdavis.IFW/Documents/Projects/SDM/R codez/ORBTSmodelout.RData",envir = .GlobalEnv)
# load("ORBTS_app/ORBTSmodelout.RData",envir = .GlobalEnv)


# load objects
oneway <- readRDS("objects/oneway.rds")
twoway <- readRDS("objects/twoway.rds")
model <- readRDS("objects/model.rds")
mod.out <- readRDS("objects/mod.out.rds")
mod.out2 <- readRDS("objects/mod.out2.rds")
PV <- readRDS("objects/PV.rds")
b <- readRDS("objects/b.rds")
nms <- readRDS("objects/nms.rds")
cores <- readRDS("objects/cores.rds")
rnk <- readRDS("objects/rnk.rds")
LO <- readRDS("objects/LO.rds")
TWsq <- readRDS("objects/TWsq.rds")
TWsq2 <- readRDS("objects/TWsq2.rds")
SQ <- readRDS("objects/SQ.rds")

library(shiny)
library(shinythemes)

ui <- fluidPage(theme = shinytheme("slate"),
                titlePanel("ORBTS DSM"),
                navbarPage(NULL,
                           tabPanel(title = "Diagram",
                                    fluidRow(column(12,img(src="graph.png")))),
                           tabPanel(title = "Relative Priority",
                                    fluidRow(
                                        column(8,
                                               plotOutput("RankPlot")
                                        ),
                                        column(4,
                                               tableOutput("RankTable"))
                                    )
                           ),
# Tornado
                           tabPanel(title = "One-way Sensitivity Analysis - Tornado Plots",
                                    fluidRow(
                                        column(3,
                                               radioButtons("ca",
                                                            "Relative Priority Variability:",
                                                            choices = list("Clackamas" = 1,"Hood River" = 2,"Imnaha River" = 3,"Little Minam River" = 4,"Lookingglass/Wenaha" = 5,"Lower Deschutes River" = 6,"Middle Fork John Day River" = 7,"North Fork John Day River" = 8,"North Fork Malheur"  = 9,"North Santiam" = 10,"Odell Lake" = 11,"Pine/Indian/Wildhorse Creeks" = 12,"Powder River" = 13,"Sycan River" = 14,"Umatilla River" = 15,"Upper Grand Ronde" = 16,"Upper Klamath Lake" = 17,"Upper Mainstem John Day River" = 18,"Upper Malheur" = 19,"Upper Sprague River" = 20,"Upper Willamette River" = 21,"Walla Walla River" = 22,"Wallowa/Minam" = 23)[rnk],
                                                            selected = 8)
                                        ),
                                        column(6,
                                               plotOutput("OneWayPlot")
                                        ),
                                        column(3,
                                               checkboxGroupInput("CAest","Relative Priority Estimates:",
                                                                  choices = list("Clackamas" = 1,"Hood River" = 2,"Imnaha River" = 3,"Little Minam River" = 4,"Lookingglass/Wenaha" = 5,"Lower Deschutes River" = 6,"Middle Fork John Day River" = 7,"North Fork John Day River" = 8,"North Fork Malheur"  = 9,"North Santiam" = 10,"Odell Lake" = 11,"Pine/Indian/Wildhorse Creeks" = 12,"Powder River" = 13,"Sycan River" = 14,"Umatilla River" = 15,"Upper Grand Ronde" = 16,"Upper Klamath Lake" = 17,"Upper Mainstem John Day River" = 18,"Upper Malheur" = 19,"Upper Sprague River" = 20,"Upper Willamette River" = 21,"Walla Walla River" = 22,"Wallowa/Minam" = 23)[rnk])
                                        )
                                        
                                    )
                           ),
# Contour
                           tabPanel(title = "Two-way Sensitivity",
                                    tabsetPanel(
                                        tabPanel("Contour Plots",
                                                 fluidRow(
                                                     column(12,
                                                            radioButtons("Cont",
                                                                         "Core Area:",
                                                                         choices = list("Clackamas" = 1,"Hood River" = 2,"Imnaha River" = 3,"Little Minam River" = 4,"Lookingglass/Wenaha" = 5,"Lower Deschutes River" = 6,"Middle Fork John Day River" = 7,"North Fork John Day River" = 8,"North Fork Malheur"  = 9,"North Santiam" = 10,"Odell Lake" = 11,"Pine/Indian/Wildhorse Creeks" = 12,"Powder River" = 13,"Sycan River" = 14,"Umatilla River" = 15,"Upper Grand Ronde" = 16,"Upper Klamath Lake" = 17,"Upper Mainstem John Day River" = 18,"Upper Malheur" = 19,"Upper Sprague River" = 20,"Upper Willamette River" = 21,"Walla Walla River" = 22,"Wallowa/Minam" = 23)[rnk],
                                                                         selected = 7,inline = T)
                                                     )  
                                                 ),
                                                 fluidRow(h3(textOutput("Title"))),
                                                 fluidRow(column(6,br(uiOutput("Cplots"))),
                                                          column(6,br(uiOutput("Cplots2")))
                                                          )),
                                        tabPanel("Response Profile Plots",
                                                 fluidRow(
                                                     column(12,
                                                            checkboxGroupInput("RP","Uncheck to Remove from Analysis",
                                                                               choices = list("Clackamas (1)" = 1,"Hood River (2)" = 2,"Imnaha River (3)" = 3,"Little Minam River (4)" = 4,"Lookingglass/Wenaha (5)" = 5,"Lower Deschutes River (6)" = 6,"Middle Fork John Day River (7)" = 7,"North Fork John Day River (8)" = 8,"North Fork Malheur (9)"  = 9,"North Santiam (10)" = 10,"Odell Lake (11)" = 11,"Pine/Indian/Wildhorse Creeks (12)" = 12,"Powder River (13)" = 13,"Sycan River (14)" = 14,"Umatilla River (15)" = 15,"Upper Grand Ronde (16)" = 16,"Upper Klamath Lake (17)" = 17,"Upper Mainstem John Day River (18)" = 18,"Upper Malheur (19)" = 19,"Upper Sprague River (20)" = 20,"Upper Willamette River (21)" = 21,"Walla Walla River (22)" = 22,"Wallowa/Minam (23)" = 23),selected = 1:23,inline = T)
                                                     )
                                                 ),
                                                 fluidRow(column(6,uiOutput("RPplots")),
                                                          column(6,uiOutput("RPplots2"))
                                                          )

                                        )
                                    )
                           )
                )
)




# Define server logic required to draw a histogram
server <- function(input, output) {
    
    # Relative Priority
    
    output$RankPlot <- renderPlot({
        par(mar = c(4,16,1,1))
        a <- barplot(rev(mod.out2[[2]]),horiz = T,col = "grey50",border = F)
        axis(2,a,rev(mod.out2[[1]]),las = 2)
        title(xlab = "Relative Priority",line = 2.4)
        title(ylab = "Core Area",line = 14)
    },height = 600)
    
    output$RankTable <- renderTable({
        rownames(mod.out2) <- NULL
        mod.out2
    })
    
    output$RankPlot2 <- renderPlot({
        par(mar = c(4,16,1,1))
        a <- barplot(rev(mod.out2[[2]]),horiz = T,col = "grey50",border = F)
        axis(2,a,rev(mod.out2[[1]]),las = 2)
        title(xlab = "Relative Priority",line = 2.4)
        title(ylab = "Core Area",line = 14)
    },height = 600)
    
    output$RankTable <- renderTable({
        rownames(mod.out2) <- NULL
        mod.out2
    })
    
    # Tornados
    
    output$OneWayPlot <- renderPlot({
        tmp <- oneway[as.integer(input$ca),,]
        ii <- order(apply(tmp,2,function(x) abs(diff(x))))
        par(mar = c(5.1, 4.1 + 5, 4.1, 2.1))
        plot.new();plot.window(c(0,10),c(1,length(nms)))
        for(i in seq_along(nms)){
            xx <- range(tmp[,ii][,i])
            segments(x0 = xx[1],y0 = i,x1 = xx[2],y1 = i,lwd = 30,lend = 1)
        }
        abline(v = mod.out[as.integer(input$ca),2],lty = 2, col = 2)
        axis(2,at = seq_along(nms),names(nms[ii]),las = 2,col = "grey80")
        axis(1,seq(0,10,1),col = "grey80",col.axis ="grey40")
        title(main = dimnames(oneway)[[1]][as.integer(input$ca)],xlab = "Relative Priority")
        abline(v = mod.out[as.integer(input$CAest),2])
    },height = 600)
    
    # Contour Plots
    output$Title <- renderText(cores[as.integer(input$Cont)]) 
    ### This is the function to break the whole data into different blocks for each page
    plotInput <- reactive({
        h_plot <- 14
        half_data <- lapply(1:h_plot,function(x) twoway[,,x,as.integer(input$Cont)])
        CoreArea <- as.integer(input$Cont)
        return (list("h_plot" = h_plot,"half_data"=half_data,"CoreArea"=CoreArea))
    })
    
    plotInput2 <- reactive({
        h_plot <- 28
        n_plot <- 14
        half_data <- lapply(15:h_plot,function(x) twoway[,,x,as.integer(input$Cont)])
        CoreArea <- as.integer(input$Cont)
        return (list("h_plot" = h_plot,"half_data"=half_data,"n_plot" = n_plot,"CoreArea"=CoreArea))
    })
    
    ##### Create divs######
    output$Cplots <- renderUI({
        plot_output_list <- lapply(1:plotInput()$h_plot, function(x) {
            plotname <- paste("plot", x, sep="")
            plotOutput(outputId = plotname,height = 600)#,height = 500, width = 650
        })
        do.call(tagList, plot_output_list)
    })
    ##### Create divs######
    output$Cplots2 <- renderUI({
        plot_output_list <- lapply(15:plotInput2()$h_plot, function(x) {
            plotname <- paste("plot", x, sep="")
            plotOutput(outputId = plotname,height = 600)#,height = 500, width = 650
        })
        do.call(tagList, plot_output_list)
    })
    
    observe({
        lapply(1:plotInput()$h_plot, function(i){
            output[[paste("plot", i, sep="") ]] <- renderPlot({
                par(mar = c(5.1, 4.1 + 3, 4.1, 2.1))
                xy <- strsplit(dimnames(twoway)[[3]][i]," ")[[1]]
                xc <- which(dimnames(TWsq)[[3]] %in% xy[1])
                yc <- which(dimnames(TWsq)[[3]] %in% xy[2])
                contour(TWsq2[as.integer(input$Cont),,xc],TWsq2[as.integer(input$Cont),,yc],z = plotInput()$half_data[[i]])
                title(xlab = names(nms[which(nms %in% xy[1])]),ylab = names(nms[which(nms %in% xy[2])]))
                
            })
        })
    })
    observe({
        lapply(15:plotInput2()$h_plot, function(i){
            output[[paste("plot", i, sep="") ]] <- renderPlot({
                par(mar = c(5.1, 4.1 + 3, 4.1, 2.1))
                xy <- strsplit(dimnames(twoway)[[3]][i]," ")[[1]]
                xc <- which(dimnames(TWsq)[[3]] %in% xy[1])
                yc <- which(dimnames(TWsq)[[3]] %in% xy[2])
                contour(TWsq2[as.integer(input$Cont),,xc],TWsq2[as.integer(input$Cont),,yc],z = plotInput2()$half_data[[i-14]])
                title(xlab = names(nms[which(nms %in% xy[1])]),ylab = names(nms[which(nms %in% xy[2])]))
                
            })
        })
    })
    
    # Response Profiles
    
    # First Half
    RPInput <- reactive({
        h_plot <- 14
        cas <- as.integer(input$RP)
        cas <- ifelse(1:23 %in% cas,1:23,NA)
        half_data <- lapply(1:h_plot,function(x) apply(twoway[,,x,cas],c(1,2),which.max))
        return (list("h_plot" = h_plot,"half_data"=half_data))
    })

    ##### Create divs######
    output$RPplots <- renderUI({
        plot_output_list <- lapply(1:RPInput()$h_plot, function(x) {
            plotname <- paste("RP", x, sep="")
            plotOutput(outputId = plotname,height = 600)
        })
        do.call(tagList, plot_output_list)
    })

    observe({
        lapply(1:RPInput()$h_plot, function(b){
            output[[paste("RP", b, sep="") ]] <- renderPlot({
                col <- grey.colors(n=36)[14:36] # colors
                tmp <- RPInput()$half_data[[b]]
                labs <- sapply(strsplit(dimnames(twoway)[[3]][b]," ")[[1]],function(x) names(nms[which(nms %in% x)]))
                ii <- unique(as.vector(tmp))
                ind <- lapply(ii,function(x) which(tmp == x,arr.ind = T))
                par(mar = c(5.1, 4.1 + 2, 4.1, 2.1))
                image(x = 1:nrow(tmp),1:nrow(tmp),z = tmp,col=col,xlim = c(0,nrow(tmp)+1), ylim = c(0,nrow(tmp)+1),bty = "n",xaxt = "n",yaxt = "n",xlab = labs[1],ylab = labs[2])
                invisible(lapply(1:length(ii), function(x) text(ind[[x]][,1],ind[[x]][,2],ii[x],cex = .7) ))
                axis(2,seq(1,nrow(tmp),length.out = 10),1:10,las = 2)
                axis(1,seq(1,nrow(tmp),length.out = 10),1:10)

            })
        })
    })


# Other Half
RPInput2 <- reactive({
    h_plot <- 28
    cas <- as.integer(input$RP)
    cas <- ifelse(1:23 %in% cas,1:23,NA)
    half_data <- lapply(15:h_plot,function(x) apply(twoway[,,x,cas],c(1,2),which.max))
    return (list("h_plot" = h_plot,"half_data"=half_data))
})

##### Create divs######
output$RPplots2 <- renderUI({
    plot_output_list <- lapply(15:RPInput2()$h_plot, function(x) {
        plotname <- paste("RP", x, sep="")
        plotOutput(outputId = plotname,height = 600)
    })
    do.call(tagList, plot_output_list)
})

observe({
    lapply(15:RPInput2()$h_plot, function(b){
        output[[paste("RP", b, sep="") ]] <- renderPlot({
            col <- grey.colors(n=36)[14:36] # colors
            tmp <- RPInput2()$half_data[[b-14]]
            labs <- sapply(strsplit(dimnames(twoway)[[3]][b]," ")[[1]],function(x) names(nms[which(nms %in% x)]))
            ii <- unique(as.vector(tmp))
            ind <- lapply(ii,function(x) which(tmp == x,arr.ind = T))
            par(mar = c(5.1, 4.1 + 2, 4.1, 2.1))
            image(x = 1:nrow(tmp),1:nrow(tmp),z = tmp,col=col,xlim = c(0,nrow(tmp)+1), ylim = c(0,nrow(tmp)+1),bty = "n",xaxt = "n",yaxt = "n",xlab = labs[1],ylab = labs[2])
            invisible(lapply(1:length(ii), function(x) text(ind[[x]][,1],ind[[x]][,2],ii[x],cex = .7) ))
            axis(2,seq(1,nrow(tmp),length.out = 10),1:10,las = 2)
            axis(1,seq(1,nrow(tmp),length.out = 10),1:10)
            
        })
    })
})


}


# Run the application 
shinyApp(ui = ui, server = server)
