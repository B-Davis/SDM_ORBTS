load("C:/Users/brdavis.IFW/Documents/Projects/SDM/R codez/ORBTSmodelout.RData",envir = .GlobalEnv)

library(shiny)
library(shinythemes)

ui <- fluidPage(theme = shinytheme("slate"),
                titlePanel("ORBTS DSM"),
                navbarPage(NULL,
                           tabPanel(title = "Relative Priority",
                                    fluidRow(
                                        column(8,
                                               plotOutput("RankPlot")
                                        ),
                                        column(4,
                                               tableOutput("RankTable"))
                                    )
                           ),
                           tabPanel(title = "One-way Sensitivity Analysis - Tornado Plots",
                                    fixedRow(
                                        column(3,
                                               radioButtons("ca",
                                                            "Relative Priority Variability:",
                                                            choices = list("Clackamas" = 1,"Hood River" = 2,"Imnaha River" = 3,"Little Minam River" = 4,"Lookingglass/Wenaha" = 5,"Lower Deschutes River" = 6,"Middle Fork John Day River" = 7,"North Fork John Day River" = 8,"North Fork Malheur"  = 9,"North Santiam" = 10,"Odell Lake" = 11,"Pine/Indian/Wildhorse Creeks" = 12,"Powder River" = 13,"Sycan River" = 14,"Umatilla River" = 15,"Upper Grand Ronde" = 16,"Upper Klamath Lake" = 17,"Upper Mainstem John Day River" = 18,"Upper Malheur" = 19,"Upper Sprague River" = 20,"Upper Willamette River" = 21,"Walla Walla River" = 22,"Wallowa/Minam" = 23)[rnk],
                                                            selected = 13)
                                        ),
                                        column(6,
                                               plotOutput("OneWayPlot")
                                        ),
                                        column(3,
                                               checkboxGroupInput("CAest","Relative Priority Estimates:",
                                                                  choices = list("Clackamas" = 1,"Hood River" = 2,"Imnaha River" = 3,"Little Minam River" = 4,"Lookingglass/Wenaha" = 5,"Lower Deschutes River" = 6,"Middle Fork John Day River" = 7,"North Fork John Day River" = 8,"North Fork Malheur"  = 9,"North Santiam" = 10,"Odell Lake" = 11,"Pine/Indian/Wildhorse Creeks" = 12,"Powder River" = 13,"Sycan River" = 14,"Umatilla River" = 15,"Upper Grand Ronde" = 16,"Upper Klamath Lake" = 17,"Upper Mainstem John Day River" = 18,"Upper Malheur" = 19,"Upper Sprague River" = 20,"Upper Willamette River" = 21,"Walla Walla River" = 22,"Wallowa/Minam" = 23)[rnk],)
                                        )
                                        
                                    )
                           ),
                           tabPanel(title = "Two-way Sensitivity",
                                    tabsetPanel(
                                        tabPanel("Contour Plots",
                                                 fluidRow(
                                                     column(12,
                                                            radioButtons("Cont",
                                                                         "Core Area:",
                                                                         choices = list("Clackamas" = 1,"Hood River" = 2,"Imnaha River" = 3,"Little Minam River" = 4,"Lookingglass/Wenaha" = 5,"Lower Deschutes River" = 6,"Middle Fork John Day River" = 7,"North Fork John Day River" = 8,"North Fork Malheur"  = 9,"North Santiam" = 10,"Odell Lake" = 11,"Pine/Indian/Wildhorse Creeks" = 12,"Powder River" = 13,"Sycan River" = 14,"Umatilla River" = 15,"Upper Grand Ronde" = 16,"Upper Klamath Lake" = 17,"Upper Mainstem John Day River" = 18,"Upper Malheur" = 19,"Upper Sprague River" = 20,"Upper Willamette River" = 21,"Walla Walla River" = 22,"Wallowa/Minam" = 23)[rnk],
                                                                         selected = 13,inline = T)
                                                     )  
                                                 ),
                                                 fluidRow(column(6,
                                                                 uiOutput("Cplots"))
                                        )),
                                        tabPanel("Response Profile Plots",
                                                 fluidRow(
                                                     column(12,
                                                            checkboxGroupInput("RP","Uncheck to Remove from Analysis",
                                                                               choices = list("Clackamas (1)" = 1,"Hood River (2)" = 2,"Imnaha River (3)" = 3,"Little Minam River (4)" = 4,"Lookingglass/Wenaha (5)" = 5,"Lower Deschutes River (6)" = 6,"Middle Fork John Day River (7)" = 7,"North Fork John Day River (8)" = 8,"North Fork Malheur (9)"  = 9,"North Santiam (10)" = 10,"Odell Lake (11)" = 11,"Pine/Indian/Wildhorse Creeks (12)" = 12,"Powder River (13)" = 13,"Sycan River (14)" = 14,"Umatilla River (15)" = 15,"Upper Grand Ronde (16)" = 16,"Upper Klamath Lake (17)" = 17,"Upper Mainstem John Day River (18)" = 18,"Upper Malheur (19)" = 19,"Upper Sprague River (20)" = 20,"Upper Willamette River (21)" = 21,"Walla Walla River (22)" = 22,"Wallowa/Minam (23)" = 23),selected = 1:23,inline = T)
                                                     )
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
    
    # Tornados
    
    output$OneWayPlot <- renderPlot({
        tmp <- oneway[as.integer(input$ca),,]
        ii <- order(apply(tmp,2,function(x) abs(diff(x))))
        par(mar = c(5.1, 4.1 + 5, 4.1, 2.1))
        plot.new();plot.window(c(0,10),c(1,length(nms)))
        for(i in seq_along(nms)){
            xx <- range(tmp[,ii][,i])
            if(i == 1) abline(v = model(PV)$`relative priority`[as.integer(input$ca)],lty = 2, col = 2)
            segments(x0 = xx[1],y0 = i,x1 = xx[2],y1 = i,lwd = 30,lend = 1)
        }
        axis(2,at = seq_along(nms),names(nms[ii]),las = 2,col = "grey80")
        axis(1,seq(0,10,1),col = "grey80",col.axis ="grey40")
        title(main = dimnames(oneway)[[1]][as.integer(input$ca)],xlab = "Relative Priority")
        abline(v = mod.out[input$CAest,2])
    },height = 600)
    
    # Contour Plots
    
    ### This is the function to break the whole data into different blocks for each page
    plotInput <- reactive({
        n_plot <- 28
        h_plot <- 14
        total_data <- lapply(1:n_plot,function(x) twoway[,,x,1])
        half_data <- lapply(1:h_plot,function(x) twoway[,,x,1])
        return (list("n_plot"=n_plot, "h_plot" = h_plot,"total_data"=total_data,"half_data"=half_data))
    })
    
    ##### Create divs######
    output$Cplots <- renderUI({
        plot_output_list <- lapply(1:plotInput()$h_plot, function(x) {
            plotname <- paste("plot", x, sep="")
            plotOutput(outputId = plotname,height = 280, width = 500)
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
                contour(TWsq[1,,xc],TWsq[1,,yc],z = plotInput()$half_data[[i]], main = paste("Plot", i))
                title(xlab = names(nms[which(nms %in% xy[1])]),ylab = names(nms[which(nms %in% xy[2])]))
                
            })
        })
    })
    

}

# Run the application 
shinyApp(ui = ui, server = server)

#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
# load("C:/Users/brdavis.IFW/Documents/Projects/SDM/R codez/ORBTSmodelout.RData",envir = .GlobalEnv)
# 
# library(shiny)
# library(shinythemes)
# 
# ui <- fluidPage(theme = shinytheme("slate"),
#     titlePanel("ORBTS DSM"),
#     navbarPage(NULL,
#     tabPanel(title = "Relative Priority",
#              fluidRow(
#                  column(8,
#                         plotOutput("RankPlot")
#                         ),
#                  column(4,
#                         tableOutput("RankTable"))
#              )
#     ),
#         tabPanel(title = "One-way Sensitivity Analysis - Tornado Plots",
#     fixedRow(
#         column(3,
#             radioButtons("ca",
#                         "Relative Priority Variability:",
#                         choices = list("Clackamas" = 1,"Hood River" = 2,"Imnaha River" = 3,"Little Minam River" = 4,"Lookingglass/Wenaha" = 5,"Lower Deschutes River" = 6,"Middle Fork John Day River" = 7,"North Fork John Day River" = 8,"North Fork Malheur"  = 9,"North Santiam" = 10,"Odell Lake" = 11,"Pine/Indian/Wildhorse Creeks" = 12,"Powder River" = 13,"Sycan River" = 14,"Umatilla River" = 15,"Upper Grand Ronde" = 16,"Upper Klamath Lake" = 17,"Upper Mainstem John Day River" = 18,"Upper Malheur" = 19,"Upper Sprague River" = 20,"Upper Willamette River" = 21,"Walla Walla River" = 22,"Wallowa/Minam" = 23)[rnk],
#                         selected = 13)
#         ),
#         column(6,
#            plotOutput("OneWayPlot")
#         ),
#         column(3,
#             checkboxGroupInput("CAest","Relative Priority Estimates:",
#                                choices = list("Clackamas" = 1,"Hood River" = 2,"Imnaha River" = 3,"Little Minam River" = 4,"Lookingglass/Wenaha" = 5,"Lower Deschutes River" = 6,"Middle Fork John Day River" = 7,"North Fork John Day River" = 8,"North Fork Malheur"  = 9,"North Santiam" = 10,"Odell Lake" = 11,"Pine/Indian/Wildhorse Creeks" = 12,"Powder River" = 13,"Sycan River" = 14,"Umatilla River" = 15,"Upper Grand Ronde" = 16,"Upper Klamath Lake" = 17,"Upper Mainstem John Day River" = 18,"Upper Malheur" = 19,"Upper Sprague River" = 20,"Upper Willamette River" = 21,"Walla Walla River" = 22,"Wallowa/Minam" = 23)[rnk],)
#                                     )
#     
#         )
#     ),
#     tabPanel(title = "Two-way Sensitivity",
#              tabsetPanel(
#                  tabPanel("Contour Plots",
#                           fluidRow(
#                             column(12,
#                                    radioButtons("Cont",
#                                                 "Core Area:",
#                                                 choices = list("Clackamas" = 1,"Hood River" = 2,"Imnaha River" = 3,"Little Minam River" = 4,"Lookingglass/Wenaha" = 5,"Lower Deschutes River" = 6,"Middle Fork John Day River" = 7,"North Fork John Day River" = 8,"North Fork Malheur"  = 9,"North Santiam" = 10,"Odell Lake" = 11,"Pine/Indian/Wildhorse Creeks" = 12,"Powder River" = 13,"Sycan River" = 14,"Umatilla River" = 15,"Upper Grand Ronde" = 16,"Upper Klamath Lake" = 17,"Upper Mainstem John Day River" = 18,"Upper Malheur" = 19,"Upper Sprague River" = 20,"Upper Willamette River" = 21,"Walla Walla River" = 22,"Wallowa/Minam" = 23)[rnk],
#                                                 selected = 13,inline = T)
#                                    )  
#                           ),
#                           fluidRow(column(6,
#                                           plotOutput("Cplots")),
#                                    column(6,
#                                           plotOutput("Cplots2"))),
#                           fluidRow(column(6,
#                                           plotOutput("Cplots3")),
#                                    column(6,
#                                           plotOutput("Cplots4"))),
#                           fluidRow(column(6,
#                                           plotOutput("Cplots5")),
#                                    column(6,
#                                           plotOutput("Cplots6"))),
#                           fluidRow(column(6,
#                                           plotOutput("Cplots7")),
#                                    column(6,
#                                           plotOutput("Cplots8"))),
#                           fluidRow(column(6,
#                                           plotOutput("Cplots9")),
#                                    column(6,
#                                           plotOutput("Cplots10"))),
#                           fluidRow(column(6,
#                                           plotOutput("Cplots11")),
#                                    column(6,
#                                           plotOutput("Cplots12"))),
#                           fluidRow(column(6,
#                                           plotOutput("Cplots13")),
#                                    column(6,
#                                           plotOutput("Cplots14"))),
#                           fluidRow(column(6,
#                                           plotOutput("Cplots15")),
#                                    column(6,
#                                           plotOutput("Cplots16"))),
#                           fluidRow(column(6,
#                                           plotOutput("Cplots17")),
#                                    column(6,
#                                           plotOutput("Cplots18"))),
#                           fluidRow(column(6,
#                                           plotOutput("Cplots19")),
#                                    column(6,
#                                           plotOutput("Cplots20"))),
#                           fluidRow(column(6,
#                                           plotOutput("Cplots21")),
#                                    column(6,
#                                           plotOutput("Cplots22"))),
#                           fluidRow(column(6,
#                                           plotOutput("Cplots23")),
#                                    column(6,
#                                           plotOutput("Cplots24"))),
#                           fluidRow(column(6,
#                                           plotOutput("Cplots25")),
#                                    column(6,
#                                           plotOutput("Cplots26"))),
#                           fluidRow(column(6,
#                                           plotOutput("Cplots27")),
#                                    column(6,
#                                           plotOutput("Cplots28")))
#                  ), 
#                  tabPanel("Response Profile Plots",
#                           fluidRow(
#                               column(12,
#                                      checkboxGroupInput("RP","Uncheck to Remove from Analysis",
#                                                         choices = list("Clackamas (1)" = 1,"Hood River (2)" = 2,"Imnaha River (3)" = 3,"Little Minam River (4)" = 4,"Lookingglass/Wenaha (5)" = 5,"Lower Deschutes River (6)" = 6,"Middle Fork John Day River (7)" = 7,"North Fork John Day River (8)" = 8,"North Fork Malheur (9)"  = 9,"North Santiam (10)" = 10,"Odell Lake (11)" = 11,"Pine/Indian/Wildhorse Creeks (12)" = 12,"Powder River (13)" = 13,"Sycan River (14)" = 14,"Umatilla River (15)" = 15,"Upper Grand Ronde (16)" = 16,"Upper Klamath Lake (17)" = 17,"Upper Mainstem John Day River (18)" = 18,"Upper Malheur (19)" = 19,"Upper Sprague River (20)" = 20,"Upper Willamette River (21)" = 21,"Walla Walla River (22)" = 22,"Wallowa/Minam (23)" = 23),selected = 1:23,inline = T)
#                               )
#                                      ),
#                           fluidRow(column(6,
#                                           plotOutput("RPplots1")),
#                                    column(6,
#                                           plotOutput("RPplots2"))),
#                           fluidRow(column(6,
#                                           plotOutput("RPplots3")),
#                                    column(6,
#                                           plotOutput("RPplots4"))),
#                           fluidRow(column(6,
#                                           plotOutput("RPplots5")),
#                                    column(6,
#                                           plotOutput("RPplots6"))),
#                           fluidRow(column(6,
#                                           plotOutput("RPplots7")),
#                                    column(6,
#                                           plotOutput("RPplots8"))),
#                           fluidRow(column(6,
#                                           plotOutput("RPplots9")),
#                                    column(6,
#                                           plotOutput("RPplots10"))),
#                           fluidRow(column(6,
#                                           plotOutput("RPplots11")),
#                                    column(6,
#                                           plotOutput("RPplots12"))),
#                           fluidRow(column(6,
#                                           plotOutput("RPplots13")),
#                                    column(6,
#                                           plotOutput("RPplots14"))),
#                           fluidRow(column(6,
#                                           plotOutput("RPplots15")),
#                                    column(6,
#                                           plotOutput("RPplots16"))),
#                           fluidRow(column(6,
#                                           plotOutput("RPplots17")),
#                                    column(6,
#                                           plotOutput("RPplots18"))),
#                           fluidRow(column(6,
#                                           plotOutput("RPplots19")),
#                                    column(6,
#                                           plotOutput("RPplots20"))),
#                           fluidRow(column(6,
#                                           plotOutput("RPplots21")),
#                                    column(6,
#                                           plotOutput("RPplots22"))),
#                           fluidRow(column(6,
#                                           plotOutput("RPplots23")),
#                                    column(6,
#                                           plotOutput("RPplots24"))),
#                           fluidRow(column(6,
#                                           plotOutput("RPplots25")),
#                                    column(6,
#                                           plotOutput("RPplots26"))),
#                           fluidRow(column(6,
#                                           plotOutput("RPplots27")),
#                                    column(6,
#                                           plotOutput("RPplots28")))
#                           )
#              )
# )
# )
# )
# 
# 
# 
# 
# # Define server logic required to draw a histogram
# server <- function(input, output) {
#     
#     # Relative Priority
#     
#     output$RankPlot <- renderPlot({
#         par(mar = c(4,16,1,1))
#         a <- barplot(rev(mod.out2[[2]]),horiz = T,col = "grey50",border = F)
#         axis(2,a,rev(mod.out2[[1]]),las = 2)
#         title(xlab = "Relative Priority",line = 2.4)
#         title(ylab = "Core Area",line = 14)
#     },height = 600)
#     
#     output$RankTable <- renderTable({
#         rownames(mod.out2) <- NULL
#         mod.out2
#         })
#     
#     # Tornados
#     
#     output$OneWayPlot <- renderPlot({
#         tmp <- oneway[as.integer(input$ca),,]
#         ii <- order(apply(tmp,2,function(x) abs(diff(x))))
#         par(mar = c(5.1, 4.1 + 5, 4.1, 2.1))
#         plot.new();plot.window(c(0,10),c(1,length(nms)))
#         for(i in seq_along(nms)){
#             xx <- range(tmp[,ii][,i])
#             if(i == 1) abline(v = model(PV)$`relative priority`[as.integer(input$ca)],lty = 2, col = 2)
#             segments(x0 = xx[1],y0 = i,x1 = xx[2],y1 = i,lwd = 30,lend = 1)
#         }
#         axis(2,at = seq_along(nms),names(nms[ii]),las = 2,col = "grey80")
#         axis(1,seq(0,10,1),col = "grey80",col.axis ="grey40")
#         title(main = dimnames(oneway)[[1]][as.integer(input$ca)],xlab = "Relative Priority")
#         abline(v = mod.out[input$CAest,2])
#     },height = 600)
#     
#     # Contour Plots
#     
#     output$Cplots <- renderPlot({
#         b <- 1
#         xy <- strsplit(dimnames(twoway)[[3]][b]," ")[[1]]
#         tmp <- twoway[,,b,as.integer(input$Cont)]
#         par(mar = c(5.1, 4.1 + 3, 4.1, 2.1))
#         contour(1:LO,1:LO,tmp,xaxt = "n",yaxt = "n")
#         axis(1,seq(1,LO,length.out = 11),0:10)
#         axis(2,seq(1,LO,length.out = 11),0:10,las = 2)
#         title(xlab = names(nms[which(nms %in% xy[1])]),ylab = names(nms[which(nms %in% xy[2])]),main = cores[as.integer(input$Cont)])
#         })
#     
#     output$Cplots2 <- renderPlot({
#         b <- 2
#         xy <- strsplit(dimnames(twoway)[[3]][b]," ")[[1]]
#         tmp <- twoway[,,b,as.integer(input$Cont)]
#         par(mar = c(5.1, 4.1 + 3, 4.1, 2.1))
#         contour(1:LO,1:LO,tmp,xaxt = "n",yaxt = "n")
#         axis(1,seq(1,LO,length.out = 11),0:10)
#         axis(2,seq(1,LO,length.out = 11),0:10,las = 2)
#         title(xlab = names(nms[which(nms %in% xy[1])]),ylab = names(nms[which(nms %in% xy[2])]),main = cores[as.integer(input$Cont)])
#     })
#     
#     output$Cplots3 <- renderPlot({
#         b <- 3
#         xy <- strsplit(dimnames(twoway)[[3]][b]," ")[[1]]
#         tmp <- twoway[,,b,as.integer(input$Cont)]
#         par(mar = c(5.1, 4.1 + 3, 4.1, 2.1))
#         contour(1:LO,1:LO,tmp,xaxt = "n",yaxt = "n")
#         axis(1,seq(1,LO,length.out = 11),0:10)
#         axis(2,seq(1,LO,length.out = 11),0:10,las = 2)
#         title(xlab = names(nms[which(nms %in% xy[1])]),ylab = names(nms[which(nms %in% xy[2])]))
#         })
#     
#     output$Cplots4 <- renderPlot({
#         b <- 4
#         xy <- strsplit(dimnames(twoway)[[3]][b]," ")[[1]]
#         tmp <- twoway[,,b,as.integer(input$Cont)]
#         par(mar = c(5.1, 4.1 + 3, 4.1, 2.1))
#         contour(1:LO,1:LO,tmp,xaxt = "n",yaxt = "n")
#         axis(1,seq(1,LO,length.out = 11),0:10)
#         axis(2,seq(1,LO,length.out = 11),0:10,las = 2)
#         title(xlab = names(nms[which(nms %in% xy[1])]),ylab = names(nms[which(nms %in% xy[2])]))
#     })
#     
#     output$Cplots5 <- renderPlot({
#         b <- 5
#         xy <- strsplit(dimnames(twoway)[[3]][b]," ")[[1]]
#         tmp <- twoway[,,b,as.integer(input$Cont)]
#         par(mar = c(5.1, 4.1 + 3, 4.1, 2.1))
#         contour(1:LO,1:LO,tmp,xaxt = "n",yaxt = "n")
#         axis(1,seq(1,LO,length.out = 11),0:10)
#         axis(2,seq(1,LO,length.out = 11),0:10,las = 2)
#         title(xlab = names(nms[which(nms %in% xy[1])]),ylab = names(nms[which(nms %in% xy[2])]))
#     })
#     
#     output$Cplots6 <- renderPlot({
#         b <- 6
#         xy <- strsplit(dimnames(twoway)[[3]][b]," ")[[1]]
#         tmp <- twoway[,,b,as.integer(input$Cont)]
#         par(mar = c(5.1, 4.1 + 3, 4.1, 2.1))
#         contour(1:LO,1:LO,tmp,xaxt = "n",yaxt = "n")
#         axis(1,seq(1,LO,length.out = 11),0:10)
#         axis(2,seq(1,LO,length.out = 11),0:10,las = 2)
#         title(xlab = names(nms[which(nms %in% xy[1])]),ylab = names(nms[which(nms %in% xy[2])]))
#     })
#     
#     output$Cplots7 <- renderPlot({
#         b <- 7
#         xy <- strsplit(dimnames(twoway)[[3]][b]," ")[[1]]
#         tmp <- twoway[,,b,as.integer(input$Cont)]
#         par(mar = c(5.1, 4.1 + 3, 4.1, 2.1))
#         contour(1:LO,1:LO,tmp,xaxt = "n",yaxt = "n")
#         axis(1,seq(1,LO,length.out = 11),0:10)
#         axis(2,seq(1,LO,length.out = 11),0:10,las = 2)
#         title(xlab = names(nms[which(nms %in% xy[1])]),ylab = names(nms[which(nms %in% xy[2])]))
#     })
#     
#     output$Cplots8 <- renderPlot({
#         b <- 8
#         xy <- strsplit(dimnames(twoway)[[3]][b]," ")[[1]]
#         tmp <- twoway[,,b,as.integer(input$Cont)]
#         par(mar = c(5.1, 4.1 + 3, 4.1, 2.1))
#         contour(1:LO,1:LO,tmp,xaxt = "n",yaxt = "n")
#         axis(1,seq(1,LO,length.out = 11),0:10)
#         axis(2,seq(1,LO,length.out = 11),0:10,las = 2)
#         title(xlab = names(nms[which(nms %in% xy[1])]),ylab = names(nms[which(nms %in% xy[2])]))
#     })
#     
#     output$Cplots9 <- renderPlot({
#         b <- 9
#         xy <- strsplit(dimnames(twoway)[[3]][b]," ")[[1]]
#         tmp <- twoway[,,b,as.integer(input$Cont)]
#         par(mar = c(5.1, 4.1 + 3, 4.1, 2.1))
#         contour(1:LO,1:LO,tmp,xaxt = "n",yaxt = "n")
#         axis(1,seq(1,LO,length.out = 11),0:10)
#         axis(2,seq(1,LO,length.out = 11),0:10,las = 2)
#         title(xlab = names(nms[which(nms %in% xy[1])]),ylab = names(nms[which(nms %in% xy[2])]))
#     })
#     
#     output$Cplots10 <- renderPlot({
#         b <- 10
#         xy <- strsplit(dimnames(twoway)[[3]][b]," ")[[1]]
#         tmp <- twoway[,,b,as.integer(input$Cont)]
#         par(mar = c(5.1, 4.1 + 3, 4.1, 2.1))
#         contour(1:LO,1:LO,tmp,xaxt = "n",yaxt = "n")
#         axis(1,seq(1,LO,length.out = 11),0:10)
#         axis(2,seq(1,LO,length.out = 11),0:10,las = 2)
#         title(xlab = names(nms[which(nms %in% xy[1])]),ylab = names(nms[which(nms %in% xy[2])]))
#     })
#     
#     output$Cplots11 <- renderPlot({
#         b <- 11
#         xy <- strsplit(dimnames(twoway)[[3]][b]," ")[[1]]
#         tmp <- twoway[,,b,as.integer(input$Cont)]
#         par(mar = c(5.1, 4.1 + 3, 4.1, 2.1))
#         contour(1:LO,1:LO,tmp,xaxt = "n",yaxt = "n")
#         axis(1,seq(1,LO,length.out = 11),0:10)
#         axis(2,seq(1,LO,length.out = 11),0:10,las = 2)
#         title(xlab = names(nms[which(nms %in% xy[1])]),ylab = names(nms[which(nms %in% xy[2])]))
#     })
#     
#     output$Cplots12 <- renderPlot({
#         b <- 12
#         xy <- strsplit(dimnames(twoway)[[3]][b]," ")[[1]]
#         tmp <- twoway[,,b,as.integer(input$Cont)]
#         par(mar = c(5.1, 4.1 + 3, 4.1, 2.1))
#         contour(1:LO,1:LO,tmp,xaxt = "n",yaxt = "n")
#         axis(1,seq(1,LO,length.out = 11),0:10)
#         axis(2,seq(1,LO,length.out = 11),0:10,las = 2)
#         title(xlab = names(nms[which(nms %in% xy[1])]),ylab = names(nms[which(nms %in% xy[2])]))
#     })
#     
#     output$Cplots13 <- renderPlot({
#         b <- 13
#         xy <- strsplit(dimnames(twoway)[[3]][b]," ")[[1]]
#         tmp <- twoway[,,b,as.integer(input$Cont)]
#         par(mar = c(5.1, 4.1 + 3, 4.1, 2.1))
#         contour(1:LO,1:LO,tmp,xaxt = "n",yaxt = "n")
#         axis(1,seq(1,LO,length.out = 11),0:10)
#         axis(2,seq(1,LO,length.out = 11),0:10,las = 2)
#         title(xlab = names(nms[which(nms %in% xy[1])]),ylab = names(nms[which(nms %in% xy[2])]))
#     })
#     
#     output$Cplots14 <- renderPlot({
#         b <- 14
#         xy <- strsplit(dimnames(twoway)[[3]][b]," ")[[1]]
#         tmp <- twoway[,,b,as.integer(input$Cont)]
#         par(mar = c(5.1, 4.1 + 3, 4.1, 2.1))
#         contour(1:LO,1:LO,tmp,xaxt = "n",yaxt = "n")
#         axis(1,seq(1,LO,length.out = 11),0:10)
#         axis(2,seq(1,LO,length.out = 11),0:10,las = 2)
#         title(xlab = names(nms[which(nms %in% xy[1])]),ylab = names(nms[which(nms %in% xy[2])]))
#     })
#     
#     output$Cplots15 <- renderPlot({
#         b <- 15
#         xy <- strsplit(dimnames(twoway)[[3]][b]," ")[[1]]
#         tmp <- twoway[,,b,as.integer(input$Cont)]
#         par(mar = c(5.1, 4.1 + 3, 4.1, 2.1))
#         contour(1:LO,1:LO,tmp,xaxt = "n",yaxt = "n")
#         axis(1,seq(1,LO,length.out = 11),0:10)
#         axis(2,seq(1,LO,length.out = 11),0:10,las = 2)
#         title(xlab = names(nms[which(nms %in% xy[1])]),ylab = names(nms[which(nms %in% xy[2])]))
#     })
#     
#     output$Cplots16 <- renderPlot({
#         b <- 16
#         xy <- strsplit(dimnames(twoway)[[3]][b]," ")[[1]]
#         tmp <- twoway[,,b,as.integer(input$Cont)]
#         par(mar = c(5.1, 4.1 + 3, 4.1, 2.1))
#         contour(1:LO,1:LO,tmp,xaxt = "n",yaxt = "n")
#         axis(1,seq(1,LO,length.out = 11),0:10)
#         axis(2,seq(1,LO,length.out = 11),0:10,las = 2)
#         title(xlab = names(nms[which(nms %in% xy[1])]),ylab = names(nms[which(nms %in% xy[2])]))
#         title(xlab = names(nms[which(nms %in% xy[1])]),ylab = names(nms[which(nms %in% xy[2])]))
#     })
#     
#     output$Cplots17 <- renderPlot({
#         b <- 17
#         xy <- strsplit(dimnames(twoway)[[3]][b]," ")[[1]]
#         tmp <- twoway[,,b,as.integer(input$Cont)]
#         par(mar = c(5.1, 4.1 + 3, 4.1, 2.1))
#         contour(1:LO,1:LO,tmp,xaxt = "n",yaxt = "n")
#         axis(1,seq(1,LO,length.out = 11),0:10)
#         axis(2,seq(1,LO,length.out = 11),0:10,las = 2)
#         title(xlab = names(nms[which(nms %in% xy[1])]),ylab = names(nms[which(nms %in% xy[2])]))
#     })
#     
#     output$Cplots18 <- renderPlot({
#         b <- 18
#         xy <- strsplit(dimnames(twoway)[[3]][b]," ")[[1]]
#         tmp <- twoway[,,b,as.integer(input$Cont)]
#         par(mar = c(5.1, 4.1 + 3, 4.1, 2.1))
#         contour(1:LO,1:LO,tmp,xaxt = "n",yaxt = "n")
#         axis(1,seq(1,LO,length.out = 11),0:10)
#         axis(2,seq(1,LO,length.out = 11),0:10,las = 2)
#         title(xlab = names(nms[which(nms %in% xy[1])]),ylab = names(nms[which(nms %in% xy[2])]))
#     })
#     
#     output$Cplots19 <- renderPlot({
#         b <- 19
#         xy <- strsplit(dimnames(twoway)[[3]][b]," ")[[1]]
#         tmp <- twoway[,,b,as.integer(input$Cont)]
#         par(mar = c(5.1, 4.1 + 3, 4.1, 2.1))
#         contour(1:LO,1:LO,tmp,xaxt = "n",yaxt = "n")
#         axis(1,seq(1,LO,length.out = 11),0:10)
#         axis(2,seq(1,LO,length.out = 11),0:10,las = 2)
#         title(xlab = names(nms[which(nms %in% xy[1])]),ylab = names(nms[which(nms %in% xy[2])]))
#     })
#     
#     output$Cplots20 <- renderPlot({
#         b <- 20
#         xy <- strsplit(dimnames(twoway)[[3]][b]," ")[[1]]
#         tmp <- twoway[,,b,as.integer(input$Cont)]
#         par(mar = c(5.1, 4.1 + 3, 4.1, 2.1))
#         contour(1:LO,1:LO,tmp,xaxt = "n",yaxt = "n")
#         axis(1,seq(1,LO,length.out = 11),0:10)
#         axis(2,seq(1,LO,length.out = 11),0:10,las = 2)
#         title(xlab = names(nms[which(nms %in% xy[1])]),ylab = names(nms[which(nms %in% xy[2])]))
#     })
#     
#     output$Cplots21 <- renderPlot({
#         b <- 21
#         xy <- strsplit(dimnames(twoway)[[3]][b]," ")[[1]]
#         tmp <- twoway[,,b,as.integer(input$Cont)]
#         par(mar = c(5.1, 4.1 + 3, 4.1, 2.1))
#         contour(1:LO,1:LO,tmp,xaxt = "n",yaxt = "n")
#         axis(1,seq(1,LO,length.out = 11),0:10)
#         axis(2,seq(1,LO,length.out = 11),0:10,las = 2)
#         title(xlab = names(nms[which(nms %in% xy[1])]),ylab = names(nms[which(nms %in% xy[2])]))
#     })
#     
#     output$Cplots22 <- renderPlot({
#         b <- 22
#         xy <- strsplit(dimnames(twoway)[[3]][b]," ")[[1]]
#         tmp <- twoway[,,b,as.integer(input$Cont)]
#         par(mar = c(5.1, 4.1 + 3, 4.1, 2.1))
#         contour(1:LO,1:LO,tmp,xaxt = "n",yaxt = "n")
#         axis(1,seq(1,LO,length.out = 11),0:10)
#         axis(2,seq(1,LO,length.out = 11),0:10,las = 2)
#         title(xlab = names(nms[which(nms %in% xy[1])]),ylab = names(nms[which(nms %in% xy[2])]))
#     })
#     
#     output$Cplots23 <- renderPlot({
#         b <- 23
#         xy <- strsplit(dimnames(twoway)[[3]][b]," ")[[1]]
#         tmp <- twoway[,,b,as.integer(input$Cont)]
#         par(mar = c(5.1, 4.1 + 3, 4.1, 2.1))
#         contour(1:LO,1:LO,tmp,xaxt = "n",yaxt = "n")
#         axis(1,seq(1,LO,length.out = 11),0:10)
#         axis(2,seq(1,LO,length.out = 11),0:10,las = 2)
#         title(xlab = names(nms[which(nms %in% xy[1])]),ylab = names(nms[which(nms %in% xy[2])]))
#     })
#     
#     output$Cplots24 <- renderPlot({
#         b <- 24
#         xy <- strsplit(dimnames(twoway)[[3]][b]," ")[[1]]
#         tmp <- twoway[,,b,as.integer(input$Cont)]
#         par(mar = c(5.1, 4.1 + 3, 4.1, 2.1))
#         contour(1:LO,1:LO,tmp,xaxt = "n",yaxt = "n")
#         axis(1,seq(1,LO,length.out = 11),0:10)
#         axis(2,seq(1,LO,length.out = 11),0:10,las = 2)
#         title(xlab = names(nms[which(nms %in% xy[1])]),ylab = names(nms[which(nms %in% xy[2])]))
#     })
#     
#     output$Cplots25 <- renderPlot({
#         b <- 25
#         xy <- strsplit(dimnames(twoway)[[3]][b]," ")[[1]]
#         tmp <- twoway[,,b,as.integer(input$Cont)]
#         par(mar = c(5.1, 4.1 + 3, 4.1, 2.1))
#         contour(1:LO,1:LO,tmp,xaxt = "n",yaxt = "n")
#         axis(1,seq(1,LO,length.out = 11),0:10)
#         axis(2,seq(1,LO,length.out = 11),0:10,las = 2)
#         title(xlab = names(nms[which(nms %in% xy[1])]),ylab = names(nms[which(nms %in% xy[2])]))
#     })
#     
#     output$Cplots26 <- renderPlot({
#         b <- 26
#         xy <- strsplit(dimnames(twoway)[[3]][b]," ")[[1]]
#         tmp <- twoway[,,b,as.integer(input$Cont)]
#         par(mar = c(5.1, 4.1 + 3, 4.1, 2.1))
#         contour(1:LO,1:LO,tmp,xaxt = "n",yaxt = "n")
#         axis(1,seq(1,LO,length.out = 11),0:10)
#         axis(2,seq(1,LO,length.out = 11),0:10,las = 2)
#         title(xlab = names(nms[which(nms %in% xy[1])]),ylab = names(nms[which(nms %in% xy[2])]))
#     })
#     
#     output$Cplots27 <- renderPlot({
#         b <- 27
#         xy <- strsplit(dimnames(twoway)[[3]][b]," ")[[1]]
#         tmp <- twoway[,,b,as.integer(input$Cont)]
#         par(mar = c(5.1, 4.1 + 3, 4.1, 2.1))
#         contour(1:LO,1:LO,tmp,xaxt = "n",yaxt = "n")
#         axis(1,seq(1,LO,length.out = 11),0:10)
#         axis(2,seq(1,LO,length.out = 11),0:10,las = 2)
#         title(xlab = names(nms[which(nms %in% xy[1])]),ylab = names(nms[which(nms %in% xy[2])]))
#     })
#     
#     output$Cplots28 <- renderPlot({
#         b <- 28
#         xy <- strsplit(dimnames(twoway)[[3]][b]," ")[[1]]
#         tmp <- twoway[,,b,as.integer(input$Cont)]
#         par(mar = c(5.1, 4.1 + 3, 4.1, 2.1))
#         contour(1:LO,1:LO,tmp,xaxt = "n",yaxt = "n")
#         axis(1,seq(1,LO,length.out = 11),0:10)
#         axis(2,seq(1,LO,length.out = 11),0:10,las = 2)
#         title(xlab = names(nms[which(nms %in% xy[1])]),ylab = names(nms[which(nms %in% xy[2])]))
#     })
#     
#     # Response Profile Plots
#     
#     output$RPplots1 <- renderPlot({
#         b <- 1
#         col <- sprintf("grey%i",seq(100,0,-10)) # colors
#         cas <- as.integer(input$RP)
#         cas <- ifelse(1:23 %in% cas,cas,NA)
#         asd <- lapply(1:28,function(x) apply(twoway[,,x,cas],c(1,2),which.max))
#         tmp <- asd[[b]]
#         labs <- sapply(strsplit(dimnames(twoway)[[3]][b]," ")[[1]],function(x) names(nms[which(nms %in% x)]))
#         ii <- unique(as.vector(tmp))
#         ind <- lapply(ii,function(x) which(tmp == x,arr.ind = T))
#         par(mar = c(5.1, 4.1 + 2, 4.1, 2.1))
#         image(x = 1:nrow(tmp),1:nrow(tmp),z = tmp,col=col[seq_along(ii)],xlim = c(0,nrow(tmp)+1), ylim = c(0,nrow(tmp)+1),bty = "n",xaxt = "n",yaxt = "n",xlab = labs[1],ylab = labs[2])
#         invisible(lapply(1:length(ii), function(x) text(ind[[x]][,1],ind[[x]][,2],ii[x],cex = .7) ))
#         axis(2,seq(1,20,length.out = 6),seq(0,10,2),las = 2)
#         axis(1,seq(1,20,length.out = 6),seq(0,10,2))   
#     })
#     
#     output$RPplots2 <- renderPlot({
#         b <- 2
#         col <- sprintf("grey%i",seq(100,0,-10)) # colors
#         cas <- as.integer(input$RP)
#         cas <- ifelse(1:23 %in% cas,cas,NA)
#         asd <- lapply(1:28,function(x) apply(twoway[,,x,cas],c(1,2),which.max))
#         tmp <- asd[[b]]
#         labs <- sapply(strsplit(dimnames(twoway)[[3]][b]," ")[[1]],function(x) names(nms[which(nms %in% x)]))
#         ii <- unique(as.vector(tmp))
#         ind <- lapply(ii,function(x) which(tmp == x,arr.ind = T))
#         par(mar = c(5.1, 4.1 + 2, 4.1, 2.1))
#         image(x = 1:nrow(tmp),1:nrow(tmp),z = tmp,col=col[seq_along(ii)],xlim = c(0,nrow(tmp)+1), ylim = c(0,nrow(tmp)+1),bty = "n",xaxt = "n",yaxt = "n",xlab = labs[1],ylab = labs[2])
#         invisible(lapply(1:length(ii), function(x) text(ind[[x]][,1],ind[[x]][,2],ii[x],cex = .7) ))
#         axis(2,seq(1,20,length.out = 6),seq(0,10,2),las = 2)
#         axis(1,seq(1,20,length.out = 6),seq(0,10,2))   
#     })
#     
#     output$RPplots3 <- renderPlot({
#         b <- 3
#         col <- sprintf("grey%i",seq(100,0,-10)) # colors
#         cas <- as.integer(input$RP)
#         cas <- ifelse(1:23 %in% cas,cas,NA)
#         asd <- lapply(1:28,function(x) apply(twoway[,,x,cas],c(1,2),which.max))
#         tmp <- asd[[b]]
#         labs <- sapply(strsplit(dimnames(twoway)[[3]][b]," ")[[1]],function(x) names(nms[which(nms %in% x)]))
#         ii <- unique(as.vector(tmp))
#         ind <- lapply(ii,function(x) which(tmp == x,arr.ind = T))
#         par(mar = c(5.1, 4.1 + 2, 4.1, 2.1))
#         image(x = 1:nrow(tmp),1:nrow(tmp),z = tmp,col=col[seq_along(ii)],xlim = c(0,nrow(tmp)+1), ylim = c(0,nrow(tmp)+1),bty = "n",xaxt = "n",yaxt = "n",xlab = labs[1],ylab = labs[2])
#         invisible(lapply(1:length(ii), function(x) text(ind[[x]][,1],ind[[x]][,2],ii[x],cex = .7) ))
#         axis(2,seq(1,20,length.out = 6),seq(0,10,2),las = 2)
#         axis(1,seq(1,20,length.out = 6),seq(0,10,2))   
#     })
#     
#     output$RPplots4 <- renderPlot({
#         b <- 4
#         col <- sprintf("grey%i",seq(100,0,-10)) # colors
#         cas <- as.integer(input$RP)
#         cas <- ifelse(1:23 %in% cas,cas,NA)
#         asd <- lapply(1:28,function(x) apply(twoway[,,x,cas],c(1,2),which.max))
#         tmp <- asd[[b]]
#         labs <- sapply(strsplit(dimnames(twoway)[[3]][b]," ")[[1]],function(x) names(nms[which(nms %in% x)]))
#         ii <- unique(as.vector(tmp))
#         ind <- lapply(ii,function(x) which(tmp == x,arr.ind = T))
#         par(mar = c(5.1, 4.1 + 2, 4.1, 2.1))
#         image(x = 1:nrow(tmp),1:nrow(tmp),z = tmp,col=col[seq_along(ii)],xlim = c(0,nrow(tmp)+1), ylim = c(0,nrow(tmp)+1),bty = "n",xaxt = "n",yaxt = "n",xlab = labs[1],ylab = labs[2])
#         invisible(lapply(1:length(ii), function(x) text(ind[[x]][,1],ind[[x]][,2],ii[x],cex = .7) ))
#         axis(2,seq(1,20,length.out = 6),seq(0,10,2),las = 2)
#         axis(1,seq(1,20,length.out = 6),seq(0,10,2))   
#     })
#     
#     output$RPplots5 <- renderPlot({
#         b <- 5
#         col <- sprintf("grey%i",seq(100,0,-10)) # colors
#         cas <- as.integer(input$RP)
#         cas <- ifelse(1:23 %in% cas,cas,NA)
#         asd <- lapply(1:28,function(x) apply(twoway[,,x,cas],c(1,2),which.max))
#         tmp <- asd[[b]]
#         labs <- sapply(strsplit(dimnames(twoway)[[3]][b]," ")[[1]],function(x) names(nms[which(nms %in% x)]))
#         ii <- unique(as.vector(tmp))
#         ind <- lapply(ii,function(x) which(tmp == x,arr.ind = T))
#         par(mar = c(5.1, 4.1 + 2, 4.1, 2.1))
#         image(x = 1:nrow(tmp),1:nrow(tmp),z = tmp,col=col[seq_along(ii)],xlim = c(0,nrow(tmp)+1), ylim = c(0,nrow(tmp)+1),bty = "n",xaxt = "n",yaxt = "n",xlab = labs[1],ylab = labs[2])
#         invisible(lapply(1:length(ii), function(x) text(ind[[x]][,1],ind[[x]][,2],ii[x],cex = .7) ))
#         axis(2,seq(1,20,length.out = 6),seq(0,10,2),las = 2)
#         axis(1,seq(1,20,length.out = 6),seq(0,10,2))   
#     })
#     
#     output$RPplots6 <- renderPlot({
#         b <- 6
#         col <- sprintf("grey%i",seq(100,0,-10)) # colors
#         cas <- as.integer(input$RP)
#         cas <- ifelse(1:23 %in% cas,cas,NA)
#         asd <- lapply(1:28,function(x) apply(twoway[,,x,cas],c(1,2),which.max))
#         tmp <- asd[[b]]
#         labs <- sapply(strsplit(dimnames(twoway)[[3]][b]," ")[[1]],function(x) names(nms[which(nms %in% x)]))
#         ii <- unique(as.vector(tmp))
#         ind <- lapply(ii,function(x) which(tmp == x,arr.ind = T))
#         par(mar = c(5.1, 4.1 + 2, 4.1, 2.1))
#         image(x = 1:nrow(tmp),1:nrow(tmp),z = tmp,col=col[seq_along(ii)],xlim = c(0,nrow(tmp)+1), ylim = c(0,nrow(tmp)+1),bty = "n",xaxt = "n",yaxt = "n",xlab = labs[1],ylab = labs[2])
#         invisible(lapply(1:length(ii), function(x) text(ind[[x]][,1],ind[[x]][,2],ii[x],cex = .7) ))
#         axis(2,seq(1,20,length.out = 6),seq(0,10,2),las = 2)
#         axis(1,seq(1,20,length.out = 6),seq(0,10,2))   
#     })
#     
#     output$RPplots7 <- renderPlot({
#         b <- 7
#         col <- sprintf("grey%i",seq(100,0,-10)) # colors
#         cas <- as.integer(input$RP)
#         cas <- ifelse(1:23 %in% cas,cas,NA)
#         asd <- lapply(1:28,function(x) apply(twoway[,,x,cas],c(1,2),which.max))
#         tmp <- asd[[b]]
#         labs <- sapply(strsplit(dimnames(twoway)[[3]][b]," ")[[1]],function(x) names(nms[which(nms %in% x)]))
#         ii <- unique(as.vector(tmp))
#         ind <- lapply(ii,function(x) which(tmp == x,arr.ind = T))
#         par(mar = c(5.1, 4.1 + 2, 4.1, 2.1))
#         image(x = 1:nrow(tmp),1:nrow(tmp),z = tmp,col=col[seq_along(ii)],xlim = c(0,nrow(tmp)+1), ylim = c(0,nrow(tmp)+1),bty = "n",xaxt = "n",yaxt = "n",xlab = labs[1],ylab = labs[2])
#         invisible(lapply(1:length(ii), function(x) text(ind[[x]][,1],ind[[x]][,2],ii[x],cex = .7) ))
#         axis(2,seq(1,20,length.out = 6),seq(0,10,2),las = 2)
#         axis(1,seq(1,20,length.out = 6),seq(0,10,2))   
#     })
#     
#     output$RPplots8 <- renderPlot({
#         b <- 8
#         col <- sprintf("grey%i",seq(100,0,-10)) # colors
#         cas <- as.integer(input$RP)
#         cas <- ifelse(1:23 %in% cas,cas,NA)
#         asd <- lapply(1:28,function(x) apply(twoway[,,x,cas],c(1,2),which.max))
#         tmp <- asd[[b]]
#         labs <- sapply(strsplit(dimnames(twoway)[[3]][b]," ")[[1]],function(x) names(nms[which(nms %in% x)]))
#         ii <- unique(as.vector(tmp))
#         ind <- lapply(ii,function(x) which(tmp == x,arr.ind = T))
#         par(mar = c(5.1, 4.1 + 2, 4.1, 2.1))
#         image(x = 1:nrow(tmp),1:nrow(tmp),z = tmp,col=col[seq_along(ii)],xlim = c(0,nrow(tmp)+1), ylim = c(0,nrow(tmp)+1),bty = "n",xaxt = "n",yaxt = "n",xlab = labs[1],ylab = labs[2])
#         invisible(lapply(1:length(ii), function(x) text(ind[[x]][,1],ind[[x]][,2],ii[x],cex = .7) ))
#         axis(2,seq(1,20,length.out = 6),seq(0,10,2),las = 2)
#         axis(1,seq(1,20,length.out = 6),seq(0,10,2))   
#     })
#     
#     output$RPplots9 <- renderPlot({
#         b <- 9
#         col <- sprintf("grey%i",seq(100,0,-10)) # colors
#         cas <- as.integer(input$RP)
#         cas <- ifelse(1:23 %in% cas,cas,NA)
#         asd <- lapply(1:28,function(x) apply(twoway[,,x,cas],c(1,2),which.max))
#         tmp <- asd[[b]]
#         labs <- sapply(strsplit(dimnames(twoway)[[3]][b]," ")[[1]],function(x) names(nms[which(nms %in% x)]))
#         ii <- unique(as.vector(tmp))
#         ind <- lapply(ii,function(x) which(tmp == x,arr.ind = T))
#         par(mar = c(5.1, 4.1 + 2, 4.1, 2.1))
#         image(x = 1:nrow(tmp),1:nrow(tmp),z = tmp,col=col[seq_along(ii)],xlim = c(0,nrow(tmp)+1), ylim = c(0,nrow(tmp)+1),bty = "n",xaxt = "n",yaxt = "n",xlab = labs[1],ylab = labs[2])
#         invisible(lapply(1:length(ii), function(x) text(ind[[x]][,1],ind[[x]][,2],ii[x],cex = .7) ))
#         axis(2,seq(1,20,length.out = 6),seq(0,10,2),las = 2)
#         axis(1,seq(1,20,length.out = 6),seq(0,10,2))   
#     })
#     
#     output$RPplots10 <- renderPlot({
#         b <- 10
#         col <- sprintf("grey%i",seq(100,0,-10)) # colors
#         cas <- as.integer(input$RP)
#         cas <- ifelse(1:23 %in% cas,cas,NA)
#         asd <- lapply(1:28,function(x) apply(twoway[,,x,cas],c(1,2),which.max))
#         tmp <- asd[[b]]
#         labs <- sapply(strsplit(dimnames(twoway)[[3]][b]," ")[[1]],function(x) names(nms[which(nms %in% x)]))
#         ii <- unique(as.vector(tmp))
#         ind <- lapply(ii,function(x) which(tmp == x,arr.ind = T))
#         par(mar = c(5.1, 4.1 + 2, 4.1, 2.1))
#         image(x = 1:nrow(tmp),1:nrow(tmp),z = tmp,col=col[seq_along(ii)],xlim = c(0,nrow(tmp)+1), ylim = c(0,nrow(tmp)+1),bty = "n",xaxt = "n",yaxt = "n",xlab = labs[1],ylab = labs[2])
#         invisible(lapply(1:length(ii), function(x) text(ind[[x]][,1],ind[[x]][,2],ii[x],cex = .7) ))
#         axis(2,seq(1,20,length.out = 6),seq(0,10,2),las = 2)
#         axis(1,seq(1,20,length.out = 6),seq(0,10,2))   
#     })
#     
#     output$RPplots11 <- renderPlot({
#         b <- 11
#         col <- sprintf("grey%i",seq(100,0,-10)) # colors
#         cas <- as.integer(input$RP)
#         cas <- ifelse(1:23 %in% cas,cas,NA)
#         asd <- lapply(1:28,function(x) apply(twoway[,,x,cas],c(1,2),which.max))
#         tmp <- asd[[b]]
#         labs <- sapply(strsplit(dimnames(twoway)[[3]][b]," ")[[1]],function(x) names(nms[which(nms %in% x)]))
#         ii <- unique(as.vector(tmp))
#         ind <- lapply(ii,function(x) which(tmp == x,arr.ind = T))
#         par(mar = c(5.1, 4.1 + 2, 4.1, 2.1))
#         image(x = 1:nrow(tmp),1:nrow(tmp),z = tmp,col=col[seq_along(ii)],xlim = c(0,nrow(tmp)+1), ylim = c(0,nrow(tmp)+1),bty = "n",xaxt = "n",yaxt = "n",xlab = labs[1],ylab = labs[2])
#         invisible(lapply(1:length(ii), function(x) text(ind[[x]][,1],ind[[x]][,2],ii[x],cex = .7) ))
#         axis(2,seq(1,20,length.out = 6),seq(0,10,2),las = 2)
#         axis(1,seq(1,20,length.out = 6),seq(0,10,2))   
#     })
#     
#     output$RPplots12 <- renderPlot({
#         b <- 12
#         col <- sprintf("grey%i",seq(100,0,-10)) # colors
#         cas <- as.integer(input$RP)
#         cas <- ifelse(1:23 %in% cas,cas,NA)
#         asd <- lapply(1:28,function(x) apply(twoway[,,x,cas],c(1,2),which.max))
#         tmp <- asd[[b]]
#         labs <- sapply(strsplit(dimnames(twoway)[[3]][b]," ")[[1]],function(x) names(nms[which(nms %in% x)]))
#         ii <- unique(as.vector(tmp))
#         ind <- lapply(ii,function(x) which(tmp == x,arr.ind = T))
#         par(mar = c(5.1, 4.1 + 2, 4.1, 2.1))
#         image(x = 1:nrow(tmp),1:nrow(tmp),z = tmp,col=col[seq_along(ii)],xlim = c(0,nrow(tmp)+1), ylim = c(0,nrow(tmp)+1),bty = "n",xaxt = "n",yaxt = "n",xlab = labs[1],ylab = labs[2])
#         invisible(lapply(1:length(ii), function(x) text(ind[[x]][,1],ind[[x]][,2],ii[x],cex = .7) ))
#         axis(2,seq(1,20,length.out = 6),seq(0,10,2),las = 2)
#         axis(1,seq(1,20,length.out = 6),seq(0,10,2))   
#     })
#     
#     output$RPplots13 <- renderPlot({
#         b <- 13
#         col <- sprintf("grey%i",seq(100,0,-10)) # colors
#         cas <- as.integer(input$RP)
#         cas <- ifelse(1:23 %in% cas,cas,NA)
#         asd <- lapply(1:28,function(x) apply(twoway[,,x,cas],c(1,2),which.max))
#         tmp <- asd[[b]]
#         labs <- sapply(strsplit(dimnames(twoway)[[3]][b]," ")[[1]],function(x) names(nms[which(nms %in% x)]))
#         ii <- unique(as.vector(tmp))
#         ind <- lapply(ii,function(x) which(tmp == x,arr.ind = T))
#         par(mar = c(5.1, 4.1 + 2, 4.1, 2.1))
#         image(x = 1:nrow(tmp),1:nrow(tmp),z = tmp,col=col[seq_along(ii)],xlim = c(0,nrow(tmp)+1), ylim = c(0,nrow(tmp)+1),bty = "n",xaxt = "n",yaxt = "n",xlab = labs[1],ylab = labs[2])
#         invisible(lapply(1:length(ii), function(x) text(ind[[x]][,1],ind[[x]][,2],ii[x],cex = .7) ))
#         axis(2,seq(1,20,length.out = 6),seq(0,10,2),las = 2)
#         axis(1,seq(1,20,length.out = 6),seq(0,10,2))   
#     })
#     
#     output$RPplots14 <- renderPlot({
#         b <- 14
#         col <- sprintf("grey%i",seq(100,0,-10)) # colors
#         cas <- as.integer(input$RP)
#         cas <- ifelse(1:23 %in% cas,cas,NA)
#         asd <- lapply(1:28,function(x) apply(twoway[,,x,cas],c(1,2),which.max))
#         tmp <- asd[[b]]
#         labs <- sapply(strsplit(dimnames(twoway)[[3]][b]," ")[[1]],function(x) names(nms[which(nms %in% x)]))
#         ii <- unique(as.vector(tmp))
#         ind <- lapply(ii,function(x) which(tmp == x,arr.ind = T))
#         par(mar = c(5.1, 4.1 + 2, 4.1, 2.1))
#         image(x = 1:nrow(tmp),1:nrow(tmp),z = tmp,col=col[seq_along(ii)],xlim = c(0,nrow(tmp)+1), ylim = c(0,nrow(tmp)+1),bty = "n",xaxt = "n",yaxt = "n",xlab = labs[1],ylab = labs[2])
#         invisible(lapply(1:length(ii), function(x) text(ind[[x]][,1],ind[[x]][,2],ii[x],cex = .7) ))
#         axis(2,seq(1,20,length.out = 6),seq(0,10,2),las = 2)
#         axis(1,seq(1,20,length.out = 6),seq(0,10,2))   
#     })
#     
#     output$RPplots15 <- renderPlot({
#         b <- 15
#         col <- sprintf("grey%i",seq(100,0,-10)) # colors
#         cas <- as.integer(input$RP)
#         cas <- ifelse(1:23 %in% cas,cas,NA)
#         asd <- lapply(1:28,function(x) apply(twoway[,,x,cas],c(1,2),which.max))
#         tmp <- asd[[b]]
#         labs <- sapply(strsplit(dimnames(twoway)[[3]][b]," ")[[1]],function(x) names(nms[which(nms %in% x)]))
#         ii <- unique(as.vector(tmp))
#         ind <- lapply(ii,function(x) which(tmp == x,arr.ind = T))
#         par(mar = c(5.1, 4.1 + 2, 4.1, 2.1))
#         image(x = 1:nrow(tmp),1:nrow(tmp),z = tmp,col=col[seq_along(ii)],xlim = c(0,nrow(tmp)+1), ylim = c(0,nrow(tmp)+1),bty = "n",xaxt = "n",yaxt = "n",xlab = labs[1],ylab = labs[2])
#         invisible(lapply(1:length(ii), function(x) text(ind[[x]][,1],ind[[x]][,2],ii[x],cex = .7) ))
#         axis(2,seq(1,20,length.out = 6),seq(0,10,2),las = 2)
#         axis(1,seq(1,20,length.out = 6),seq(0,10,2))   
#     })
#     
#     output$RPplots16 <- renderPlot({
#         b <- 16
#         col <- sprintf("grey%i",seq(100,0,-10)) # colors
#         cas <- as.integer(input$RP)
#         cas <- ifelse(1:23 %in% cas,cas,NA)
#         asd <- lapply(1:28,function(x) apply(twoway[,,x,cas],c(1,2),which.max))
#         tmp <- asd[[b]]
#         labs <- sapply(strsplit(dimnames(twoway)[[3]][b]," ")[[1]],function(x) names(nms[which(nms %in% x)]))
#         ii <- unique(as.vector(tmp))
#         ind <- lapply(ii,function(x) which(tmp == x,arr.ind = T))
#         par(mar = c(5.1, 4.1 + 2, 4.1, 2.1))
#         image(x = 1:nrow(tmp),1:nrow(tmp),z = tmp,col=col[seq_along(ii)],xlim = c(0,nrow(tmp)+1), ylim = c(0,nrow(tmp)+1),bty = "n",xaxt = "n",yaxt = "n",xlab = labs[1],ylab = labs[2])
#         invisible(lapply(1:length(ii), function(x) text(ind[[x]][,1],ind[[x]][,2],ii[x],cex = .7) ))
#         axis(2,seq(1,20,length.out = 6),seq(0,10,2),las = 2)
#         axis(1,seq(1,20,length.out = 6),seq(0,10,2))   
#     })
#     
#     output$RPplots17 <- renderPlot({
#         b <- 17
#         col <- sprintf("grey%i",seq(100,0,-10)) # colors
#         cas <- as.integer(input$RP)
#         cas <- ifelse(1:23 %in% cas,cas,NA)
#         asd <- lapply(1:28,function(x) apply(twoway[,,x,cas],c(1,2),which.max))
#         tmp <- asd[[b]]
#         labs <- sapply(strsplit(dimnames(twoway)[[3]][b]," ")[[1]],function(x) names(nms[which(nms %in% x)]))
#         ii <- unique(as.vector(tmp))
#         ind <- lapply(ii,function(x) which(tmp == x,arr.ind = T))
#         par(mar = c(5.1, 4.1 + 2, 4.1, 2.1))
#         image(x = 1:nrow(tmp),1:nrow(tmp),z = tmp,col=col[seq_along(ii)],xlim = c(0,nrow(tmp)+1), ylim = c(0,nrow(tmp)+1),bty = "n",xaxt = "n",yaxt = "n",xlab = labs[1],ylab = labs[2])
#         invisible(lapply(1:length(ii), function(x) text(ind[[x]][,1],ind[[x]][,2],ii[x],cex = .7) ))
#         axis(2,seq(1,20,length.out = 6),seq(0,10,2),las = 2)
#         axis(1,seq(1,20,length.out = 6),seq(0,10,2))   
#     })
#     
#     output$RPplots18 <- renderPlot({
#         b <- 18
#         col <- sprintf("grey%i",seq(100,0,-10)) # colors
#         cas <- as.integer(input$RP)
#         cas <- ifelse(1:23 %in% cas,cas,NA)
#         asd <- lapply(1:28,function(x) apply(twoway[,,x,cas],c(1,2),which.max))
#         tmp <- asd[[b]]
#         labs <- sapply(strsplit(dimnames(twoway)[[3]][b]," ")[[1]],function(x) names(nms[which(nms %in% x)]))
#         ii <- unique(as.vector(tmp))
#         ind <- lapply(ii,function(x) which(tmp == x,arr.ind = T))
#         par(mar = c(5.1, 4.1 + 2, 4.1, 2.1))
#         image(x = 1:nrow(tmp),1:nrow(tmp),z = tmp,col=col[seq_along(ii)],xlim = c(0,nrow(tmp)+1), ylim = c(0,nrow(tmp)+1),bty = "n",xaxt = "n",yaxt = "n",xlab = labs[1],ylab = labs[2])
#         invisible(lapply(1:length(ii), function(x) text(ind[[x]][,1],ind[[x]][,2],ii[x],cex = .7) ))
#         axis(2,seq(1,20,length.out = 6),seq(0,10,2),las = 2)
#         axis(1,seq(1,20,length.out = 6),seq(0,10,2))   
#     })
#     
#     output$RPplots19 <- renderPlot({
#         b <- 19
#         col <- sprintf("grey%i",seq(100,0,-10)) # colors
#         cas <- as.integer(input$RP)
#         cas <- ifelse(1:23 %in% cas,cas,NA)
#         asd <- lapply(1:28,function(x) apply(twoway[,,x,cas],c(1,2),which.max))
#         tmp <- asd[[b]]
#         labs <- sapply(strsplit(dimnames(twoway)[[3]][b]," ")[[1]],function(x) names(nms[which(nms %in% x)]))
#         ii <- unique(as.vector(tmp))
#         ind <- lapply(ii,function(x) which(tmp == x,arr.ind = T))
#         par(mar = c(5.1, 4.1 + 2, 4.1, 2.1))
#         image(x = 1:nrow(tmp),1:nrow(tmp),z = tmp,col=col[seq_along(ii)],xlim = c(0,nrow(tmp)+1), ylim = c(0,nrow(tmp)+1),bty = "n",xaxt = "n",yaxt = "n",xlab = labs[1],ylab = labs[2])
#         invisible(lapply(1:length(ii), function(x) text(ind[[x]][,1],ind[[x]][,2],ii[x],cex = .7) ))
#         axis(2,seq(1,20,length.out = 6),seq(0,10,2),las = 2)
#         axis(1,seq(1,20,length.out = 6),seq(0,10,2))   
#     })
#     
#     output$RPplots20 <- renderPlot({
#         b <- 20
#         col <- sprintf("grey%i",seq(100,0,-10)) # colors
#         cas <- as.integer(input$RP)
#         cas <- ifelse(1:23 %in% cas,cas,NA)
#         asd <- lapply(1:28,function(x) apply(twoway[,,x,cas],c(1,2),which.max))
#         tmp <- asd[[b]]
#         labs <- sapply(strsplit(dimnames(twoway)[[3]][b]," ")[[1]],function(x) names(nms[which(nms %in% x)]))
#         ii <- unique(as.vector(tmp))
#         ind <- lapply(ii,function(x) which(tmp == x,arr.ind = T))
#         par(mar = c(5.1, 4.1 + 2, 4.1, 2.1))
#         image(x = 1:nrow(tmp),1:nrow(tmp),z = tmp,col=col[seq_along(ii)],xlim = c(0,nrow(tmp)+1), ylim = c(0,nrow(tmp)+1),bty = "n",xaxt = "n",yaxt = "n",xlab = labs[1],ylab = labs[2])
#         invisible(lapply(1:length(ii), function(x) text(ind[[x]][,1],ind[[x]][,2],ii[x],cex = .7) ))
#         axis(2,seq(1,20,length.out = 6),seq(0,10,2),las = 2)
#         axis(1,seq(1,20,length.out = 6),seq(0,10,2))   
#     })
#     
#     output$RPplots21 <- renderPlot({
#         b <- 21
#         col <- sprintf("grey%i",seq(100,0,-10)) # colors
#         cas <- as.integer(input$RP)
#         cas <- ifelse(1:23 %in% cas,cas,NA)
#         asd <- lapply(1:28,function(x) apply(twoway[,,x,cas],c(1,2),which.max))
#         tmp <- asd[[b]]
#         labs <- sapply(strsplit(dimnames(twoway)[[3]][b]," ")[[1]],function(x) names(nms[which(nms %in% x)]))
#         ii <- unique(as.vector(tmp))
#         ind <- lapply(ii,function(x) which(tmp == x,arr.ind = T))
#         par(mar = c(5.1, 4.1 + 2, 4.1, 2.1))
#         image(x = 1:nrow(tmp),1:nrow(tmp),z = tmp,col=col[seq_along(ii)],xlim = c(0,nrow(tmp)+1), ylim = c(0,nrow(tmp)+1),bty = "n",xaxt = "n",yaxt = "n",xlab = labs[1],ylab = labs[2])
#         invisible(lapply(1:length(ii), function(x) text(ind[[x]][,1],ind[[x]][,2],ii[x],cex = .7) ))
#         axis(2,seq(1,20,length.out = 6),seq(0,10,2),las = 2)
#         axis(1,seq(1,20,length.out = 6),seq(0,10,2))   
#     })
#     
#     output$RPplots22 <- renderPlot({
#         b <- 22
#         col <- sprintf("grey%i",seq(100,0,-10)) # colors
#         cas <- as.integer(input$RP)
#         cas <- ifelse(1:23 %in% cas,cas,NA)
#         asd <- lapply(1:28,function(x) apply(twoway[,,x,cas],c(1,2),which.max))
#         tmp <- asd[[b]]
#         labs <- sapply(strsplit(dimnames(twoway)[[3]][b]," ")[[1]],function(x) names(nms[which(nms %in% x)]))
#         ii <- unique(as.vector(tmp))
#         ind <- lapply(ii,function(x) which(tmp == x,arr.ind = T))
#         par(mar = c(5.1, 4.1 + 2, 4.1, 2.1))
#         image(x = 1:nrow(tmp),1:nrow(tmp),z = tmp,col=col[seq_along(ii)],xlim = c(0,nrow(tmp)+1), ylim = c(0,nrow(tmp)+1),bty = "n",xaxt = "n",yaxt = "n",xlab = labs[1],ylab = labs[2])
#         invisible(lapply(1:length(ii), function(x) text(ind[[x]][,1],ind[[x]][,2],ii[x],cex = .7) ))
#         axis(2,seq(1,20,length.out = 6),seq(0,10,2),las = 2)
#         axis(1,seq(1,20,length.out = 6),seq(0,10,2))   
#     })
#     
#     output$RPplots23 <- renderPlot({
#         b <- 23
#         col <- sprintf("grey%i",seq(100,0,-10)) # colors
#         cas <- as.integer(input$RP)
#         cas <- ifelse(1:23 %in% cas,cas,NA)
#         asd <- lapply(1:28,function(x) apply(twoway[,,x,cas],c(1,2),which.max))
#         tmp <- asd[[b]]
#         labs <- sapply(strsplit(dimnames(twoway)[[3]][b]," ")[[1]],function(x) names(nms[which(nms %in% x)]))
#         ii <- unique(as.vector(tmp))
#         ind <- lapply(ii,function(x) which(tmp == x,arr.ind = T))
#         par(mar = c(5.1, 4.1 + 2, 4.1, 2.1))
#         image(x = 1:nrow(tmp),1:nrow(tmp),z = tmp,col=col[seq_along(ii)],xlim = c(0,nrow(tmp)+1), ylim = c(0,nrow(tmp)+1),bty = "n",xaxt = "n",yaxt = "n",xlab = labs[1],ylab = labs[2])
#         invisible(lapply(1:length(ii), function(x) text(ind[[x]][,1],ind[[x]][,2],ii[x],cex = .7) ))
#         axis(2,seq(1,20,length.out = 6),seq(0,10,2),las = 2)
#         axis(1,seq(1,20,length.out = 6),seq(0,10,2))   
#     })
#     
#     output$RPplots24 <- renderPlot({
#         b <- 24
#         col <- sprintf("grey%i",seq(100,0,-10)) # colors
#         cas <- as.integer(input$RP)
#         cas <- ifelse(1:23 %in% cas,cas,NA)
#         asd <- lapply(1:28,function(x) apply(twoway[,,x,cas],c(1,2),which.max))
#         tmp <- asd[[b]]
#         labs <- sapply(strsplit(dimnames(twoway)[[3]][b]," ")[[1]],function(x) names(nms[which(nms %in% x)]))
#         ii <- unique(as.vector(tmp))
#         ind <- lapply(ii,function(x) which(tmp == x,arr.ind = T))
#         par(mar = c(5.1, 4.1 + 2, 4.1, 2.1))
#         image(x = 1:nrow(tmp),1:nrow(tmp),z = tmp,col=col[seq_along(ii)],xlim = c(0,nrow(tmp)+1), ylim = c(0,nrow(tmp)+1),bty = "n",xaxt = "n",yaxt = "n",xlab = labs[1],ylab = labs[2])
#         invisible(lapply(1:length(ii), function(x) text(ind[[x]][,1],ind[[x]][,2],ii[x],cex = .7) ))
#         axis(2,seq(1,20,length.out = 6),seq(0,10,2),las = 2)
#         axis(1,seq(1,20,length.out = 6),seq(0,10,2))   
#     })
#     
#     output$RPplots25 <- renderPlot({
#         b <- 25
#         col <- sprintf("grey%i",seq(100,0,-10)) # colors
#         cas <- as.integer(input$RP)
#         cas <- ifelse(1:23 %in% cas,cas,NA)
#         asd <- lapply(1:28,function(x) apply(twoway[,,x,cas],c(1,2),which.max))
#         tmp <- asd[[b]]
#         labs <- sapply(strsplit(dimnames(twoway)[[3]][b]," ")[[1]],function(x) names(nms[which(nms %in% x)]))
#         ii <- unique(as.vector(tmp))
#         ind <- lapply(ii,function(x) which(tmp == x,arr.ind = T))
#         par(mar = c(5.1, 4.1 + 2, 4.1, 2.1))
#         image(x = 1:nrow(tmp),1:nrow(tmp),z = tmp,col=col[seq_along(ii)],xlim = c(0,nrow(tmp)+1), ylim = c(0,nrow(tmp)+1),bty = "n",xaxt = "n",yaxt = "n",xlab = labs[1],ylab = labs[2])
#         invisible(lapply(1:length(ii), function(x) text(ind[[x]][,1],ind[[x]][,2],ii[x],cex = .7) ))
#         axis(2,seq(1,20,length.out = 6),seq(0,10,2),las = 2)
#         axis(1,seq(1,20,length.out = 6),seq(0,10,2))   
#     })
#     
#     output$RPplots26 <- renderPlot({
#         b <- 26
#         col <- sprintf("grey%i",seq(100,0,-10)) # colors
#         cas <- as.integer(input$RP)
#         cas <- ifelse(1:23 %in% cas,cas,NA)
#         asd <- lapply(1:28,function(x) apply(twoway[,,x,cas],c(1,2),which.max))
#         tmp <- asd[[b]]
#         labs <- sapply(strsplit(dimnames(twoway)[[3]][b]," ")[[1]],function(x) names(nms[which(nms %in% x)]))
#         ii <- unique(as.vector(tmp))
#         ind <- lapply(ii,function(x) which(tmp == x,arr.ind = T))
#         par(mar = c(5.1, 4.1 + 2, 4.1, 2.1))
#         image(x = 1:nrow(tmp),1:nrow(tmp),z = tmp,col=col[seq_along(ii)],xlim = c(0,nrow(tmp)+1), ylim = c(0,nrow(tmp)+1),bty = "n",xaxt = "n",yaxt = "n",xlab = labs[1],ylab = labs[2])
#         invisible(lapply(1:length(ii), function(x) text(ind[[x]][,1],ind[[x]][,2],ii[x],cex = .7) ))
#         axis(2,seq(1,20,length.out = 6),seq(0,10,2),las = 2)
#         axis(1,seq(1,20,length.out = 6),seq(0,10,2))   
#     })
#     
#     output$RPplots27 <- renderPlot({
#         b <- 27
#         col <- sprintf("grey%i",seq(100,0,-10)) # colors
#         cas <- as.integer(input$RP)
#         cas <- ifelse(1:23 %in% cas,cas,NA)
#         asd <- lapply(1:28,function(x) apply(twoway[,,x,cas],c(1,2),which.max))
#         tmp <- asd[[b]]
#         labs <- sapply(strsplit(dimnames(twoway)[[3]][b]," ")[[1]],function(x) names(nms[which(nms %in% x)]))
#         ii <- unique(as.vector(tmp))
#         ind <- lapply(ii,function(x) which(tmp == x,arr.ind = T))
#         par(mar = c(5.1, 4.1 + 2, 4.1, 2.1))
#         image(x = 1:nrow(tmp),1:nrow(tmp),z = tmp,col=col[seq_along(ii)],xlim = c(0,nrow(tmp)+1), ylim = c(0,nrow(tmp)+1),bty = "n",xaxt = "n",yaxt = "n",xlab = labs[1],ylab = labs[2])
#         invisible(lapply(1:length(ii), function(x) text(ind[[x]][,1],ind[[x]][,2],ii[x],cex = .7) ))
#         axis(2,seq(1,20,length.out = 6),seq(0,10,2),las = 2)
#         axis(1,seq(1,20,length.out = 6),seq(0,10,2))   
#     })
#     
#     output$RPplots28 <- renderPlot({
#         b <- 28
#         col <- sprintf("grey%i",seq(100,0,-10)) # colors
#         cas <- as.integer(input$RP)
#         cas <- ifelse(1:23 %in% cas,cas,NA)
#         asd <- lapply(1:28,function(x) apply(twoway[,,x,cas],c(1,2),which.max))
#         tmp <- asd[[b]]
#         labs <- sapply(strsplit(dimnames(twoway)[[3]][b]," ")[[1]],function(x) names(nms[which(nms %in% x)]))
#         ii <- unique(as.vector(tmp))
#         ind <- lapply(ii,function(x) which(tmp == x,arr.ind = T))
#         par(mar = c(5.1, 4.1 + 2, 4.1, 2.1))
#         image(x = 1:nrow(tmp),1:nrow(tmp),z = tmp,col=col[seq_along(ii)],xlim = c(0,nrow(tmp)+1), ylim = c(0,nrow(tmp)+1),bty = "n",xaxt = "n",yaxt = "n",xlab = labs[1],ylab = labs[2])
#         invisible(lapply(1:length(ii), function(x) text(ind[[x]][,1],ind[[x]][,2],ii[x],cex = .7) ))
#         axis(2,seq(1,20,length.out = 6),seq(0,10,2),las = 2)
#         axis(1,seq(1,20,length.out = 6),seq(0,10,2))   
#     })
# }
# 
# # Run the application 
# shinyApp(ui = ui, server = server)




