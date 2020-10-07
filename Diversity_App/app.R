#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

rm(list=ls()) #Clear R

library(shiny)
library(tidyverse)
library(shinythemes)

sample_data<-read.csv("Plant_Biota.csv",header=T,stringsAsFactors = T)
sample_data2<-pivot_longer(sample_data,cols=c("shannon","simpson","insimpson"))%>%rename("Index"="name","alpha"="value")
sample_data3<-pivot_longer(sample_data2,cols=c("Site","Genotype","Type"))%>%rename("Group"="name","level"="value")

a<-

ui <- navbarPage("Diversity of Plant\nmicrobiota",
               theme=shinytheme('united'),
               navbarMenu("Navigate",
                          tabPanel("Intro", strong("The Study"),
                                   br(),
                                   HTML("We will look at some data from Wagner, <em>et al</em> 2016."),
                                   tags$a(href="https://www.nature.com/articles/ncomms12151", 
                                          "Full Paper Here!"),
                                   br(),
                                   HTML("The Author's were intereted in the bacterial microbiota of a wild mustard plant <em>Boechera stricta</em>. They sampled both the roots and the leaves of five genotypes of the plant at several different locations."),
                                   br(),
                                   br(),
                                   HTML("Use the 'Navigate' button above to explore how different indicies of alpha diversity generate different pattern in the data. You can also break down both alpha and beta diversity by different groupings."),
                                   br(),
                                   br(),
                                   ),
                          tabPanel("Alpha", strong("Metrics of Alpha Diversity"),
                          sidebarLayout(position="left",
                                        sidebarPanel(width=4,
                                            selectInput(
                                            "index",
                                            "Select a metric of alpha diversity:",
                                            choices = c("shannon","simpson","insimpson")),
                                            selectInput("group",
                                             "Select groups to compare:",
                                             choices=c("Site","Genotype","Type"))),
                                        mainPanel(width=5,
                                            plotOutput("plot")
                                            ))),
                          tabPanel(width=4,"Beta", strong("Visualising Beta Diversity"),
                                   sidebarLayout(position="left",
                                                 sidebarPanel(
                                                     selectInput("group2",
                                                                  "Select groups to compare:",
                                                                  choices=c("Site","Genotype","Type"))),
                                                 mainPanel(width=5,plotOutput("betaplot"))))),
               img(src="design.png",width="300px"),
               img(src="plant.jpg",width="300px",height="400px")
               )

server <- function(input, output, session) {
    dataset <- reactive({
        filter(sample_data3,Index==input$index&Group==input$group)
    })
    dataset2 <- reactive({
        filter(sample_data3,Group==input$group2)
    })
    output$plot<-renderPlot(
        ggplot(data=dataset(), aes(x = level, y = alpha,col=level,fill=level)) + 
            geom_boxplot(alpha=.2)+
            geom_point(position=position_jitter(w=.1))+
            theme_bw()+
            scale_x_discrete("")+
            scale_y_continuous("Diversity Measure")+
            theme(legend.position = "None",
                  axis.text = element_text(size=15),
                  axis.title = element_text(size=15))
    )
    output$betaplot<-renderPlot(
        ggplot(data=dataset2(), aes(x = MDS1, y = MDS2, color = level)) +
            geom_point(size=3,alpha=.5)+
            theme_bw()+
            theme(legend.position = c(.9,.8),
                  axis.text = element_text(size=15),
                  axis.title = element_text(size=15))+
            scale_color_discrete("Grouping")+
            ggtitle(paste("Beta Diversity","-","Bray-Curtis"))
    )
}

shinyApp(ui,server)

