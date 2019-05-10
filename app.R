library(shiny)
library(dplyr)
library(ggvis)
library(gsheet)
library(DT)
library(data.table)
library(rsconnect)
#library(questionr)
library(ggplot2)
library(shinythemes)
library(visNetwork)
library(stringr)
library(plotly)

#load the dataset from the google sheets document
url <- 'https://docs.google.com/spreadsheets/d/1VBnEGbanAt5yLXkpMGRGJWZeuw1TLNVudksgXyksHq0/edit?ts=5beae9fc#gid=0'
#url <- 'https://docs.google.com/spreadsheets/d/1Li1ufThPTZNE_svGQtVlHb-5vQ9EiwX2pOTKvjxfd0E/edit?usp=sharing'

etd <- as.data.frame(read.csv(text = gsheet2text(url, format = 'csv'), stringsAsFactors = FALSE))

#add clickable icons with link to the paper
link_icon<-as.character(img(src="pdf-3.png", height="13px", width="13px"))
link_icon_osf<-as.character(img(src="osf.png", height="13px", width="13px"))
etd$Link.to.the.paper <- paste0("<a href='",etd$Link.to.the.paper,"' target='_blank'>",link_icon, "</a>", 
                                "<a href='",etd$Link.to.the.OSF.Project,"' target='_blank'>",link_icon_osf,"</a>")

# c is one dimentional array (vector)

original_effect_size        <- etd[,c("Effect.size.Original")]
low_original_effect_size    <- etd[,c("CI_low.Original")]
high_original_effect_size   <- etd[,c("CI_high.Original")]

replicated_effect_size      <- etd[,c("Effect.size.Replication")]
low_replicated_effect_size  <- etd[,c("CI_low.Replication")]
high_replicated_effect_size <- etd[,c("CI_high.Replication")]

original_effect_size_y_zus = seq(from=2, to=length(original_effect_size)*3+1, by=3)
replicated_effect_size_y_zus = seq(from=1, to=length(replicated_effect_size)*3, by=3)

original_effect_size_y = seq(1, length(original_effect_size))
replicated_effect_size_y = seq(1, length(replicated_effect_size))

#Load the Codebook with additional Info about the columns of the dataset: Order,TopCategory, SubCategory, Variable, Clear.Variable.Name, Definitions,
url_codebook<-'https://docs.google.com/spreadsheets/d/1f5QYC1F-Pd2v9N4DU6Yf29jdKPhOzdad6B2GDhWoVjM/edit?usp=sharing'
codebook<-as.data.frame(read.csv(text = gsheet2text(url_codebook, format = 'csv'), stringsAsFactors = FALSE))
codebook <- subset(codebook, !is.na(Order) & Clear.Variable.Name != "")

#replace the column names with the
etd.column.names<-as.vector(codebook$Clear.Variable.Name)
colnames(etd)<-etd.column.names

#to check the names of the rows uncomment
print(typeof(etd))

ui<-navbarPage(
  title="Hagen Cumulative Science Project I",
  fluid=TRUE,
  selected = "Home",
  theme = shinytheme("flatly"),
  
  # Hagen Cumulative Science Project I (the very first (zero) tab) 
  #is a hyperlink, however, hothing happens when pressing there
  
  tags$head(
    tags$style(HTML("@import url('https://fonts.googleapis.com/css?family=Raleway:400,600,600i');
                    }
                    "))
    ),
  
  tags$head(
    tags$style(HTML(
      "hr.style1
      {
      border-top: 1px solid #8c8b8b;
      }
      #plotting_output {
      margin: 0 10%;
      }
      #original_studies_effect_size_output {
      margin: 0 10%;
      }
      "
    ))
    ),
  
  tabPanel(title = "Home",
           h1("Hagen Cumulative Science Project I", align="center", style= "font-family: 'Raleway';font-size:46px"),
           br(),
           h3("Estimating the Reproducibility of JDM Research",align="center", style= "font-family: 'Raleway';font-size:36px"),
           br(),
           h5("Gloeckner et al.",align="center", style= "font-family: 'Raleway';font-size:20px"),
           visNetworkOutput("network", height = "300px"),
           #Info text? 
           fluidRow(
             column(width = 2),
             column(width = 8,
                    h6("", align="center", style= "font-family: 'Raleway';font-size:16px")
             ),
             column(width = 2)
           )
           
  ),
  
  
  tabPanel(title="Database",
           
           #define the placeholder for the output
           mainPanel(
             width = 12,
             DT::dataTableOutput("results")  
             # navbarMenu(title = "More",
             #            tabPanel("tab 3", "contents"),
             #            tabPanel("tab 4", "contents"),
             #            tabPanel("tab 5", "contents")
           )
  ),
  
  
  tabPanel(title="Plotting",
           mainPanel
           (
             width = 10,
             plotlyOutput("plotting_output"),   
             verbatimTextOutput("click"),
             verbatimTextOutput("brush"),
             tags$head(tags$style("#click, #brush{color: #350B0B;
                                  font-size: 14px;
                                  font-style: italic;
                                  }"))
      #verbatimTextOutput("event")
             )       
             ),
  
  tabPanel(title="Effect Size",
           mainPanel
           (
             width = 10,
             plotOutput('effect_size_output', width = 2000, height = 3500)
           )             
  ),
  
  tabPanel(title="Original Studies Effect Size",
           mainPanel
           (
             width = 10,
             plotOutput('original_studies_effect_size_output', width = 2000, height = 1300)
           )             
  ),
  
  tabPanel(title="Replicated Studies Effect Size",
           mainPanel
           (
             #width = 10,
             plotlyOutput("replicated_studies_effect_size_output"), 
             tags$head(tags$style("#click, #brush{color: #350B0B;
                                  font-size: 14px;
                                  font-style: italic;
                                  }")),
             verbatimTextOutput("click_replicated_effect_size")
           )             
  )
  
           )

server <- function(input, output, session) {
  
  
  #link to the pre-registration is 0 -> so far we are not using it: new_etd <- etd[,c(6,7,8,1,2,3,4,5,9,10,11,12,13,14,15,16,17)]
  new_etd <- etd[,c(7,1,2,3,4,5,9,10,11,12,13,14,15,16,17)]
  #print(column(etd))
  
  #filter: Remove duplicates and the added row with "...", and the chosen area of content
  filtered_content<- reactive({
    
    subset(new_etd, !duplicated(new_etd$Title) & new_etd$Title!="empty")
    
  })
  
  
  #filter the whole data (also duplicates)
  filtered_dep_measure<-reactive({
    return(filtered_content())
    
  })
  
  output$results <- renderDT({
    datatable( escape = FALSE,
               filtered_dep_measure(),
               #etd,
               extensions = c('FixedColumns','FixedHeader', 'Buttons'),
               rownames = TRUE,
               selection = 'none',
               class = 'compact nowrap row-border',
               options = list(scrollX=TRUE,scrollY="75vh",
                              autoWidth=TRUE,
                              paging=FALSE,
                              fixedHeader=TRUE,
                              buttons = list(c('copy', 'excel')),
                              dom='Bfrtip',
                              autoWidth=TRUE,
                              
                              columnDefs=list(
                                # list(width= "300px", targets=c(1,3)),
                                # list(width= "500px", targets=c(5)),
                                list(visible=FALSE, targets=c(0)),
                                list(
                                  #targets = c(3,1,4,5,6,9,10,11,12,13,14,15,16,17), change this and line 186 to see part of the table
                                  targets = c(2,3,4, 5,6,7,8,9,10,11,12,13,14,15),
                                  render = JS(
                                    "function(data, type, row, meta) {",
                                    "return type === 'display' && data.length > 40 ?",
                                    "'<span title=\"' + data + '\">' + data.substr(0, 40) + '...</span>' : data;",
                                    "}")
                                )
                              )
                              
               )          
    )
  })
  
  output$definitions<- renderUI({
    if(input$varInput != "..."){
      def<-as.character(subset(codebook,codebook$Clear.Variable.Name == input$varInput, select = "Definition"))
      HTML(paste0(h4(tags$strong(def))))
    }
    
  })
  
  
  output$plotting_output <- renderPlotly({
    plot_ly(etd, x = ~Effect.size.Original, y = ~Effect.size.Replication, marker = list(size = 12,
                                                                                        color = 'rgba(255, 182, 193, .9)',
                                                                                        line = list(color = 'rgba(152, 0, 0, .8)',
                                                                                                    width = 3))) %>%
      layout(title = 'Effect size',
             yaxis = list(zeroline = FALSE),
             xaxis = list(zeroline = FALSE))
  })
  
  # output$event <- renderPrint({
  #   d <- event_data("plotly_hover")
  #   if (is.null(d)) "Hover on a point!" else d
  # })
  
  output$click <- renderPrint({
    d <- event_data("plotly_click")
    if (is.null(d)) "Click events appear here (double-click on the empty plane to clear)" else 
    {
      d = d[,c(2,3,4)]
      names(d) <- c("Study Number","Effect size original","Effect size replication")
      d
    }
  })
  output$click_replicated_effect_size <- renderPrint({
    d <- event_data("plotly_click")
    if (is.null(d)) "Click events appear here (double-click on the empty plane to clear)" else 
    {
      #get autors name here or in the plotly
      d = d[,c(4,3)]
      d <- append(d, etd[d[1,1],c("Authors")])
      d <- as.data.frame(d)
      names(d) <- c("Study Number",
                    "Effect size replication",
                    "Authors")
      d
    }
  })
  
  output$brush <- renderPrint({
    an.error.occured <- FALSE
    tryCatch( { 
      d <- event_data("plotly_selected")
      #print(d)
      if (is.null(d)) print("Click and drag events (i.e., select/lasso) appear here (double-click to clear)") 
      else 
      {
        d = d[,c(2,3,4)]
        names(d) <- c("Study Number","Effect size original","Effect size replication")
        print(d)
      }}, error = function(e) {an.error.occured <<- TRUE})
    
    if (an.error.occured)
    {print("Choose the area with points")}
  })
  
  
  
  
  # Plotting both the original and the replicated effect size
  
  output$effect_size_output <- renderPlot({
    
    palette(c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3",
              "#FF7F00", "#FFFF33", "#A65628", "#F781BF", "#999999"))
    
    par(pin=c(15, 45))
    
    # Plotting the lowest values of the error effect size
    plot(
      low_original_effect_size,
      y=original_effect_size_y_zus,
      xlab="Effect Size",
      ylab="",
      col = "cyan",
      pch = 1, # Code of the shape of the plotting point, in this case it is a point
      las=1,
      xlim=c(0, 6),
      cex = 0.1, # Size of the plotting point
      yaxt="n",
      frame.plot=FALSE)
    par(new=TRUE)
    
    # Plotting the highest values of the error effect size
    plot(
      high_original_effect_size,
      y=original_effect_size_y_zus,
      xlab="Effect Size",
      ylab="",
      col = "cyan",
      pch = 1, # Code of the shape of the plotting point, in this case it is a point
      las=1,
      xlim=c(0, 6),
      cex = 0.1, # Size of the plotting point
      yaxt="n",
      frame.plot=FALSE)
    par(new=TRUE)
    
    # Drawing segments to match between the lowest and highest error values
    for (i in 1:length(original_effect_size_y_zus))
    {
      segments(
        low_original_effect_size[i], 
        original_effect_size_y_zus[i], 
        high_original_effect_size[i], 
        original_effect_size_y_zus[i]
      )
    }
    par(new=TRUE)
    
    # Drawing segments to match between the lowest and highest error values
    for (i in 1:length(replicated_effect_size_y_zus))
    {
      segments(
        low_replicated_effect_size[i], 
        replicated_effect_size_y_zus[i], 
        high_replicated_effect_size[i], 
        replicated_effect_size_y_zus[i],
        col = "grey"
      )
    }
    par(new=TRUE)
    
    plot(
      original_effect_size,
      y=original_effect_size_y_zus,
      xlab="Effect Size",
      ylab="",
      col = "cyan",
      pch = 20, # Code of the shape of the plotting point, in this case it is a circle
      las=1,
      xlim=c(0, 6),
      cex = 2.2, # Size of the plotting point
      yaxt="n", 
      frame.plot=FALSE)
    par(new=TRUE)
    
    points(
      replicated_effect_size, 
      replicated_effect_size_y_zus, 
      col="firebrick1",
      pch = 15, # Code of the shape of the plotting point, in this case it is a square
      cex = 1.5 # Size of the plotting point
    )
    
    # To use the author names as the axis labels.
    axis(2, at=original_effect_size_y_zus, labels=etd[,c("Authors")], las=1)
    
    # print(etd[,c("Title")])
    # print(etd[,c("Authors")])
    # print(etd[,c("Year.of.publication")])
    
  })
  
  
  
  # Plotting the original effect size
  output$original_studies_effect_size_output <- renderPlot({
    
    palette(c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3",
              "#FF7F00", "#FFFF33", "#A65628", "#F781BF", "#999999"))
    
    par(pin=c(10, 15))
    
    # Plotting the lowest values of the error effect size
    plot(
      low_original_effect_size,
      y=original_effect_size_y,
      xlab="Effect Size",
      ylab="",
      col = "black",
      pch = 1, # Code of the shape of the plotting point, in this case it is a point
      las=1,
      xlim=c(0, 6),
      cex = 0.1, # Size of the plotting point
      yaxt="n",
      frame.plot=FALSE)
    par(new=TRUE)
    
    # Plotting the highest values of the error effect size
    plot(
      high_original_effect_size,
      y=original_effect_size_y,
      xlab="Effect Size",
      ylab="",
      col = "black",
      pch = 1, # Code of the shape of the plotting point, in this case it is a point
      las=1,
      xlim=c(0, 6),
      cex = 0.1, # Size of the plotting point
      yaxt="n",
      frame.plot=FALSE)
    par(new=TRUE)
    
    # Drawing segments to match between the lowest and highest error values
    for (i in 1:length(replicated_effect_size))
    {
      segments(low_original_effect_size[i], i, high_original_effect_size[i], i)
    }
    par(new=TRUE)
    
    plot(
      original_effect_size,
      original_effect_size_y,
      xlab="Effect Size",
      ylab="",
      col = "cyan",
      pch = 20, # Code of the shape of the plotting point, in this case it is a circle
      las=1,
      xlim=c(0, 6),
      cex = 2.2, # Size of the plotting point
      yaxt="n",
      frame.plot=FALSE)
    
    # To use the author names as the axis labels.
    axis(2, at=1:length(original_effect_size), labels=etd[,c("Authors")], las=1)
    
  })
  
  
  
  # Plotting the replicated effect size
  output$replicated_studies_effect_size_output <- renderPlotly({
    
 
    #Plotting the lowest values of the error effect size
    # plot(
    #   low_replicated_effect_size,
    #   y=replicated_effect_size_y,
    #   xlab="Effect Size",
    #   ylab="",
    #   col = "grey",
    #   pch = 1, # Code of the shape of the plotting point, in this case it is a point
    #   las=1,
    #   xlim=c(0, 6),
    #   cex = 0.1, # Size of the plotting point
    #   yaxt="n",
    #   frame.plot=FALSE)
    # par(new=TRUE)

    # Plotting the highest values of the error effect size
    # plot(
    #   high_replicated_effect_size,
    #   y=replicated_effect_size_y,
    #   xlab="Effect Size",
    #   ylab="",
    #   col = "grey",
    #   pch = 1, # Code of the shape of the plotting point, in this case it is a point
    #   las=1,
    #   xlim=c(0, 6),
    #   cex = 0.1, # Size of the plotting point
    #   yaxt="n",
    #   frame.plot=FALSE)
    # par(new=TRUE)

    # Drawing segments to match between the lowest and highest error values
    # for (i in 1:length(replicated_effect_size))
    # {
    #   segments(low_replicated_effect_size[i], i, high_replicated_effect_size[i], i, col = "grey")
    # }
    # par(new=TRUE)
    
    plot_ly(etd,
            x=~Effect.size.Replication,
            y=~replicated_effect_size_y,
            #z = ~Authors,
            marker = list(size = 12,
                      color = 'rgba(255, 182, 193, .9)',
                      line = list(color = 'rgba(152, 0, 0, .8)',
                                width = 3))) %>%
    layout(title = 'Replicated studies effect size',
             yaxis = list(
                title = "Study number",
                #labels = etd[,c("Authors")],
                zeroline = FALSE
             ),
             xaxis = list(title = "Effect Size Replication", zeroline = FALSE)
           #,autosize = F
           )

    # To use the author names as the axis labels.
    #axis(2, at=1:length(replicated_effect_size), labels=etd[,c("Authors")], las=1)

  })
  
}

#Run the application
shinyApp(ui = ui, server = server)