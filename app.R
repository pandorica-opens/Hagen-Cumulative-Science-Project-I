
library(shiny)
library(dplyr)
library(ggvis)
library(gsheet)
library(DT)
library(data.table)
library(rsconnect)
#library(questionr)
#library(ggplot2)
library(shinythemes)
library(visNetwork)
library(stringr)

#load the dataset from the google sheets document
url <- 'https://docs.google.com/spreadsheets/d/1VBnEGbanAt5yLXkpMGRGJWZeuw1TLNVudksgXyksHq0/edit?ts=5beae9fc#gid=0'
etd <- as.data.frame(read.csv(text = gsheet2text(url, format = 'csv'), stringsAsFactors = FALSE))

#add clickable icons with link to the paper
link_icon<-as.character(img(src="pdf-3.png", height="13px", width="13px"))
etd$Link.to.the.paper <- paste0("<a href='",etd$Link.to.the.paper,"' target='_blank'>",link_icon,"</a>")
etd$Link.to.the.OSF.Project <- paste0("<a href='",etd$Link.to.the.OSF.Project,"' target='_blank'>",link_icon,"</a>")

#print(etd$Link.to.the.paper)
#authors <- etd[,c("Auhors")]

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

  # problem with Database:
  # 1)the table is too wide, but the titles are still not shown fully
  # 2) maybe make the titles in 2 rows (so all the titled would be seen) + Year + Authors
  # everything else would be in the external bar
  # 3) look up completely different layout for the google docs file

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

  # 1) figure out the logic of the prot and nice way of presenting it
  #
  #
  
  tabPanel(title="Plotting",
    sidebarPanel(
      sliderInput("Effect.size.Replication", "Effect Size Replication", 0.0, 1.0, value = c(0.0, 1.0),
                  sep = ""),
      # those variables - "Effect.size.Replication" - "Effect.size.Original" 
      # at this point doesn't mean anything because they are not reactive, thus the plot is 
      # not getting changed after we change the variable; we have to do it from server side to get a
      # respond here
      # also good idea to get the values by giving a max and min of the measuarements, and
      # not the fixed values like 0.0, 4.9, 0.0, 1.0
      
      sliderInput("Effect.size.Original", "Effect Size Original", 0.0, 4.9, value = c(0.0, 1.5),
                  sep = "")
    ),
    mainPanel
    (
     width = 7,
     plotOutput('plotting_output', width = "100%", height = 550)
    )       
  #   titlePanel("Movie explorer"),
  #   fluidRow(
  #     column(3,
  #            wellPanel(
  #              h4("Filter"),
  #              sliderInput("reviews", "Minimum number of reviews on Rotten Tomatoes",
  #                          10, 300, 80, step = 10),
  #            
  #     column(9,
  #            ggvisOutput('plotting_output'),
  #            wellPanel(
  #              span("Number of movies selected:",
  #                   textOutput("n_movies")
  #              )
  #            )
  #     )
  #            )
  #     ) 
  # ) 
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
      width = 10,
      plotOutput('replicated_studies_effect_size_output', width = 2000, height = 1300)
    )             
  )
  
)

server <- function(input, output, session) {
  
  #filter: Remove duplicates and the added row with "...", and the chosen area of content
  
  # when is this reactive value used???
  #new_etd <- etd [,c("Title","Link.to.the.paper") ]
  new_etd <- etd
  #print(new_etd["Link.to.the.paper"])
  
  # filtered_content<- reactive({
  # 
  #   subset(etd, !duplicated(etd$Title) & etd$Title!="empty")
  # 
  # })
  
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
                         targets = c(1,3,4,5,6,9,10,11,12,13,14,15,16,17), #change this and line 186 to see part of the table
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
  
  
  
  output$plotting_output <- renderPlot({
    
    palette(c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3",
      "#FF7F00", "#FFFF33", "#A65628", "#F781BF", "#999999"))

    par(pin=c(7, 7))
    plot(
      etd[,c("Effect.size.Original")],
      etd[,c("Effect.size.Replication")],
      xlab="Effect Size Original",
      ylab="Effect Size Replication",
      col = "blue",
      pch = 20, 
      cex = 3)
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
  output$replicated_studies_effect_size_output <- renderPlot({
    
    palette(c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3",
      "#FF7F00", "#FFFF33", "#A65628", "#F781BF", "#999999"))
    
    par(pin=c(10, 15))

    # Plotting the lowest values of the error effect size
    plot(
      low_replicated_effect_size,
      y=replicated_effect_size_y,
      xlab="Effect Size",
      ylab="",
      col = "grey",
      pch = 1, # Code of the shape of the plotting point, in this case it is a point
      las=1,
      xlim=c(0, 6),
      cex = 0.1, # Size of the plotting point
      yaxt="n",
      frame.plot=FALSE)
    par(new=TRUE)
    
    # Plotting the highest values of the error effect size
    plot(
        high_replicated_effect_size,
        y=replicated_effect_size_y,
        xlab="Effect Size",
        ylab="",
        col = "grey",
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
      segments(low_replicated_effect_size[i], i, high_replicated_effect_size[i], i, col = "grey")
    }
    par(new=TRUE)

    plot(
        replicated_effect_size,
        replicated_effect_size_y,
        xlab="Effect Size",
        ylab="",
        col = "firebrick1",
        pch = 15, # Code of the shape of the plotting point, in this case it is a square
        las=1,
        xlim=c(0, 6),
        cex = 1.5, # Size of the plotting point
        yaxt="n",
        frame.plot=FALSE)
    
    # To use the author names as the axis labels.
    axis(2, at=1:length(replicated_effect_size), labels=etd[,c("Authors")], las=1)

  })
 
}

#Run the application
shinyApp(ui = ui, server = server)



