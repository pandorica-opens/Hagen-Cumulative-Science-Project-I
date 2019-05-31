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
             plotlyOutput('effect_size_output')
           )             
  ),
  
  tabPanel(title="Original Studies Effect Size",
           mainPanel
           (
             width = 10,
             plotlyOutput('original_studies_effect_size_output')
           )             
  ),
  
  tabPanel(title="Replicated Studies Effect Size",
           mainPanel
           (
             width = 10,
             height = 500,
             plotlyOutput("replicated_studies_effect_size_output"), 
             verbatimTextOutput("click_replicated_effect_size"),
             tags$head(tags$style("#click_replicated_effect_size, #brush{color: #350B0B;
                                  font-size: 14px;
                                  font-style: italic;
                                  }"))
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
  
  #plot for Plotting tab panel
  output$plotting_output <- renderPlotly({
    plot_ly(etd, x = ~Effect.size.Original, y = ~Effect.size.Replication, type="scatter",
            mode='markers', marker = list(size = 12,color = 'rgba(255, 182, 193, .9)',
                          line = list(color = 'rgba(152, 0, 0, .8)',width = 3))) %>%
      layout(title = 'Effect size',
             yaxis = list(zeroline = FALSE),
             xaxis = list(zeroline = FALSE))
  })
  
  # output$event <- renderPrint({
  #   d <- event_data("plotly_hover")
  #   if (is.null(d)) "Hover on a point!" else d
  # })
  
  #click response for Plotting tab panel
  output$click <- renderPrint({
    d <- event_data("plotly_click")
    if (is.null(d)) "Click events appear here (double-click on the empty plane to clear)" else 
    {
      d = d[,c(2,3,4)]
      names(d) <- c("Study Number","Effect size original","Effect size replication")
      d
    }
  })
  
  #click response for Replicated Studies Effect size tab panel
  output$click_replicated_effect_size <- renderPrint({
    d <- event_data("plotly_click")
    if (is.null(d)) "Click events appear here (double-click on the empty plane to clear)" else
    {
      #get autors name here or in the plotly
      d = d[,c(3,4)]
      d <- append(d, etd[d[1,1],c("Authors")])
      d <- as.data.frame(d)
      names(d) <- c("Study Number",
                     "Effect size replication",
                     "Authors")
      d
    }
  })
  
  #select/lasso response for Plotting tab panel
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
  
  output$effect_size_output <- renderPlotly({
    
    p<-plot_ly(etd,
               y=~c(low_original_effect_size, high_original_effect_size, original_effect_size),
               x=~c(original_effect_size_y, original_effect_size_y, original_effect_size_y),
               type="scatter",
               mode='lines+markers',
               marker = list(size = 12,
                             color = 'rgba(0, 182, 193, .9)',
                             line = list(color = 'rgba(0, 0, 0, .8)',
                                         width = 3)),
               split=~c(original_effect_size_y,original_effect_size_y,original_effect_size_y))%>%
    
    add_trace(y=~c(Effect.size.Replication, high_replicated_effect_size, low_replicated_effect_size),
              x=~c(original_effect_size_y, original_effect_size_y, original_effect_size_y),
              type = "scatter", mode = "markers+lines", marker = list(size = 12,
              color = 'rgba(255, 182, 193, .9)',line = list(color = 'rgba(152, 0, 0, .8)', width = 3)))
      
    
    layout(p,
      title = 'Both the original and the replicated effect size',
             xaxis = list(
               title = "Study number",
               #labels = etd[,c("Authors")],
               zeroline = FALSE
             ),
             #height = 500,
             yaxis = list(title = "Effect size", zeroline = FALSE),
             showlegend=F
             #autosize = F
    )
    
  })
  
  
  
  # Plotting the original effect size
  output$original_studies_effect_size_output <- renderPlotly({
    
    p<-plot_ly(etd,
               y=~c(low_original_effect_size, high_original_effect_size, original_effect_size),
               x=~c(original_effect_size_y, original_effect_size_y, original_effect_size_y),
               type="scatter",
               mode='lines+markers',
               marker = list(size = 12,
                             color = 'rgba(255, 182, 193, .9)',
                             line = list(color = 'rgba(152, 0, 0, .8)',
                                         width = 3)),
               split=~c(original_effect_size_y,original_effect_size_y,original_effect_size_y))
    
    
    p=layout(p, title = 'Original studies effect size',
             xaxis = list(
               title = "Study number",
               #labels = etd[,c("Authors")],
               zeroline = FALSE
             ),
             #height = 500,
             yaxis = list(title = "Original studies effect size", zeroline = FALSE),
             showlegend=F
             #autosize = F
    )
    # # To use the author names as the axis labels.
    # axis(2, at=1:length(original_effect_size), labels=etd[,c("Authors")], las=1)
    
    
  })
  
  
  
  # Plotting the replicated effect size
  output$replicated_studies_effect_size_output <- renderPlotly({
    
    p<-plot_ly(etd,
               y=~c(Effect.size.Replication, high_replicated_effect_size, low_replicated_effect_size),
               x=~c(replicated_effect_size_y, replicated_effect_size_y, replicated_effect_size_y),
               type="scatter",
               mode='lines+markers',
               marker = list(size = 12,
                             color = 'rgba(255, 182, 193, .9)',
                             line = list(color = 'rgba(152, 0, 0, .8)',
                                         width = 3)),
               split=~c(replicated_effect_size_y,replicated_effect_size_y,replicated_effect_size_y))

    
    p=layout(p, title = 'Replicated studies effect size',
             xaxis = list(
               title = "Study number",
               #labels = etd[,c("Authors")],
               zeroline = FALSE
             ),
             #height = 500,
             yaxis = list(title = "Effect Size Replication", zeroline = FALSE),
             showlegend=F
             #autosize = F
             )
    
    print(p)
    
  })
  
}

#Run the application
shinyApp(ui = ui, server = server)