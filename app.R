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
library(profvis)
library(shinycssloaders)

#load the test dataset from the google sheets document
url <- 'https://docs.google.com/spreadsheets/d/1VBnEGbanAt5yLXkpMGRGJWZeuw1TLNVudksgXyksHq0/edit?ts=5beae9fc#gid=0'

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

titles <- etd[,c("Title")]

original_effect_size_y_zus = seq(from=2, to=length(original_effect_size)*3+1, by=3)
replicated_effect_size_y_zus = seq(from=1, to=length(replicated_effect_size)*3, by=3)

original_effect_size_y = seq(1, length(original_effect_size))
replicated_effect_size_y = seq(1, length(replicated_effect_size))
study_numbers = seq(1, length(titles))

max_original_replication = max(original_effect_size, replicated_effect_size)

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
           h3("Estimating the Reproducibility of JDM Research", align="center", style= "font-family: 'Raleway';font-size:36px"),
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
           #<div class="loader">Loading...</div>,
           
           mainPanel(
             width = 12,
             withSpinner(
               DT::dataTableOutput("results"),
               type=2, color='#2B3E50', size = 2,
               color.background='#eef2f6')   
           )
  ),
  
  
  tabPanel(title="Plotting", 
           mainPanel
           (
             width = 12,
             withSpinner(
               div(plotlyOutput("plotting_output3"), align = 'center'), type=2, color='#2B3E50', size = 2,
               color.background='#eef2f6'),
             # verbatimTextOutput("click"),
             verbatimTextOutput("brush"),
             DT::dataTableOutput("table_plotting"),
             tags$head(tags$style("#click, #brush{color: #350B0B;
                                  font-size: 14px;
                                  font-style: italic;
                                  }"))
             )
             ),
  
  
  tabPanel(title="Effect Size",
           mainPanel
           (
             width = 12,
             withSpinner(div(plotlyOutput("effect_size_output"), align = 'center'), type=2, color='#2B3E50', size = 2,
                         color.background='#eef2f6'),
             verbatimTextOutput("brush_effect"),
             DT::dataTableOutput("table_effect"),
             tags$head(tags$style("#brusheffect{color: #350B0B;
                                  font-size: 14px;
                                  font-style: italic;
                                  }"))
           )
           
             ),
  
  tabPanel(title="Original Studies Effect Size",
           mainPanel
           (
             width = 12,
             withSpinner(
               div(plotlyOutput("original_studies_effect_size_output2"), align = 'center'), 
               type=2, color='#2B3E50', size = 2,
               color.background='#eef2f6'), 
             verbatimTextOutput("brush_original"),
             DT::dataTableOutput("table_orig_effect")
           )
  ),
  
  tabPanel(title="Replicated Studies Effect Size",
           
           mainPanel
           (
             width = 12,
             withSpinner(
               div(plotlyOutput("replicated_studies_effect_size_output"), align = 'center'),
               type=2, color='#2B3E50', size = 2,
               color.background='#eef2f6'), 
             verbatimTextOutput("brush_replicated"),
             DT::dataTableOutput("table_repl_effect"),
             
             tags$head(tags$style("#brush_replicated, #brush{color: #350B0B;
                                  font-size: 14px;
                                  font-style: italic;
                                  }"))
             
             
             )
             )
  ) 
server <- function(input, output, session) {
  
  #link to the pre-registration is 0 -> so far we are not using it: new_etd <- etd[,c(6,7,8,1,2,3,4,5,9,10,11,12,13,14,15,16,17)]
  new_etd <- etd[,c(7,13,14,15,16,17,20,21,22,1,3)]
  #row.names(new_etd) <- study_numbers
  new_etd = cbind(id = study_numbers, new_etd)
  
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
               #filter = 'top',
               #etd,
               extensions = c('FixedColumns','FixedHeader', 'Buttons', 'Responsive'),
               #rownames = TRUE,
               #rownames = study_numbers,
               selection = 'multiple', 
               #selection = 'none',
               colnames=c("Study<br/>number","Links","N <br/>original", "p-value<br/>original", 
                          "Effect size<br/>original", "CI low<br/>original", "CI high<br/>original", 
                          "Effect size<br/>replication", "CI low<br/>replication",
                          "CI high <br/> replication",  "Title", "Authors"),
               class = 'compact nowrap row-border',
               options = list(#scrollX=TRUE, scrollY="75vh",
                              #autoWidth=TRUE,
                              paging=FALSE,
                              fixedHeader=TRUE,
                              buttons = list(c('copy', 'excel')),
                              dom='Bfrtip',
                              columnDefs=list(
                                # list(width= "300px", targets=c(1,3)),
                                # list(width= "500px", targets=c(5)),
                                list(visible=FALSE, targets=c(0)),
                                list(
                                  #targets = c(3,1,4,5,6,9,10,11,12,13,14,15,16,17), change this and line 186 to see part of the table
                                  targets = c(2,3,4,5,6,7,8,9,10,11),
                                  render = JS(
                                    "function(data, type, row, meta) {",
                                    "return type === 'display' && data.length > 300 ?",
                                    "'<span title=\"' + data + '\">' + data.substr(0, 300) + '...</span>' : data;",
                                    "}")
                                  
                                )
                              )
                              # ,
                              # initComplete = JS(
                              #   "function(settings, json) {",
                              #   "$(this.api().table().header()).css({'background-color': '#2B3E50', 'color': '#fff'});",
                              #   "}")
                              
               )          
    )
  })
  
  output$definitions<- renderUI({
    if(input$varInput != "..."){
      def<-as.character(subset(codebook,codebook$Clear.Variable.Name == input$varInput, select = "Definition"))
      HTML(paste0(h4(tags$strong(def))))
    }
    
  })
  
  
  output$plotting_output3 <- renderPlotly({
    
    p<-plot_ly(etd,
               x = ~Effect.size.Original, 
               y = ~Effect.size.Replication,
               type="scatter",
               name = "Effect sizes",
               mode='markers', marker = list(size = 20, color = ~c(original_effect_size_y),
                                             opacity = 0.7,
                                             line = list(color = 'rgba(152, 0, 0, .8)',width = 3)),
               width = 1200, height = 400
    )%>%
      add_trace(y=~c(0, max_original_replication),
                x=~c(0, max_original_replication),
                type = "scatter", mode = "markers+lines",
                name = "45 degree line",
                marker = list(size = 0.3, color = c("#66ffc2"),
                              line = list(color = 'rgba(43, 62, 80, .8)', width = 3))
      ) %>%
      add_trace(p, y=~c(0, 0),
                x=~c(0, max_original_replication),
                name = "dotted line",
                line = list(color = 'rgb(205, 12, 24)', width = 4, dash = 'dash'),
                marker = list(size = 0.3, color = c("#66ffc2"),
                              line = list(color = 'rgba(43, 62, 80, .8)', width = 3)))
    
    p=layout(p, title = 'Effect size',
             yaxis = list(title = 'Effect size Original', zeroline = FALSE),
             xaxis = list(title = 'Effect size Replication', zeroline = FALSE), 
             autosize = F)
    
    print(p)
    
  })

  
  #select/lasso response for Plotting tab panel
  output$brush <- renderPrint({
    d <- event_data("plotly_selected")
    if (is.null(d)) 
    {
      print("Click and drag events (i.e., select/lasso) appear here (double-click to clear)")
    }
    else if (length(d)==0)
    {print("Choose the area with points")}})
  
  #DT for Plotting tab
  output$table_plotting = DT::renderDataTable({
    d <- event_data("plotly_selected")
    if (length(d)!=0) {
    if (any(d[c(2)]))
    {
      d <- d[c(2,3,4)]
      names(d) <- c("Study Number","Effect size original","Effect size replication") #study number =2
      d
    }
    else
    { 
      d <- NULL
      #print "Click and drag events (i.e., select/lasso) appear here (double-click to clear)"
      # study number, lots of 0, study number +1, original OR replicated effect size, we are not 
      # sure which one, choice is happening through the plot
      # hence, when we are going from effect size to plotting tab, or from original/replicated studies
      # it is always different
      # maybe find the way to unselect or put a disclaimer
    }
    }
  })
  
  
  #select/lasso response for Effect size tab panel
  output$brush_effect <- renderPrint({
    d2 <- event_data("plotly_selected")
    if (is.null(d2)) 
    {
      print("Click and drag events (i.e., select/lasso) appear here (double-click to clear) null")
    }
    else if (length(d2)==0)
    {print("Choose the area with points")}})
  
  output$table_effect = DT::renderDataTable({
    d <- event_data("plotly_selected")
    if (length(d)!=0) {
      study_number = d[,c(3)]
      d = as.data.table(c(etd[study_number,c(3, 15, 16, 17, 20, 21, 22)])) #study authors extra
      names(d) <- c("Study Authors", "Effect size Original","CI low Original","CI high Original",
                    "Effect size Replication", "CI low Replication", "CI high Replication")
      d
    }
  })
  
  #select/lasso response for Original studies tab panel
  output$brush_original <- renderPrint({
    d3 <- event_data("plotly_selected")
    if (is.null(d3)) 
    {
      print("Click and drag events (i.e., select/lasso) appear here (double-click to clear) null")
    }
    else if (length(d3)==0)
    {print("Choose the area with points")}
    })
  
  output$table_orig_effect = DT::renderDataTable({
    d <- event_data("plotly_selected")
    if (length(d)!=0) {
      study_number = d[,c(3)]
      d = as.data.table(c(etd[study_number,c(15, 16, 17)]))
      names(d) <- c("Effect size Original","CI low Original","CI high Original")
      d
    }
  })
  
  #select/lasso response for Replicated studies tab panel
  output$brush_replicated <- renderPrint({
      d4 <- event_data("plotly_selected")
      if (is.null(d4)) 
      {
        print("Click and drag events (i.e., select/lasso) appear here (double-click to clear) null")
      }
      else if (length(d4)==0)
      {print("Choose the area with points")}})
  
  output$table_repl_effect = DT::renderDataTable({
    d <- event_data("plotly_selected")
    if (length(d)!=0) {
      study_number = d[,c(3)]
      d = as.data.table(c(etd[study_number,c(20, 21, 22)]))
      names(d) <- c("Effect size Replication", "CI low Replication", "CI high Replication")
      d
    }
  })
  
  
  output$effect_size_output <- renderPlotly({
    
    p<-plot_ly(etd,
               y=~c(original_effect_size),
               x=~c(original_effect_size_y),
               type="scatter", 
               mode="markers+lines",
               marker = list(size = 12, color = c("#ccebff", "#ffcccc" , "#ccffeb"), #can also change size to 20
                             #opacity = 0.9,
                             line = list(color = 'rgba(43, 62, 80, .8)',
                                         width = 3)),
               #line = list(color = 'rgba(43, 62, 80, .8)', width = 2),
               split=~c(original_effect_size_y),
               width = 1200, height = 400,
               
               error_y = list(type = "data",
                              symmetric = FALSE,
                              array = ~c(high_original_effect_size-original_effect_size), #, high_replicated_effect_size-replicated_effect_size),
                              arrayminus = ~c(original_effect_size-low_original_effect_size))) %>% #, replicated_effect_size-low_replicated_effect_size)))
      
      
      add_trace(y=~c(Effect.size.Replication),
                x=~c(original_effect_size_y)+0.1,
                type = "scatter", mode = "markers+lines",
                error_y = list(type = "data",
                               symmetric = FALSE,
                               array = ~c(high_replicated_effect_size-replicated_effect_size), #, high_replicated_effect_size-replicated_effect_size),
                               arrayminus = ~c(replicated_effect_size-low_replicated_effect_size)),
                marker = list(size = 12, color = c("#66ffc2", "#ff8080" , "#66c2ff"),
                              line = list(color = 'rgba(43, 62, 80, .8)', width = 3)))
    
    
    
    layout(p, title = 'Both the original and the replicated effect size',
           xaxis = list(
             title = "Study number",
             zeroline = FALSE
           ),
           yaxis = list(title = "Effect size", zeroline = FALSE),
           showlegend=F,
           autosize = F
    )
    
  })
  
  
  
  # Plotting the original effect size
  
  output$original_studies_effect_size_output2 <- renderPlotly({
    
    p<-plot_ly(etd,
               y=~c(original_effect_size),
               x=~c(original_effect_size_y),
               type="scatter",
               mode='lines+markers',
               marker = list(size = 12,
                             color = c("#ccebff", "#ffcccc" , "#ccffeb"),
                             line = list(color = 'rgba(43, 62, 80, .8)',
                                         width = 3)),
               line = list(color = 'rgba(43, 62, 80, .8)', width = 2),
               error_y = list(type = "data",
                              symmetric = FALSE,
                              array = c(high_original_effect_size-original_effect_size),
                              arrayminus = c(original_effect_size-low_original_effect_size)),
               split=~c(original_effect_size_y),
               width = 1200, height = 400
    )
    
    
    p=layout(p, title = 'Original studies effect size',
             xaxis = list(
               title = "Study number",
               #labels = etd[,c("Authors")],
               zeroline = FALSE
             ),
             yaxis = list(title = "Effect Size Original", zeroline = FALSE),
             showlegend=F,
             autosize = F
    )
    
    print(p)
    
  })
  
  # Plotting the replicated effect size
  output$replicated_studies_effect_size_output <- renderPlotly({
    
    p<-plot_ly(etd,
               y=~c(Effect.size.Replication),
               x=~c(replicated_effect_size_y),
               type="scatter",
               mode='lines+markers',
               marker = list(size = 12,
                             color = c("#66ffc2", "#ff8080" , "#66c2ff"),
                             line = list(color = 'rgba(43, 62, 80, .8)',
                                         width = 3)),
               error_y = list(type = "data",
                              symmetric = FALSE,
                              array = c(high_replicated_effect_size-replicated_effect_size),
                              arrayminus = c(replicated_effect_size-low_replicated_effect_size)),
               line = list(color = 'rgba(43, 62, 80, .8)', width = 2),
               split=~c(replicated_effect_size_y),
               width = 1200, height = 400
    )
    
    
    p=layout(p, title = 'Replicated studies effect size',
             xaxis = list(
               title = "Study number",
               #labels = etd[,c("Authors")],
               zeroline = FALSE
             ),
             yaxis = list(title = "Effect Size Replication", zeroline = FALSE),
             showlegend=F,
             autosize = F
    )
    
    print(p)
    
  })
  
  
  
}

#Run the application
shinyApp(ui = ui, server = server)
