#Load in Libraries
library(tidyverse)
library(plotly)
library(reshape2)
library(viridis)
library(shiny)



ui <- fluidPage(
  column(align = "center",titlePanel("Group 1"), width = 7,style='padding-left:350px;',style = 'width:1450px;'),
  fluidRow(div(column(plotlyOutput("o1"), width = 4),
           column(plotlyOutput("g1p1"), width = 4),
           column(plotlyOutput("g1p2"), width = 4), 
           column(plotlyOutput("g1p3"), width = 4),
           column(plotlyOutput("g1p4"), width = 4),
           column(plotlyOutput("g1p5"), width = 4), style = 'width:1450px;', align = "center", style='padding-left:350px;')
  ),
  column(7,align = "center",titlePanel("Group 2"),style='padding-left:350px;',style = 'width:1450px;'),
  fluidRow(div(column(plotlyOutput("o2"), width = 4),
           column(plotlyOutput("g2p1"), width = 4),
           column(plotlyOutput("g2p2"), width = 4),
           column(plotlyOutput("g2p3"), width = 4),
           column(plotlyOutput("g2p4"), width = 4), style = 'width:1450px;', align = "center", style='padding-left:350px;')
  ),
  column(7,align = "center",titlePanel("Group 3"),style='padding-left:350px;',style = 'width:1450px;'),
  fluidRow(div(column(plotlyOutput("o3"), width = 4),
         column(plotlyOutput("g3p1"), width = 4),
         column(plotlyOutput("g3p2"), width = 4),
         column(plotlyOutput("g3p3"), width = 4),
         column(plotlyOutput("g3p4"), width = 4),
         column(plotlyOutput("g3p5"), width = 4), style = 'width:1450px;', align = "center", style='padding-left:350px;')
  ),
  column(7,align = "center",titlePanel("Group 4"),style='padding-left:350px;',style = 'width:1450px;'),
  fluidRow(div(column(plotlyOutput("o4"), width = 4),
           column(plotlyOutput("g4p1"), width = 4),
           column(plotlyOutput("g4p2"), width = 4),
           column(plotlyOutput("g4p3"), width = 4), style = 'width:1450px;', align = "center", style='padding-left:350px;')
  ),
  column(7,align = "center",titlePanel("Group 5"),style='padding-left:350px;',style = 'width:1450px;'),
  fluidRow(div(column(plotlyOutput("g5p1"), width = 4), style = 'width:1450px;', align = "center", style='padding-left:350px;')
  )
)

server <- shinyServer(function(input, output, session) {

  #Loaded in data
  path <- 'https://raw.githubusercontent.com/artofstat/ArtistDiversity/master/artistdata.csv'
  artists <- read.csv(path)
  
  #Calculate percentages
  geodf <- artists %>% select(museum, GEO3major) %>% group_by(museum) %>% 
    summarize(Africa=round(100*prop.table(table(GEO3major))[1],1), 
              Asia=round(100*prop.table(table(GEO3major))[2],1), 
              Europe=round(100*prop.table(table(GEO3major))[3],1), 
              "Latin America"=round(100*prop.table(table(GEO3major))[4],1), 
              "North America"=round(100*prop.table(table(GEO3major))[5],1), 
              "West Asia"=round(100*prop.table(table(GEO3major))[6],1)
    )
  
  Group <- c(2,3,3,1,3,3,1,4,1,3,4,1,2,1,2,4,5,2)
  
  geodf <- cbind(Group, geodf)
  
  geodf2 <- geodf
  
  #recode to fit name on plot
  levels(geodf$museum)[levels(geodf$museum) == "Metropolitan Museum of Art, New York, NY"]<- "Metropolitan Museum of Art, New York"
  
  #Convert to long format
  art_long <- melt(geodf, id.vars = c("Group","museum"),value.name="Percent", variable.name = "Region") %>%
    arrange(museum)
  
  #Generate donut plots for given group number
  donut_plot <- function(Group_n){
    p <- art_long %>%
      filter(Group == Group_n)%>%
      arrange(factor(Region, levels =  c("Europe",
                                         "North America",
                                         "Asia",
                                         "Latin America",
                                         "Africa",
                                         "West Asia")))
    colors3 <- viridis(6, alpha = .75, begin = 0, end = 1)
    M_list <- distinct(p, museum)
    
    plot_vec <- list()
    
    for (i in seq(1,nrow(M_list))){
      x <- filter(p, museum == M_list[i,])
      k <- x %>% plot_ly(sort = FALSE,hovertext = paste("<b>Region:<b>",x$Region,"<br>Percent:",x$Percent,"%"),
                         hoverlabel =list(font = list(size = 12)),
                         labels = ~Region, values = ~Percent, 
                         textinfo = paste(x$Percent), hoverinfo = "text",
                         insidetextfont = list(color = '#FFFFFF'),
                         marker = list(colors=colors3),
                         height = 400
      ) %>%
        add_pie(hole = 0.6) %>%
        layout(title = '') %>%
        layout(plot_bgcolor='rgb(0, 0,0,0)') %>% 
        layout(paper_bgcolor='rgb(0, 0,0,0)') %>%
        layout(margin = list(t = 45, r = 35),annotations = list(text = as.character(x$museum),showarrow = FALSE, align = 'right', y = 1, font = list(size = 14), yref = "paper"))%>%
        layout(legend = list(x = 0.4,y = 0.5, align = 'right'))%>%
        config(collaborate = FALSE, 
               displaylogo = FALSE, modeBarButtonsToRemove = list("resetScale2d", 
                                                                  "sendDataToCloud", "zoom2d", "zoomIn2d", "zoomOut2d", 
                                                                  "pan2d", "select2d", "lasso2d", "hoverClosestCartesian", 
                                                                  "hoverCompareCartesian", "hoverClosestGl2d", "hoverClosestPie", 
                                                                  "toggleHover", "resetViews", "toggleSpikelines"))
      plot_vec <- append(plot_vec, list(k))
    }
    return(plot_vec)
  }
  
  #Weighted Average
  
  group_key <- as.data.frame(geodf2[,1:2]) %>% 
    merge(artists) %>%
    select(museum,Group,GEO3major) %>%
    group_by(museum,Group)%>%
    summarise(museumTotal =n())
  
  group_total <- group_key %>%
    group_by(Group)%>%
    summarise(groupTotal = sum(museumTotal))
  
  
  group_merge <- merge(group_key,group_total)
  
  weighted <- merge(group_merge, geodf2)
  
  Group <- weighted[,1]
  groupTotal <- weighted[,4]
  weighted$inverse_ni <- 1/weighted[,3]
  
  x <- weighted %>% group_by(Group) %>% mutate(inversetotal = sum(inverse_ni), st.weight = inverse_ni/inversetotal)
  x %>% filter(Group == 1)
  dim(x)
  
  x1 <- x %>% select(Africa, Asia, Europe,'Latin America', "North America",'West Asia', st.weight) 
  
  weights <- as.matrix(x1[,2:8])
  props <- weights[,1:6]*weights[,7]
  group <- as.matrix(x1[,1])
  
  weights <- as.data.frame(cbind(group,props))
  weights <- weights %>% group_by(Group)  %>%
    summarise_each(funs(sum))
  
  weight_long <- melt(weights, id.vars ="Group",value.name="Percent", variable.name = "Region")
  
  weight_plot <- function(Group_n){
    p <- weight_long %>%
      filter(Group == Group_n)%>%
      arrange(factor(Region, levels =  c("Europe",
                                         "North America",
                                         "Asia",
                                         "Latin America",
                                         "Africa",
                                         "West Asia")))
    colors3 <- viridis(6, alpha = .75, begin = 0, end = 1)
    
      k <- p %>% plot_ly(sort = FALSE,hovertext = paste("<b>Region:<b>",p$Region,"<br>Percent:",round(p$Percent,2),"%"),
                         hoverlabel =list(font = list(size = 12)),
                         labels = ~Region, values = ~Percent, 
                         textinfo = paste(x$Percent), hoverinfo = "text",
                         insidetextfont = list(color = '#FFFFFF'),
                         marker = list(colors=colors3),
                         height = 400
      ) %>%
        add_pie(hole = 0.6) %>%
        layout(title = '') %>%
        layout(plot_bgcolor='rgb(0, 0,0,0)') %>% 
        layout(paper_bgcolor='rgb(0, 0,0,0)') %>%
        layout(margin = list(t = 45, r = 35),annotations = list(text = "Overall",showarrow = FALSE, align = 'right', y = 1, font = list(family = "sans serif",size = 17), yref = "paper"))%>%
        layout(legend = list(x = 0.4,y = 0.5, align = 'right'))%>%
        config(collaborate = FALSE, 
               displaylogo = FALSE, modeBarButtonsToRemove = list("resetScale2d", 
                                                                  "sendDataToCloud", "zoom2d", "zoomIn2d", "zoomOut2d", 
                                                                  "pan2d", "select2d", "lasso2d", "hoverClosestCartesian", 
                                                                  "hoverCompareCartesian", "hoverClosestGl2d", "hoverClosestPie", 
                                                                  "toggleHover", "resetViews", "toggleSpikelines"))
    
    return(k)
  }
  
  
  
  
  #Group 1
  
  plot_vec <- donut_plot(1)
  overall_1 <- weight_plot(1)
  
  output$o1   <- renderPlotly({overall_1 %>% layout(legend = list(x = 0.29,y = 0.5, align = 'right', bgcolor = 'rgba(0,0,0,0)', font = list(size = 9)))%>% layout(height = 400)})

  output$g1p1 <- renderPlotly({plot_vec[[1]] %>% layout(showlegend = FALSE)})
  output$g1p2 <- renderPlotly({plot_vec[[5]] %>% layout(showlegend = FALSE)})
  output$g1p3 <- renderPlotly({plot_vec[[3]] %>% layout(showlegend = FALSE)})
  output$g1p4 <- renderPlotly({plot_vec[[4]] %>% layout(showlegend = FALSE)})
  output$g1p5 <- renderPlotly({plot_vec[[2]] %>% layout(showlegend = FALSE)})
  
  #Group 2
  
  plot_vec2 <- donut_plot(2)
  overall_2 <- weight_plot(2)
  
  output$o2 <- renderPlotly({overall_2 %>% layout(legend = list(x = 0.29,y = 0.5, align = 'right', bgcolor = 'rgba(0,0,0,0)', font = list(size = 9)))%>% layout(height = 400)})
  
  output$g2p1 <- renderPlotly({plot_vec2[[1]] %>% layout(showlegend = FALSE)})
  output$g2p2 <- renderPlotly({plot_vec2[[2]] %>% layout(showlegend = FALSE)})
  output$g2p3 <- renderPlotly({plot_vec2[[3]] %>% layout(showlegend = FALSE)})
  output$g2p4 <- renderPlotly({plot_vec2[[4]] %>% layout(showlegend = FALSE)})
  
  #Group 3
  
  plot_vec3 <- donut_plot(3)
  overall_3 <- weight_plot(3)
  
  output$o3 <- renderPlotly({overall_3 %>% layout(legend = list(x = 0.29,y = 0.5, align = 'right', bgcolor = 'rgba(0,0,0,0)', font = list(size = 9)))%>% layout(height = 400)})
  
  output$g3p1 <- renderPlotly({plot_vec3[[1]] %>% layout(showlegend = FALSE)})
  output$g3p2 <- renderPlotly({plot_vec3[[5]] %>% layout(showlegend = FALSE)})
  output$g3p3 <- renderPlotly({plot_vec3[[3]] %>% layout(showlegend = FALSE)})
  output$g3p4 <- renderPlotly({plot_vec3[[4]] %>% layout(showlegend = FALSE)})
  output$g3p5 <- renderPlotly({plot_vec3[[2]] %>% layout(showlegend = FALSE)})
  
  #Group 4
  
  plot_vec4 <- donut_plot(4)
  overall_4 <- weight_plot(4)
  
  output$o4 <- renderPlotly({overall_4 %>% layout(legend = list(x = 0.29,y = 0.5, align = 'right', bgcolor = 'rgba(0,0,0,0)', font = list(size = 9)))%>% layout(height = 400)})
  
  output$g4p1 <- renderPlotly({plot_vec4[[1]] %>% layout(showlegend = FALSE)})
  output$g4p2 <- renderPlotly({plot_vec4[[3]] %>% layout(showlegend = FALSE)})
  output$g4p3 <- renderPlotly({plot_vec4[[2]] %>% layout(showlegend = FALSE)})
  
  #Group 5
  
  plot_vec5 <- donut_plot(5)
  
  output$g5p1 <- renderPlotly({plot_vec5[[1]] %>% layout(legend = list(x = 0.29,y = 0.5, align = 'right', bgcolor = 'rgba(0,0,0,0)', font = list(size = 9)))%>% layout(height = 400)})
 
})

shinyApp(ui, server)