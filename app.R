library(shiny)
library(shinydashboard)
library(shinydashboardPlus)
library(dashboardthemes)
library(tidyverse)
library(lubridate)
library(plotly)
library(leaflet)
library(leaflet.extras)
library(ggmosaic)
library(shinyWidgets)

#library(RSocrata)



# # colinha pra baixar o banco e salvar
#
# years_ago <- today() - years(2)
# crash_url <- glue::glue("https://data.cityofchicago.org/Transportation/Traffic-Crashes-Crashes/85ca-t3if?$where=CRASH_DATE > '{years_ago}'")
# crash_raw <- as_tibble(read.socrata(crash_url))
# 
# write_csv(crash,"dados.csv")
# crash <- crash_raw %>%
#     arrange(desc(crash_date)) %>%
#     transmute(
#         injuries = if_else(injuries_total > 0, "injuries", "none"),
#         crash_date,
#         crash_hour,
#         report_type = if_else(report_type == "", "UNKNOWN", report_type),
#         num_units,
#         posted_speed_limit,
#         weather_condition,
#         lighting_condition,
#         roadway_surface_cond,
#         first_crash_type,
#         trafficway_type,
#         prim_contributory_cause,
#         latitude, longitude
#     ) %>%
#     na.omit()



# lendo o banco de dados ja baixado pra n demorar muito
dados <- read_csv("dados.csv")

dados_mapa <- dados %>%
  filter(latitude != 0) # retirando latitudes e longitudes estranhas


dados_g <- dados_mapa
dados_g$weather_condition <- as.factor(dados_g$weather_condition)
dados_g$lighting_condition <- as.factor(dados_g$lighting_condition)
dados_g$roadway_surface_cond <- as.factor(dados_g$roadway_surface_cond)


b_escolhas <- names(dados)[3:12]
b_escolhas_qualitativo <- c(F,T,F,T,T,T,T,T,T,T)
dados_b <- dados
dados_b$posted_speed_limit <- as.factor(dados_b$posted_speed_limit)

dados_mapa$ano_mes <- format(as.Date(dados_mapa$crash_date), "%Y-%m")
label2 <- row.names(table(dados_mapa$ano_mes))

# fazendo o banco para o mapa sem as latitudes e longitudes 0


# componentes do UI

# cabeçalho
header <- dashboardHeader( 
    title = "App BGG", # aceito sugestões de nomes melhores haha
    titleWidth = 400
)

# aba d lado
sidebar <- dashboardSidebar(
    sidebarMenu(
        menuItem("Gráfico de Correlação", tabName = "aba_bruno",icon = icon("chart-bar")), # aba para o bruno fazer
        menuItem("Mapa -- Lesão", tabName = "aba_gustavo", icon = icon("map-marker")), # aba para o gustavo fazer
        menuItem("Mapa -- cond de luz", tabName = "aba_guilherme", icon = icon("moon")),
        menuItem("Mapa Animado", tabName = "aba_bruno_animation", icon = icon("globe-americas"))
        # aba para o guilherme fazer
    ),
    width = 180
)

# corpo
body <- dashboardBody(
    # adicionando tema
    
    shinyDashboardThemes(
        theme = "grey_dark"
    ),
    
    tabItems(
        tabItem(
            "aba_guilherme",
            fluidPage(
                titlePanel(
                    "Mapa dos acidentes segundo a condição de iluminação"
                ),
                fluidRow(
                    column(
                        width = 6,
                        h4("Escolha a condição de luz da estrada"),
                        radioButtons("light",
                                     label = NULL,
                                     choices = list("Noite -- sem iluminação" = "DARKNESS",
                                                    "Noite -- com iluminação" = "DARKNESS, LIGHTED ROAD",
                                                    "Amanhecer" = "DAWN", "Dia limpo" = "DAYLIGHT",
                                                    "Anoitecer" = "DUSK"),
                                     selected = "DAYLIGHT",
                                     inline = T)
                    ),
                    column(
                        width = 6,
                        valueBoxOutput("n_acidentes_1", width = 12),
                    ),
                    column(
                        width = 12,
                        mainPanel(
                            leafletOutput("mapa_acidentes_luz", height = "600px"),
                            width = 12
                        )
                    )
                )
                
            )
        ),
        tabItem(
            "aba_bruno",
            fluidPage(
                fluidRow(box(
                    width = 6,
                    selectInput(
                        inputId = "b_input_2", multiple = FALSE, choices = names(dados)[3:12],
                        label = "Selecione a variável do eixo Y"
                    )
                ),
                box(
                    width = 6,
                    selectInput(
                        inputId = "b_input_1", multiple = FALSE, choices = names(dados)[3:12],
                        label = "Selecione a variável do eixo X", selected = names(dados)[4])
                )),
                
                fluidRow(box(width = 12,title = "Gráfico 1",collapsible = TRUE,
                             plotlyOutput("b_grafico_correlacao"))),
                
                fluidRow(box(width = 12,title = "Gráfico 2",collapsible = TRUE,
                             plotlyOutput("b_grafico_correlacao2"))),
                
                fluidRow(box(width = 12,title = "Gráfico 3",collapsible = TRUE,
                             plotlyOutput("b_grafico_correlacao3")))
                
            )
        ),
        tabItem(
            "aba_gustavo",
            fluidPage(
                titlePanel(
                    "Mapa com ocorrência dos acidentes conforme lesão"
                ),
                fluidRow(
                    column(
                        width = 4,
                        h4("Defina se serão apresentados os acidentes sem ou com lesão:"),
                        radioButtons("injury_3",
                                     label = NULL,
                                     choices = list("Sem lesão" = "none","Com lesão" = "injuries"),
                                     selected = "none",
                                     inline = T)
                    ),
                    uiOutput("input_max_3"),
                    column(
                        width = 4,
                        valueBoxOutput("n_acidentes_3", width = 12),
                    ),
                    column(
                        width = 12,
                        mainPanel(
                            leafletOutput("mapa_acidentes", height = "600px"),
                            width = 12
                        )
                        
                    )
                )
                
            )
        ),
        tabItem("aba_bruno_animation",
                fluidPage(
                  # sliderInput("horasele", "Date",
                  #             min = min(dados_mapa$ano_mes),
                  #             max = max(dados_mapa$ano_mes),
                  #             value = min(dados_mapa$ano_mes),
                  #             step = "m",
                  #             timeFormat = "%Y-%m",
                  #             animate = animationOptions(interval = 500, loop = FALSE)
                  # ),
                  
                  shinyWidgets::sliderTextInput(inputId = "mes_selec", label = "Mês",
                                  choices =  label2, 
                                  from_min =  label2[1], to_max = label2[22],animate = TRUE
                                  ),
                  
                  # ,
                  # 
                  # 
                   leafletOutput("map_b_animado")
                  
                )
                
                
                )
        
    )
)


ui <- dashboardPage(
    header = header,
    sidebar = sidebar,
    body = body
)

server <- function(input, output) {
    
  
    output$map_b_animado  <- renderLeaflet({
      cond <- dados_mapa %>%
        filter(ano_mes == input$mes_selec)
      
      max <- nrow(cond)/200
        
      leaflet(cond) %>%
        addProviderTiles(providers$CartoDB.DarkMatter) %>%
        addHeatmap(
          lng = ~ longitude,
          lat = ~ latitude,
          blur = 15,
          max = max,
          radius = 10
        )    
        

    })
      
    ######### ABA 1 ###########
    
    output$n_acidentes_1 <- renderValueBox({
        
        cond <- dados_g %>%
            filter(lighting_condition %in% input$light)
        
        valueBox(
            nrow(cond), 
            "Acidentes", 
            icon = icon("car-crash"),
            color = "purple",
            width = NULL
        )
    })
    
    output$mapa_acidentes_luz <- renderLeaflet({
        
        cond <- dados_g %>%
            filter(lighting_condition %in% input$light)
        
        max <- nrow(cond)/200
        
        if(cond$lighting_condition %in% c("DARKNESS, LIGHTED ROAD","DARKNESS")){
        
            leaflet(cond) %>%
                addProviderTiles(providers$CartoDB.DarkMatter) %>%
                addHeatmap(lng = ~longitude, lat = ~latitude,
                           blur = 15, max = max, radius = 10)    
            
        } else {
        
        leaflet(cond) %>%
            addTiles() %>%
            addHeatmap(lng = ~longitude, lat = ~latitude,
                       blur = 15, max = max, radius = 10)
        
        }
    })
    
    
    
    ######### ABA 2 ###########
    
    
    output$gera_input2 <- renderUI({ 
        nome <- names(dados_b)[3:12]
        nome <- nome[nome != input$b_input_1]
        
    })
    
    output$b_grafico_correlacao <- renderPlotly({
        
        ggplot(dados_b) + 
            geom_count(mapping = aes_string(x = input$b_input_1,y=input$b_input_2),colour="blue") +
            scale_size(range=c(2, 8))
        
    })
    
    output$b_grafico_correlacao2 <- renderPlotly({
        
        if(b_escolhas_qualitativo[which(b_escolhas == input$b_input_1)]){
            p <- ggplot(dados_b) + 
                geom_mosaic(aes(product(!!sym(input$b_input_1),!!sym(input$b_input_2)),fill = !!sym(input$b_input_1)))
        } else if(b_escolhas_qualitativo[which(b_escolhas == input$b_input_2)]){
            p <- ggplot(dados_b) + 
                geom_mosaic(aes(product(!!sym(input$b_input_1),!!sym(input$b_input_2)),fill = !!sym(input$b_input_2)))
            
        } else{
            p <- ggplot(dados_b) + 
                geom_mosaic(aes(product(!!sym(input$b_input_1),!!sym(input$b_input_2))))
        }
        ggplotly(p)
    })
    
    output$b_grafico_correlacao3 <- renderPlotly({
        
        p <- ggplot(dados_b, aes_string(x=input$b_input_1))+
            geom_bar(aes_string(fill = input$b_input_2), position = position_stack(reverse = TRUE))+
            facet_wrap(~injuries)
        
        if(b_escolhas_qualitativo[which(b_escolhas == input$b_input_2)]){
            p <- ggplot(dados_b, aes_string(x=input$b_input_1))+
                geom_bar(aes_string(fill = input$b_input_2), position = position_stack(reverse = TRUE))+
                facet_wrap(~injuries)
        } else if(b_escolhas_qualitativo[which(b_escolhas == input$b_input_1)]){
            p <- ggplot(dados_b, aes_string(x=input$b_input_1))+
                geom_bar(aes_string(fill = input$b_input_1), position = position_stack(reverse = TRUE))+
                facet_wrap(~injuries)
            
        } else{
            p <- ggplot(dados_b, aes_string(x=input$b_input_1))+
                geom_bar(aes_string(fill = input$b_input_2), position = position_stack(reverse = TRUE))+
                facet_wrap(~injuries)
        }
        ggplotly(p)
        
        
    })
    
    
    
    
    ######### ABA 3 ###########
    
    output$n_acidentes_3 <- renderValueBox({
        
        aux <- dados_mapa %>%
            filter(injuries %in% input$injury_3)
        
        valueBox(
            nrow(aux), 
            "Acidentes", 
            icon = icon("car-crash"),
            color = "purple",
            width = NULL
        )
    })
    
    output$input_max_3 <- renderUI({
      aux <- dados_mapa %>%
        filter(injuries %in% input$injury_3)
      
      default <- nrow(aux)/200
      
      column(
        width = 4,
        h4("Defina o argumento de intensidade máxima do ponto"),
        sliderInput("max_3",
                    label = NULL,
                    min = 0.5,
                    max = 1000,
                    value = default,
                    step = 1)
      )
      
    })
    
    output$mapa_acidentes <- renderLeaflet({
        
        aux <- dados_mapa %>%
            filter(injuries %in% input$injury_3)
        
        max <- input$max_3
        
        leaflet(aux) %>%
          addProviderTiles(providers$CartoDB.DarkMatter) %>%
          addHeatmap(lng = ~longitude, lat = ~latitude,
                       blur = 15, max = max, radius = 10)
        
    })
    
    
}

# Run the application 
shinyApp(ui = ui, server = server)
