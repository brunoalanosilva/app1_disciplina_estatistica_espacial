library(shiny)
library(shinydashboard)
library(shinydashboardPlus)
library(dashboardthemes)
library(tidyverse)
library(lubridate)
library(RSocrata)


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

# componentes do UI

# cabeçalho
header <- dashboardHeaderPlus( 
    title = "App BGG", # aceito sugestões de nomes melhores haha
    titleWidth = 400
)

# aba d lado
sidebar <- dashboardSidebar(
    sidebarMenu(
        menuItem("Gráfico .....", tabName = "aba_guilherme"), # aba para o guilherme fazer
        menuItem("Gráfico ......", tabName = "aba_bruno"), # aba para o bruno fazer
        menuItem("Mapa", tabName = "aba_gustavo") # aba para o gustavo fazer
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
            # exemplo de aba
            sidebarPanel(
                sliderInput("bins_aba_1",
                            "Number of bins:",
                            min = 1,
                            max = 50,
                            value = 30)
            ),
            
            # Show a plot of the generated distribution
            mainPanel(
                plotOutput("distPlot_aba_1")
            )
        ),
        tabItem(
            "aba_bruno",
            # exemplo de aba
            sidebarPanel(
                sliderInput("bins_aba_2",
                            "Number of bins:",
                            min = 1,
                            max = 50,
                            value = 30)
            ),
            
            # Show a plot of the generated distribution
            mainPanel(
                plotOutput("distPlot_aba_2")
            )
        ),
        tabItem(
            "aba_gustavo",
            # exemplo de aba
            sidebarPanel(
                sliderInput("bins_aba_3",
                            "Number of bins:",
                            min = 1,
                            max = 50,
                            value = 30)
            ),
            
            # Show a plot of the generated distribution
            mainPanel(
                plotOutput("distPlot_aba_3")
            )
        )
    )
)


ui <- dashboardPagePlus(
    header = header,
    sidebar = sidebar,
    body = body
)

server <- function(input, output) {
    
    ######### ABA 1 ###########

    output$distPlot_aba_1 <- renderPlot({
        # generate bins based on input$bins from ui.R
        x    <- faithful[, 2]
        bins <- seq(min(x), max(x), length.out = input$bins_aba_1 + 1)

        # draw the histogram with the specified number of bins
        hist(x, breaks = bins, col = 'darkgray', border = 'white')
    })
    
    ######### ABA 2 ###########
    
    output$distPlot_aba_2 <- renderPlot({
        # generate bins based on input$bins from ui.R
        x    <- faithful[, 2]
        bins <- seq(min(x), max(x), length.out = input$bins_aba_2 + 1)
        
        # draw the histogram with the specified number of bins
        hist(x, breaks = bins, col = 'darkgray', border = 'white')
    })
    
    ######### ABA 3 ###########
    
    output$distPlot_aba_3 <- renderPlot({
        # generate bins based on input$bins from ui.R
        x    <- faithful[, 2]
        bins <- seq(min(x), max(x), length.out = input$bins_aba_3 + 1)
        
        # draw the histogram with the specified number of bins
        hist(x, breaks = bins, col = 'darkgray', border = 'white')
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
