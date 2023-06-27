library(DBI)
library(dplyr)
library(readr)
library(RSQLite)
library(shinydashboard)
library(DT)
library(shinyjs)
library(tidyverse)


# Getting data
PROYECTO <- read_csv("https://docs.google.com/spreadsheets/d/1SuRCahhlUWx5F1zD_gND4sPVsGpvjEv8v03IY_-5UhI/export?format=csv&gid=0")
ENTIDAD_EXTERNA <- read_csv("https://docs.google.com/spreadsheets/d/1SuRCahhlUWx5F1zD_gND4sPVsGpvjEv8v03IY_-5UhI/export?format=csv&gid=1698938897")
PRODUCTOS <- read_csv("https://docs.google.com/spreadsheets/d/1SuRCahhlUWx5F1zD_gND4sPVsGpvjEv8v03IY_-5UhI/export?format=csv&gid=1113008466")
GRUPO <- read_csv("https://docs.google.com/spreadsheets/d/1SuRCahhlUWx5F1zD_gND4sPVsGpvjEv8v03IY_-5UhI/export?format=csv&gid=944537945")
PROGRAMA <- read_csv("https://docs.google.com/spreadsheets/d/1SuRCahhlUWx5F1zD_gND4sPVsGpvjEv8v03IY_-5UhI/export?format=csv&gid=705674233")
ADSCRIPCION <- read_csv("https://docs.google.com/spreadsheets/d/1SuRCahhlUWx5F1zD_gND4sPVsGpvjEv8v03IY_-5UhI/export?format=csv&gid=536733097")

# Create a connection
con <- dbConnect(RSQLite::SQLite(), "my_database.sqlite")

# Write your dataframes to the SQLite database
dbWriteTable(con, "PROYECTO", PROYECTO, overwrite = TRUE)
dbWriteTable(con, "ENTIDAD_EXTERNA", ENTIDAD_EXTERNA, overwrite = TRUE)
dbWriteTable(con, "PRODUCTOS", PRODUCTOS, overwrite = TRUE)
dbWriteTable(con, "GRUPO", GRUPO, overwrite = TRUE)
dbWriteTable(con, "PROGRAMA", PROGRAMA, overwrite = TRUE)
dbWriteTable(con, "ADSCRIPCION", ADSCRIPCION, overwrite = TRUE)

# Define UI for application
ui <- dashboardPage(
  dashboardHeader(title = "Interact with the PROGRAMA and GRUPO tables"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Informacion Financiera", tabName = "database", icon = icon("database")),
      menuItem("Productos comprometidos", tabName = "proyecto", icon = icon("filter")),
      menuItem("Investigadores", tabName = "investigadores", icon = icon("users"))
    )
  ),
  dashboardBody(
    useShinyjs(),
    tabItems(
      tabItem(
        tabName = "database",
        fluidRow(
          column(
            12,
            sidebarPanel(
              selectInput("unidad", "Choose a UNIDAD:", choices = c("todos", unique(PROGRAMA$UNIDAD))),
              selectInput("region", "Choose a REGION:", choices = c("todos", unique(PROGRAMA$REGION))),
              selectInput("programa", "Choose a PROGRAMA:", choices = c("todos", unique(PROGRAMA$PROGRAMA))),
              selectInput("grupo", "Choose a GRUPO:", choices = c("todos", unique(GRUPO$GRUPO)))
            )
          ),
          column(
            12,
            mainPanel(
              selectInput("filas_mostradas_db", "Filas mostradas:", choices = c(10, 25, 50, 100), selected = 10)
            )
          ),
          column(
            12,
            mainPanel(
              DT::dataTableOutput("table")
            )
          )
        )
      ),
      tabItem(
        tabName = "proyecto",
        fluidRow(
          column(
            12,
            mainPanel(
              selectInput("filas_mostradas_proyecto", "Filas mostradas:", choices = c(10, 25, 50, 100), selected = 10)
            )
          ),
          column(
            12,
            mainPanel(
              DT::dataTableOutput("productos_table")
            )
          )
        )
      ),
      tabItem(
        tabName = "investigadores",
        fluidRow(
          column(
            12,
            mainPanel(
              selectInput("filas_mostradas_investigadores", "Filas mostradas:", choices = c(10, 25, 50, 100), selected = 10)
            )
          ),
          column(
            12,
            mainPanel(
              DT::dataTableOutput("investigadores_table")
            )
          )
        )
      )
    )
  )
)

# Define server logic
server <- function(input, output, session) {
  # Load data for the "Informacion Financiera" table
  # Load data for the "Informacion Financiera" table
  data <- reactive({
    query <- "SELECT DISTINCT PROYECTO.NOMBRE_PROYECTO, PROYECTO.FECHA_INICIO, PROYECTO.FECHA_FIN, 
            PROYECTO.TOTAL_PRESUPUESTO_INTERNO, PROYECTO.PRESUPUESTO_EXTERNO, PROYECTO.PRESUPUESTO_TOTAL,
            ADSCRIPCION.ENTIDAD
            FROM PROYECTO
            JOIN PROGRAMA ON PROYECTO.ID_PROYECTO = PROGRAMA.ID_PROYECTO
            JOIN GRUPO ON PROYECTO.ID_PROYECTO = GRUPO.ID_PROYECTO
            JOIN ADSCRIPCION ON PROYECTO.ID_PROYECTO = ADSCRIPCION.ID_PROYECTO"
    
    # Construct WHERE clause based on the selected values
    conditions <- c()
    if (input$unidad != "todos") {
      conditions <- c(conditions, sprintf("PROGRAMA.UNIDAD = '%s'", input$unidad))
    }
    if (input$region != "todos") {
      conditions <- c(conditions, sprintf("PROGRAMA.REGION = '%s'", input$region))
    }
    if (input$programa != "todos") {
      conditions <- c(conditions, sprintf("PROGRAMA.PROGRAMA = '%s'", input$programa))
    }
    if (input$grupo != "todos") {
      conditions <- c(conditions, sprintf("GRUPO.GRUPO = '%s'", input$grupo))
    }
    
    if (length(conditions) > 0) {
      query <- paste(query, "WHERE", paste(conditions, collapse = " AND "))
    }
    
    data <- dbGetQuery(con, query)
    
    # Remove duplicated rows based on NOMBRE_PROYECTO column
    data <- data %>% distinct(NOMBRE_PROYECTO, .keep_all = TRUE)
    
    data
  })
  
  
  # Render the "Informacion Financiera" table
  output$table <- DT::renderDataTable({
    datatable(data(), extensions = c("Buttons"), options = list(pageLength = input$filas_mostradas_db,
                                                                dom = "Bfrtip",
                                                                buttons = list("copy",
                                                                               list(extend = "collection",
                                                                                    buttons = c("csv", "excel", "pdf"),
                                                                                    text = "Download")
                                                                ))
    )
  })
  
  # Load data for the "Productos comprometidos" table
  proyectos_data <- reactive({
    query <- "SELECT NOMBRE_PROYECTO, PRODUCTOS.CATEGORIA, PRODUCTOS.NOMBRE
            FROM PROYECTO
            JOIN PRODUCTOS ON PROYECTO.ID_PROYECTO = PRODUCTOS.ID_PROYECTO"
    dbGetQuery(con, query)
  })
  
  # Render the "Productos comprometidos" table
  output$productos_table <- DT::renderDataTable({
    datatable(proyectos_data(), options = list(
      pageLength = input$filas_mostradas_proyecto,
      dom = "Bfrtip",
      buttons = list("copy",
                     list(extend = "collection",
                          buttons = c("csv", "excel", "pdf"),
                          text = "Download")
      )
    ))
  })
  
  # Load data for the "Investigadores" table
  investigadores_data <- reactive({
    query <- "SELECT PROYECTO.ID_PROYECTO,PROYECTO.NOMBRE_PROYECTO, ADSCRIPCION.ADSCRIPCION AS Investigador, ADSCRIPCION.TIPO AS Rol, 
            ADSCRIPCION.HORAS_SEMANALES, ADSCRIPCION.TOTAL_HORAS
            FROM PROYECTO
            JOIN ADSCRIPCION ON PROYECTO.ID_PROYECTO = ADSCRIPCION.ID_PROYECTO"
    dbGetQuery(con, query)
  })
  
  # Render the "Investigadores" table
  output$investigadores_table <- DT::renderDataTable({
    datatable(investigadores_data(), options = list(
      pageLength = input$filas_mostradas_investigadores,
      dom = "Bfrtip",
      buttons = list("copy",
                     list(extend = "collection",
                          buttons = c("csv", "excel", "pdf"),
                          text = "Download")
      )
    ))
  })
}

# Run the application
shinyApp(ui = ui, server = server)
