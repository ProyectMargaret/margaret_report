library(shiny)
library(DBI)
library(dplyr)
library(readr)
library(RSQLite)
library(shinydashboard)
library(DT)

# Getting data

PROYECTO <- read_csv("https://docs.google.com/spreadsheets/d/1SuRCahhlUWx5F1zD_gND4sPVsGpvjEv8v03IY_-5UhI/export?format=csv&gid=0")
ENTIDAD_EXTERNA <-  read_csv("https://docs.google.com/spreadsheets/d/1SuRCahhlUWx5F1zD_gND4sPVsGpvjEv8v03IY_-5UhI/export?format=csv&gid=1698938897")
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
      menuItem("Database", tabName = "database", icon = icon("database")),
      menuItem("Proyecto", tabName = "proyecto", icon = icon("filter"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "database",
              fluidRow(
                column(12,
                       sidebarPanel(
                         selectInput("unidad", "Choose a UNIDAD:", choices = c("todos", unique(PROGRAMA$UNIDAD))),
                         selectInput("region", "Choose a REGION:", choices = c("todos", unique(PROGRAMA$REGION))),
                         selectInput("programa", "Choose a PROGRAMA:", choices = c("todos", unique(PROGRAMA$PROGRAMA))),
                         selectInput("grupo", "Choose a GRUPO:", choices = c("todos", unique(GRUPO$GRUPO)))
                       )
                ),
                column(12,
                       mainPanel(
                         DT::dataTableOutput("table")
                       )
                )
              )
      ),
      tabItem(tabName = "proyecto",
              fluidRow(
                column(12,
                       sidebarPanel(
                         selectInput("nombre_proyecto", "Choose a NOMBRE_PROYECTO:", choices = unique(PROYECTO$NOMBRE_PROYECTO))
                       )
                ),
                column(12,
                       mainPanel(
                         DT::dataTableOutput("productos_table")
                       )
                )
              )
      )
    )
  )
)


# Define server logic
server <- function(input, output, session) {
  
  # Load PRODUCTOS data
  PRODUCTOS <- reactive({
    query <- "SELECT * FROM PRODUCTOS"
    dbGetQuery(con, query)
  })
  
  # Reactive expression to fetch data based on input$unidad, input$region, input$programa, and input$grupo
  data <- reactive({
    query <- "SELECT PROYECTO.NOMBRE_PROYECTO, PROYECTO.FECHA_INICIO, PROYECTO.FECHA_FIN, 
              PROYECTO.TOTAL_PRESUPUESTO_INTERNO, PROYECTO.PRESUPUESTO_EXTERNO, PROYECTO.PRESUPUESTO_TOTAL 
              FROM PROYECTO 
              JOIN PROGRAMA ON PROYECTO.ID_PROYECTO = PROGRAMA.ID_PROYECTO 
              JOIN GRUPO ON PROYECTO.ID_PROYECTO = GRUPO.ID_PROYECTO"
    
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
    
    dbGetQuery(con, query)
  })
  
  # Output the table
  output$table <- renderDataTable({
    data()
  })
  
  # Reactive expression to fetch PRODUCTS data based on input$nombre_proyecto
  data_productos <- reactive({
    if (!is.null(input$nombre_proyecto)) {
      id_proyecto <- PROYECTO$ID_PROYECTO[which(PROYECTO$NOMBRE_PROYECTO == input$nombre_proyecto)]
      PRODUCTOS()[which(PRODUCTOS()$ID_PROYECTO == id_proyecto), c("CATEGORIA", "NOMBRE")]
    } else {
      data.frame()  # Return an empty dataframe if no project is selected
    }
  })
  
  
  # Output the PRODUCTS table
  output$productos_table <- renderDataTable({
    data_productos()
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

