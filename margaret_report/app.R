library(DBI)
library(dplyr)
library(readr)
library(RSQLite)
library(shinydashboard)
library(DT)
library(shinyjs)

# Getting data
PROYECTO <- read_csv("https://docs.google.com/spreadsheets/d/1SuRCahhlUWx5F1zD_gND4sPVsGpvjEv8v03IY_-5UhI/export?format=csv&gid=0")
ENTIDAD_EXTERNA <- read_csv("https://docs.google.com/spreadsheets/d/1SuRCahhlUWx5F1zD_gND4sPVsGpvjEv8v03IY_-5UhI/export?format=csv&gid=1698938897")
PRODUCTOS <- read_csv("https://docs.google.com/spreadsheets/d/1SuRCahhlUWx5F1zD_gND4sPVsGpvjEv8v03IY_-5UhI/export?format=csv&gid=1113008466")
GRUPO <- read_csv("https://docs.google.com/spreadsheets/d/1SuRCahhlUWx5F1zD_gND4sPVsGpvjEv8v03IY_-5UhI/export?format=csv&gid=944537945")
PROGRAMA <- read_csv("https://docs.google.com/spreadsheets/d/1SuRCahhlUWx5F1zD_gND4sPVsGpvjEv8v03IY_-5UhI/export?format=csv&gid=705674233")
ADSCRIPCION <- read_csv("https://docs.google.com/spreadsheets/d/1u2XbiilAR235OySiuacJ4T5ASGRO3HXbNziz-nSnF2E/export?format=csv&gid=446420086")

# Create a connection
con <- dbConnect(RSQLite::SQLite(), "my_database.sqlite")

# Write your dataframes to the SQLite database
dbWriteTable(con, "PROYECTO", PROYECTO, overwrite = TRUE)
dbWriteTable(con, "ENTIDAD_EXTERNA", ENTIDAD_EXTERNA, overwrite = TRUE)
dbWriteTable(con, "PRODUCTOS", PRODUCTOS, overwrite = TRUE)
dbWriteTable(con, "GRUPO", GRUPO, overwrite = TRUE)
dbWriteTable(con, "PROGRAMA", PROGRAMA, overwrite = TRUE)
dbWriteTable(con, "ADSCRIPCION", ADSCRIPCION, overwrite = TRUE)

# Remove duplicate products
query <- "
  DELETE FROM PRODUCTOS
  WHERE ROWID NOT IN (
    SELECT MIN(ROWID)
    FROM PRODUCTOS
    GROUP BY ID_PROYECTO, CATEGORIA, NOMBRE
  )
"
dbExecute(con, query)

# Define UI for application
ui <- dashboardPage(
  dashboardHeader(title = "Interact with the PROGRAMA and GRUPO tables"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Database", tabName = "database", icon = icon("database")),
      menuItem("Proyecto", tabName = "proyecto", icon = icon("filter")),
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
              selectInput("filas_mostradas", "Filas mostradas:", choices = c(10, 25, 50, 100), selected = 10)
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
              selectInput("filas_mostradas", "Filas mostradas:", choices = c(10, 25, 50, 100), selected = 10)
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
              selectInput("filas_mostradas", "Filas mostradas:", choices = c(10, 25, 50, 100), selected = 10)
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
  # Load PRODUCTOS data
  PRODUCTOS <- reactive({
    query <- "SELECT DISTINCT * FROM PRODUCTOS"
    dbGetQuery(con, query)
  })
  
  # Reactive expression to fetch data based on input$unidad, input$region, input$programa, and input$grupo
  data <- reactive({
    query <- "SELECT PROYECTO.NOMBRE_PROYECTO, PROYECTO.FECHA_INICIO, PROYECTO.FECHA_FIN, 
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
    
    dbGetQuery(con, query)
  })
  
  # Output the table
  output$table <- DT::renderDataTable({
    datatable(data(), extensions = c("Buttons"), options = list(pageLength = 10,
                                                                dom = "Bfrtip",
                                                                buttons = list("copy",
                                                                               list(extend = "collection",
                                                                                    buttons = c("csv", "excel", "pdf"),
                                                                                    text = "Download")
                                                                ))
    )
  })
  
  # Load data for the proyectos table
  proyectos_data <- reactive({
    query <- "SELECT NOMBRE_PROYECTO, PRODUCTOS.CATEGORIA, PRODUCTOS.NOMBRE
            FROM PROYECTO
            JOIN PRODUCTOS ON PROYECTO.ID_PROYECTO = PRODUCTOS.ID_PROYECTO"
    dbGetQuery(con, query)
  })
  
  # Output the proyectos table
  output$productos_table <- DT::renderDataTable({
    datatable(proyectos_data(), options = list(
      pageLength = 10,
      dom = "Bfrtip",
      buttons = list("copy",
                     list(extend = "collection",
                          buttons = c("csv", "excel", "pdf"),
                          text = "Download")
      )
    ))
  })
  
  # Load data for the investigadores table
  investigadores_data <- reactive({
    query <- "SELECT PROYECTO.ID_PROYECTO,PROYECTO.NOMBRE_PROYECTO, ADSCRIPCION.ADSCRIPCION AS Investigador, ADSCRIPCION.TIPO AS Rol, 
            ADSCRIPCION.HORAS_SEMANALES, ADSCRIPCION.TOTAL_HORAS
            FROM PROYECTO
            JOIN ADSCRIPCION ON PROYECTO.ID_PROYECTO = ADSCRIPCION.ID_PROYECTO"
    dbGetQuery(con, query)
  })
  
  # Output the investigadores table
  output$investigadores_table <- DT::renderDataTable({
    datatable(investigadores_data(), options = list(
      pageLength = input$filas_mostradas,  # Obtener el número de filas seleccionadas
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