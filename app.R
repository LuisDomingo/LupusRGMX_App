## Descripción

# Esta es la plantilla general para la Shinny App de lupus. Se consideran las siguientes secciones:

#1. Mapeo de algunas variables a una coropleta.
#2. Gráficos descriptivos.
#3. Nube de palabras de sentimientos.
#4. Calculadora de riesgo de nefritis y riesgo de hipertensión.
#5. Solicitud de datos del registro.

## Fuentes

# <https://rstudio.github.io/bslib/articles/theming/index.html>
#  <https://bootswatch.com/>
#  <https://rstudio.github.io/shinythemes/>
#  <https://fontawesome.com/icons?d=gallery>

library(shiny)
library(shinydashboard)
library(tidyverse)
library(tidytext)
library(dplyr)
library(ggplot2)
library(viridis)
library(bnlearn)
library(sf)
library(utils)

# Definir colores personalizados (requiere CSS adicional)
customCSS <- tags$head(
  tags$style(HTML("
    /* === Estilos del tema púrpura === */
    .skin-purple .main-header .logo {
      background-color: purple;
      color: white;
    }
    .skin-purple .main-header .navbar {
      background-color: purple;
    }
    .skin-purple .main-sidebar {
      background-color: purple;
    }
    .skin-purple .sidebar-menu > li.active > a {
      background-color: violet;
      color: #fff;
    }
    .skin-purple .sidebar-menu > li > a {
      color: #EDE7F6;
    }
    h3 {
      color: purple;
    }

    /* === Estilos personalizados para sliders === */
    .irs-bar {
      background-color: #7B157A !important;
      border-color: #7B157A !important;
    }
    .irs-slider {
      background-color: #7B157A !important;
      border-color: #7B157A !important;
    }
    .irs-from, .irs-to, .irs-single {
      background-color: #7B157A !important;
      border-color: #7B157A !important;
      color: white !important;
    }
  "))
)

############### NOTA IMPORTANTE ###############################################
# Las diferentes secciones hacen manipulación de las variables, entonces, 
# de momento será mejor que cada sección llame y manipule su propio conjunto
# de datos aunque sea ineficiente. En futuras versiones se recomienda unificar 
# un solo conjunto de datos en el ambiente reactivo.
###############################################################################

# Cargar datos

## Leer datos manteniendo todos los registros
Datos <- reactive({
  file_id <- "1y-PHjo3fjeX1B_jEOYwhu4I2Rt1NsLdE"  # Reemplaza con tu ID real
  url <- sprintf("https://drive.google.com/uc?export=download&id=%s", file_id)
  
  tryCatch({
    df <- read.csv(url, stringsAsFactors = FALSE, na.strings = c("", "NA"))
    return(df)
  }, error = function(e) {
    warning("No se pudo leer el archivo desde Google Drive: ", e$message)
    return(data.frame())
  })
})


## Leer datos manteniendo todos los registros de una replica del conjunto
## de datos, este será manipulado para la sección 2 de Gráficos Descriptivos

Datos_2 <- reactive({
  Datos() %>%  # <-- Ejecutas el reactivo aquí
    rename(
      sexo = sex___1,
      grupo_etario = age_group, 
      estado_de_residencia = home, 
      ocupacion = ocupation, 
      proveedor_de_serv_de_salud = health_provider, 
      escolaridad = school
    ) %>%
    mutate(
      sexo = recode_factor(sexo, "1" = "Mujer", "0" = "Hombre"),
      lupus = recode_factor(lupus, "1" = "sí", "2" = "no"),
      proveedor_de_serv_de_salud = recode_factor(
        proveedor_de_serv_de_salud,
        "1" = "IMSS", "2" = "ISSTE", "3" = "PEMEX", "4" = "SEDENA",
        "5" = "SEMAR", "6" = "Secretaría de salud", 
        "7" = "Servicios estatales de salud", "8" = "IMSS-Bienestar"
      ),
      grupo_etario = recode_factor(
        grupo_etario,
        "1" = "18 a 25 años", "2" = "26 a 31 años", "3" = "32 a 45 años",
        "4" = "46 a 50 años", "5" = "51 a 60 años", "6" = "Más de 60 años"
      ),
      ocupacion = recode_factor(
        ocupacion,
        "1" = "Estudiante", "2" = "Empleada", "3" = "Desempleada", 
        "4" = "Jubilada o retirada"
      ),
      escolaridad = recode_factor(
        escolaridad,
        "1" = "Ninguna", "2" = "Primaria", "3" = "Secundaria",
        "4" = "Prepa o carrera técnica", "5" = "Licenciatura", 
        "6" = "Posgrado"
      )
    )
})



# Dependencias SECCIÓN 1:

## Nombres completos de los estados de México en orden alfabético
estados <- c("Aguascalientes", "Baja California", "Baja California Sur", "Campeche",
             "Chiapas", "Chihuahua", "Ciudad de México", "Coahuila", "Colima", "Durango",
             "Guanajuato", "Guerrero", "Hidalgo", "Jalisco", "México", "Michoacán",
             "Morelos", "Nayarit", "Nuevo León", "Oaxaca", "Puebla", "Querétaro",
             "Quintana Roo", "San Luis Potosí", "Sinaloa", "Sonora", "Tabasco",
             "Tamaulipas", "Tlaxcala", "Veracruz", "Yucatán", "Zacatecas")

## Cargar shapefile de INEGI
library(sf)
library(utils)

# ID del archivo .zip en Google Drive
file_id <- "1onC2e_hRX9ZqOUBJhji15teRT1sWnDPH"  # reemplaza con tu ID

# URL directa de descarga
url <- sprintf("https://drive.google.com/uc?export=download&id=%s", file_id)

# Ruta temporal
temp_zip <- tempfile(fileext = ".zip")
temp_dir <- tempdir()

# Descargar y descomprimir
download.file(url, destfile = temp_zip, mode = "wb")
unzip(temp_zip, exdir = temp_dir)

# Leer shapefile desde los archivos extraídos
shape_estados <- read_sf(dsn = temp_dir, layer = "00ent")


## Definición correcta de regiones con códigos INEGI
regiones <- list(
  "A. Región Norte" = c(2, 3, 5, 8, 10, 19, 25, 26, 28, 32),
  "B. Región Centro" = c(1, 6, 11, 13, 14, 15, 16, 18, 22, 24, 29),
  "C. Región Sur" = c(4, 7, 12, 17, 20, 21, 23, 27, 30, 31),
  "D. Ciudad de México" = 9)

## Tabla de referencia con códigos correctos
estados_mexico <- data.frame(
  state_code = 1:32,
  state_name = c("Aguascalientes", "Baja California", "Baja California Sur", "Campeche",
                 "Coahuila", "Colima", "Chiapas", "Chihuahua", 
                 "Ciudad de México", "Durango", "Guanajuato", "Guerrero", 
                 "Hidalgo", "Jalisco", "México", "Michoacán", "Morelos", 
                 "Nayarit", "Nuevo León", "Oaxaca", "Puebla", "Querétaro", 
                 "Quintana Roo", "San Luis Potosí", "Sinaloa", "Sonora",
                 "Tabasco", "Tamaulipas", "Tlaxcala", "Veracruz", "Yucatán", "Zacatecas"),
  stringsAsFactors = FALSE
)

## Crear objeto espacial con códigos correctos
mexico_sf <- shape_estados %>%
  mutate(cve_ent_num = as.numeric(CVE_ENT)) %>%
  left_join(estados_mexico, by = c("cve_ent_num" = "state_code")) %>%
  st_transform(crs = 4326) %>%
  # Asignar regiones a cada estado
  mutate(
    region = case_when(
      cve_ent_num %in% regiones[["A. Región Norte"]] ~ "A. Región Norte",
      cve_ent_num %in% regiones[["B. Región Centro"]] ~ "B. Región Centro",
      cve_ent_num %in% regiones[["C. Región Sur"]] ~ "C. Región Sur",
      cve_ent_num == 9 ~ "D. Ciudad de México",
      TRUE ~ "Otra"
    )
  )

## Dependencias SECCIÓN 3

# Paleta de colores para la nube de palabras
morados <- c("#7B157A", "#DEA3DE", "#7F397D", "#FFD3FF", "#8F488C", "#9C449C")

## Dependencias SECCIÓN 4
library(bnlearn)
library(gRain)

# Cargar el modelo
# Construir la URL directa de descarga
file_id <- "1Okx_7vl4fzbYcF74g25HLZaR7pdXB0At"  # Reemplaza con tu ID real
url <- sprintf("https://drive.google.com/uc?export=download&id=%s", file_id)

# Ruta temporal para guardar el archivo
temp_rds <- tempfile(fileext = ".rds")

# Descargar el archivo
download.file(url, destfile = temp_rds, mode = "wb")

# Leer el archivo RDS
modelo_2 <- readRDS(temp_rds)


# Nombres anteriores
nombres_anteriores_2 <- names(modelo_2)
# Vector de nuevos nombres
nombres_nuevos_2 <- c("Nefritis", 
                      "Ancestria_originaria", 
                      "Tromboembolia_pulmonar", 
                      "Enfermedad_arterial_periférica",
                      "Infarto", 
                      "Angina_de_pecho", 
                      "Insuficiencia_cardiaca", 
                      "Diabetes_gestacional",
                      "Aborto", 
                      "Embarazo_ectopico", 
                      "Ciclofosfamida", 
                      "Años_de_retraso_en_diagnóstico",
                      "Años_con_lupus", 
                      "Comorbilidades", 
                      "SLICC", 
                      "SLEDAI", 
                      "Nivel_Socioeconómico", 
                      "Puntaje_calidad_de_vida",
                      "Depresion", 
                      "Ansiedad", 
                      "Degeneracion_macular", 
                      "Prematuro",
                      "Dislexia", 
                      "Clasificación_alimentación", 
                      "Obesidad", 
                      "Sobrepeso", 
                      "Grupo_etario", 
                      "Tabaco", 
                      "Alcohol", 
                      "Hipertensión"
)

# Renombrar variables de interés
nombres_map_2 <- setNames(nombres_nuevos_2, nombres_anteriores_2)
modelo_2 <- rename.nodes(modelo_2, nombres_map_2)
names(modelo_2)

modelo_grain_2 <- as.grain(modelo_2)

# Obtener todas las variables disponibles
todas_variables <- names(modelo_2)

# Retiro variables que no aportan mucho
todas_variables <- todas_variables[!todas_variables %in% c(
  "Alcohol",
  "Tabaco",
  "Comorbilidades",
  "Ciclofosfamida",
  "Degeneracion_macular", 
  "Prematuro",
  "Dislexia",
  "Diabetes_gestacional",
  "Aborto", 
  "Embarazo_ectopico"
)]

# Obtener niveles para los selectInput
niveles_todas_vars <- lapply(modelo_2, function(x) dimnames(x$prob)[[1]])


# Interfaz de usuario 

ui <- dashboardPage(
  skin = "purple",  # Puedes usar purple, pero le daremos un estilo personalizado arriba
  dashboardHeader(title = "Lupus en México"),
  dashboardSidebar(
    customCSS,  # Incluir estilos personalizados
    sidebarMenu(
      menuItem("Sección 1", tabName = "sec1", icon = icon("map")),
      menuItem("Sección 2", tabName = "sec2", icon = icon("chart-column")),
      menuItem("Sección 3", tabName = "sec3", icon = icon("comment")),
      menuItem("Sección 4", tabName = "sec4", icon = icon("stethoscope")),
      menuItem("Sección 5", tabName = "sec5", icon = icon("database"))
    )
  ),
  dashboardBody(
    customCSS,
    tabItems(
      ### Integrar App Sección 1: mapa temático
      tabItem(tabName = "sec1", h3("Mapeo de personas registradas"),p("Distribución espacial de lupus en México"),fluidRow(
        
        sidebarLayout(
          sidebarPanel(
            selectInput("region", "Seleccionar región:",
                        choices = c("Nacional", names(regiones)),
                        selected = "Nacional"),
            h4("Resumen de casos"),
            tableOutput("resumen_tabla"),
            h4("Casos por región"),
            tableOutput("tabla_regiones"),
            downloadButton("downloadData", "Descargar datos")
          ),
          mainPanel(
            plotOutput("mapa", height = "600px"),
            plotOutput("barplot", height = "300px")
          )
        )
      )),
      
      
      ### Integrar App Sección 2: gráficos de barras contiguas
      tabItem(
        tabName = "sec2",
        h3("Comparativo de características"),
        fluidRow(
          box(
            width = 12,
            h4("Exploración de características"),
            p("Selecciona las variables para comparar mediante gráficos de barras."),
            
            sidebarLayout(
              sidebarPanel(
                uiOutput("x_selector_2"),
                uiOutput("fill_selector_2")
              ),
              
              mainPanel(
                plotOutput("barplot_2")
              )
            )
          )
        )
      )
      ,
      
      ### Integrar App Sección 3: nube de palabras de sentimientos
      tabItem(tabName = "sec3",
              fluidRow(
                box(
                  width = 12,
                  h3("Vivir con lupus: Explorando las experiencias"),
                  p("Esta sección analiza las situaciones de estrés reportadas por personas con lupus.")
                )
              ),
              fluidRow(
                box(
                  width = 4,
                  title = span("Filtros de datos",
                               style = "color: white; background-color: #7B157A; padding: 6px 12px; display: inline-block;"),
                  #status = "primary",
                  solidHeader = TRUE,
                  sliderInput("flt_age", "Rango de edad:", min = 0, max = 100, value = c(0, 100)),
                  uiOutput("time_slider"),
                  selectizeInput("flt_home", "Estado o región:", choices = estados, multiple = TRUE),
                  selectizeInput("flt_sex", "Sexo:", choices = c("Hombre", "Mujer"), multiple = TRUE),
                  actionButton("generar", "Generar nube de palabras", 
                               class = "btn-primary", 
                               style = "background-color: #7B157A; color: white; width: 100%;"),
                  htmlOutput("record_count")
                ),
                box(
                  width = 8,
                  title = span("Palabras reportadas con más frecuencia",
                               style = "color: white; background-color: #7B157A; padding: 6px 12px; display: inline-block;"),
                  #status = "primary",
                  solidHeader = TRUE,
                  wordcloud2Output("wordcloud", height = "600px")
                )
              ),
              fluidRow(
                box(
                  width = 12,
                  status = "primary",
                  tags$div(
                    style = "margin-top: 10px; font-size: 0.9em; color: #555;",
                    p("Nota: El análisis excluye palabras comunes y términos cortos para enfocarse en conceptos significativos."),
                    p("Datos recopilados a través del Registro Mexicano de lupus.")
                  )
                )
              )
      ),
      
      ### Integrar App Sección 4: calculadora riesgo nefritis e hipertensión
      tabItem(tabName = "sec4", 
              box(
                width = 12,
                h3("Cálculo de probabilidades"),
                p("Esta sección estima la probabilidad de presentar ciertas condiciones en nuestra muestra"),
                fluidRow(
                  titlePanel("Inferencia usando Red Bayesiana"),
                  
                  sidebarLayout(
                    sidebarPanel(
                      selectInput(
                        inputId = "variable_objetivo",
                        label = "Selecciona la variable objetivo:",
                        choices = todas_variables,
                        selected = "Hipertensión"
                      ),
                      
                      checkboxGroupInput(
                        inputId = "vars_evidencia",
                        label = "Selecciona variables de evidencia:",
                        choices = todas_variables,
                        selected = c("Obesidad", "Sobrepeso", "Grupo_etario",
                                     "Ancestria_originaria", "Nivel_Socioeconómico",
                                     "Años_de_retraso_en_diagnóstico", "Ansiedad")
                      ),
                      
                      uiOutput("controles_evidencia"),
                      
                      actionButton("btn_inferir", "Hacer inferencia")
                    ),
                    
                    mainPanel(
                      h4("Probabilidad condicional de la variable objetivo"),
                      verbatimTextOutput("resultado_inferencia")
                    )
                  )
                )
              )
      ),
      
      
      
      ### Integrar App Sección 5: 
      tabItem(tabName = "sec5", h3("Solicitud de datos",
                                   sidebarLayout(
                                     sidebarPanel(
                                       h5("Si requieres datos específicos del Registro Mexicano de Lupus, favor de llenar esta solicitud.")
                                     ),
                                     
                                     mainPanel(
                                       tags$iframe(
                                         src = "https://redcap.link/nqsxtj8n",
                                         width = "100%",
                                         height = "600px",
                                         style = "border:none;"
                                       )
                                     )
                                   )
      ))
    )
  )
)

server <- function(input, output, session) {
  
  ### Reactividad SECCIÓN 1 ######################################################  
  
  # Procesar datos principales
  datos_procesados <- reactive({
    # Mapeo correcto de códigos de estado
    conteo_estados <- Datos() %>%
      mutate(codigo_corregido = case_when(
        home == 9 ~ 10,
        home == 10 ~ 9,
        TRUE ~ home
      )) %>%
      count(codigo_corregido, name = "casos")
    
    # Unir con datos geoespaciales y agregar por región
    datos_mapa <- mexico_sf %>%
      left_join(conteo_estados, by = c("cve_ent_num" = "codigo_corregido")) %>%
      mutate(casos = replace_na(casos, 0))
    
    return(datos_mapa)
  })
  
  # Calcular casos por región
  datos_por_region <- reactive({
    datos_procesados() %>%
      st_drop_geometry() %>%
      group_by(region) %>%
      summarise(Casos = sum(casos, na.rm = TRUE)) %>%
      ungroup() %>%
      filter(region != "Otra") %>%
      arrange(desc(Casos))
  })
  
  datos_filtrados <- reactive({
    if(input$region == "Nacional") {
      datos_procesados()
    } else {
      datos_procesados() %>%
        filter(region == input$region)
    }
  })
  
  # Tabla de casos por región
  output$tabla_regiones <- renderTable({
    datos_por_region()
  }, striped = TRUE, hover = TRUE, bordered = TRUE)
  
  output$resumen_tabla <- renderTable({
    df <- datos_filtrados() %>%
      st_drop_geometry() %>%
      select(Estado = state_name, Casos = casos) %>%
      arrange(desc(Casos))
    
    if(input$region == "Nacional") {
      df <- df %>% head(10)
    }
    
    df
  }, striped = TRUE, hover = TRUE, bordered = TRUE)
  
  output$mapa <- renderPlot({
    datos_sel <- datos_filtrados()
    
    if(input$region == "Nacional") {
      # Mapa nacional coloreado por región con viridis discreto
      ggplot(datos_sel) +
        geom_sf(aes(fill = region), color = "white", size = 0.5) +
        geom_sf_text(aes(label = casos), color = "black", size = 3.5, fontface = "bold") +
        scale_fill_viridis(
          discrete = TRUE,
          option = "plasma",  # Plasma tiene tonos morados/rosas
          name = "Región",
          begin = 0.1,
          end = 0.9,
          direction = -1
        ) +
        labs(title = "Personas con lupus a nivel nacional por región") +
        theme_void() +
        theme(
          plot.title = element_text(hjust = 0.5, size = 18, face = "bold", margin = margin(b = 15)),
          legend.position = "bottom",
          legend.title = element_text(face = "bold"),
          legend.key.width = unit(1.5, "cm")
        )
    } else {
      # Mapa regional con viridis continuo para casos
      ggplot(datos_sel) +
        geom_sf(aes(fill = casos), color = "white", size = 0.5) +
        geom_sf_text(aes(label = casos), color = "black", size = 4.5, fontface = "bold") +
        scale_fill_viridis(
          option = "plasma",
          direction = -1,
          name = "Personas con lupus",
          begin = 0.1,
          end = 0.9
        ) +
        labs(title = paste("Distribución de lupus -", input$region)) +
        theme_void() +
        theme(
          plot.title = element_text(hjust = 0.5, size = 18, face = "bold", margin = margin(b = 15)),
          legend.position = "bottom",
          legend.title = element_text(face = "bold"),
          legend.key.width = unit(1.5, "cm")
        )
    }
  })
  
  output$barplot <- renderPlot({
    datos_sel <- datos_filtrados() %>%
      st_drop_geometry()
    
    if(input$region == "Nacional") {
      # Gráfico de regiones con escala viridis discreta
      datos_region <- datos_por_region()
      
      ggplot(datos_region, aes(x = reorder(region, Casos), y = Casos, fill = region)) +
        geom_col() +
        scale_fill_viridis(
          discrete = TRUE,
          option = "plasma",
          direction = -1,
          begin = 0.1,
          end = 0.9
        ) +
        coord_flip() +
        labs(
          x = "", 
          y = "Número de personas",
          title = "Número de personas por región"
        ) +
        theme_minimal() +
        theme(
          plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
          axis.text.y = element_text(size = 11, face = "bold"),
          axis.text.x = element_text(size = 10),
          axis.title.x = element_text(size = 11, face = "bold"),
          legend.position = "none"
        )
    } else {
      # Gráfico de estados para la región seleccionada
      datos_sel <- datos_sel %>%
        mutate(state_name = fct_reorder(state_name, casos))
      
      ggplot(datos_sel, aes(x = state_name, y = casos, fill = casos)) +
        geom_col() +
        scale_fill_viridis(
          option = "plasma",
          direction = -1,
          begin = 0.1,
          end = 0.9
        ) +
        coord_flip() +
        labs(
          x = "", 
          y = "Número de personas",
          title = "Número de personas por estado"
        ) +
        theme_minimal() +
        theme(
          plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
          axis.text.y = element_text(size = 11),
          axis.text.x = element_text(size = 10),
          axis.title.x = element_text(size = 11, face = "bold"),
          legend.position = "none"
        )
    }
  })
  
  output$downloadData <- downloadHandler(
    filename = function() {
      if(input$region == "Nacional") {
        paste("datos_lupus_nacional_", Sys.Date(), ".csv", sep = "")
      } else {
        paste("datos_lupus_", gsub("[^A-Za-z]", "", input$region), "_", Sys.Date(), ".csv", sep = "")
      }
    },
    content = function(file) {
      write.csv(
        datos_filtrados() %>% 
          st_drop_geometry() %>% 
          select(Estado = state_name, Personas = casos),
        file,
        row.names = FALSE
      )
    }
  )
  
  ### Reactividad sección 2 ###################################################### 
  
  
  # Definir regiones como vectores
  regiones <- list(
    "A. Región Norte" = c(2, 3, 5, 8, 10, 19, 25, 26, 28, 32),
    "B. Región Centro" = c(1, 6, 11, 13, 14, 15, 16, 18, 22, 24, 29),
    "C. Región Sur" = c(4, 7, 12, 17, 20, 21, 23, 27, 30, 31),
    "D. Ciudad de México" = 9
  )
  
  # Variables categóricas disponibles
  get_categorical_vars <- reactive({
    df <- Datos_2()
    vars_interes <- c(
      "sexo", 
      "grupo_etario", 
      "lupus", 
      "estado_de_residencia", 
      "ocupacion", 
      "proveedor_de_serv_de_salud",
      "escolaridad"
    )
    intersect(vars_interes, names(df))
  })
  
  # Selectores dinámicos
  output$x_selector_2 <- renderUI({
    selectInput("x_var_2", "Variable para el eje X:", choices = get_categorical_vars())
  })
  
  output$fill_selector_2 <- renderUI({
    selectInput("fill_var_2", "Variable para agrupar (relleno):", 
                choices = get_categorical_vars(), 
                selected = "lupus")
  })
  
  # Gráfico de barras contiguas
  output$barplot_2 <- renderPlot({
    req(input$x_var_2, input$fill_var_2)
    
    df <- Datos_2()
    
    # Reemplazar 'estado_de_residencia' por región si se usa en el gráfico
    if ("estado_de_residencia" %in% c(input$x_var_2, input$fill_var_2)) {
      df <- df %>%
        mutate(estado_de_residencia = as.integer(as.character(estado_de_residencia))) %>%
        mutate(estado_de_residencia = case_when(
          estado_de_residencia %in% regiones[["A. Región Norte"]] ~ "A. Región Norte",
          estado_de_residencia %in% regiones[["B. Región Centro"]] ~ "B. Región Centro",
          estado_de_residencia %in% regiones[["C. Región Sur"]] ~ "C. Región Sur",
          estado_de_residencia == 9 ~ "D. Ciudad de México",
          TRUE ~ "Otra"
        ))
    }
    
    if (input$x_var_2 == input$fill_var_2) {
      plot.new()
      title("Selecciona dos variables diferentes")
      return()
    }
    
    # Convertir las variables seleccionadas a factor
    df[[input$x_var_2]] <- as.factor(df[[input$x_var_2]])
    df[[input$fill_var_2]] <- as.factor(df[[input$fill_var_2]])
    
    # Eliminar NA en las dos variables seleccionadas
    df <- df %>% filter(!is.na(.data[[input$x_var_2]]), !is.na(.data[[input$fill_var_2]]))
    
    ggplot(df, aes_string(x = input$x_var_2, fill = input$fill_var_2)) +
      geom_bar(position = "dodge") +
      scale_fill_viridis_d(option = "A", direction = -1) +
      labs(
        x = input$x_var_2,
        y = "Número de personas",
        fill = input$fill_var_2,
        title = paste("Conteo de", input$x_var_2, "por", input$fill_var_2)
      ) +
      theme_bw() +
      theme(
        axis.text.x = element_text(angle = 30, hjust = 1),
        plot.title = element_text(face = "bold", hjust = 0.5)
      )
  })
  
  
  ### Reactividad sección 3 ###################################################### 
  
  # Crear slider dinámico para tiempo sin duplicados
  output$time_slider <- renderUI({
    df <- Datos()
    if (nrow(df) == 0) return(NULL)
    
    # Obtener valores únicos y ordenados (incluyendo NA)
    time_vals <- unique(na.omit(df$time_symptoms))
    time_vals <- sort(time_vals)
    
    # Si no hay valores, usar rango 0-1
    if (length(time_vals) == 0) time_vals <- c(0, 1)
    
    sliderInput(
      "flt_time", 
      "Tiempo entre síntomas y diagnóstico (años):",
      min = min(time_vals),
      max = max(time_vals),
      value = range(time_vals),
      step = 1
    )
  })
  
  # Datos filtrados (manejo adecuado de NA)
  filtered_data <- reactive({
    df <- Datos()
    if (nrow(df) == 0) return(data.frame())
    if (is.null(input$flt_time)) return(df)  # Devolver todos si no hay slider
    
    # Preparar selección de estados (incluir NA)
    selected_states <- if (!is.null(input$flt_home) && length(input$flt_home) > 0) {
      which(estados %in% input$flt_home)
    } else {
      1:32
    }
    
    # Preparar selección de sexo (incluir NA)
    selected_sex <- if (!is.null(input$flt_sex) && length(input$flt_sex) > 0) {
      recode(input$flt_sex, "Hombre" = 0, "Mujer" = 1)
    } else {
      c(0, 1)
    }
    
    # Aplicar filtros sin excluir NA
    result <- df %>%
      filter(
        is.na(calculated_age) | (calculated_age >= input$flt_age[1] & calculated_age <= input$flt_age[2]),
        is.na(time_symptoms) | (time_symptoms >= input$flt_time[1] & time_symptoms <= input$flt_time[2]),
        is.na(home) | home %in% selected_states,
        is.na(sex___1) | sex___1 %in% selected_sex
      )
    
    result
  })
  
  # Mostrar número de registros seleccionados
  output$record_count <- renderUI({
    total_original <- nrow(Datos())
    count <- nrow(filtered_data())
    
    msg_total <- paste("Registros totales:", total_original)
    
    if (count == 0) {
      color <- "#E74C3C"
      msg <- "⚠️ No hay registros que coincidan con los filtros seleccionados"
    } else if (count < 10) {
      color <- "#F39C12"
      msg <- paste("ℹ️ Se analizarán", count, "registros (muestra pequeña)")
    } else {
      color <- "#27AE60"
      msg <- paste("✅ Se analizarán", count, "registros")
    }
    
    HTML(paste0(
      "<div style='margin-top:15px;padding:10px;background-color:#F8F9FA;border-radius:5px;border-left:4px solid ", color, ";'>
      <p style='margin:0;color:", color, ";font-weight:bold;'>", msg, "</p></div>"
    ))
  })
  
  # Generar texto para la nube de palabras
  text_input <- eventReactive(input$generar, {
    df <- filtered_data()
    if (nrow(df) == 0) return("")
    
    # Usar siempre la columna "inventariopregunta1" y manejar NA
    responses <- na.omit(df$inventariopregunta1)
    if (length(responses) == 0) return("")
    
    paste(responses, collapse = " ")
  })
  
  # Lista de palabras excluidas
  my_blacklist = c("de", "para", "mi", "donde", "estaba", "desde", "hacer", "sobre", "entre",
                   "estar", "hasta", "estoy", "ahora", "tener", "tiene", "tengo",
                   "había", "sentía", "podía", "veces", "tanto", "estado", "cuando",
                   "además", "varios", "haber", "aparte", "haber", "tenía", "quería", 
                   "muchas","porque")
  
  # Frecuencia de palabras
  word_freq <- eventReactive(input$generar, {
    text <- text_input()
    if (text == "" || nchar(text) < 5) return(data.frame(word = "Sin datos suficientes", n = 1))
    
    freq <- tibble(text = text) %>%
      unnest_tokens(word, text) %>%
      anti_join(stop_words, by = "word") %>%
      filter(nchar(word) >= 5, !word %in% my_blacklist) %>%
      count(word, sort = TRUE) %>%
      filter(n >= 2) %>%
      head(100)
    
    if (nrow(freq) == 0) {
      return(data.frame(word = "No se encontraron palabras relevantes con los filtros actuales", n = 1))
    }
    
    freq
  })
  
  # Renderizar nube de palabras
  output$wordcloud <- renderWordcloud2({
    freq <- word_freq()
    
    if ("word" %in% names(freq) && "n" %in% names(freq)) {
      wordcloud2(freq, 
                 size = 1, 
                 color = rep_len(morados, nrow(freq)), 
                 backgroundColor = "white")
    } else {
      wordcloud2(data.frame(word = "Error al procesar los datos", n = 1), color = "#7B157A")
    }
  })
  
  
  ### Reactividad sección 4 ###################################################### 
  
  
  # Dinámicamente crear controles para las variables de evidencia seleccionadas
  output$controles_evidencia <- renderUI({
    req(input$vars_evidencia)
    
    lapply(input$vars_evidencia, function(var) {
      selectInput(
        inputId = paste0("ev_", var),
        label = paste("Valor para:", var),
        choices = niveles_todas_vars[[var]],
        selected = niveles_todas_vars[[var]][1]
      )
    })
  })
  
  resultado_inferencia <- eventReactive(input$btn_inferir, {
    req(input$variable_objetivo, input$vars_evidencia)
    
    # Recolectar la evidencia
    evidencia <- sapply(input$vars_evidencia, function(var) {
      input[[paste0("ev_", var)]]
    })
    
    names(evidencia) <- input$vars_evidencia
    
    # Aplicar la evidencia al modelo
    modelo_evidencia <- setEvidence(modelo_grain_2, 
                                    nodes = names(evidencia), 
                                    states = as.character(evidencia))
    
    # Realizar la consulta
    querygrain(modelo_evidencia, nodes = input$variable_objetivo, type = "conditional")
  })
  
  output$resultado_inferencia <- renderPrint({
    resultado_inferencia()
  })
  
  
  ### Reactividad sección 5 ###################################################### 
  ## No aplica, solo despliega un formulario en la UI
  
}

shinyApp(ui, server)
