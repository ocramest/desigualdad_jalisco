library(tidyverse)
library(sf)
library(leaflet)
library(sp)
library(readr)
library(shiny)
library(reldist)
library(scales)
library(wesanderson)
library(shinyWidgets)
library(shinybusy)

# Municipios: ----
muns <- list('Acatic'='001','Acatlán de Juárez'='002','Ahualulco de Mercado'='003',
             'Amacueca'='004','Amatitán'='005','Ameca'='006','San Juanito de Escobedo'='007',
             'Arandas'='008','El Arenal'='009','Atemajac de Brizuela'='010','Atengo'='011',
             'Atenguillo'='012','Atotonilco el Alto'='013','Atoyac'='014','Autlán de Navarro'='015',
             'Ayotlán'='016','Ayutla'='017','La Barca'='018','Bolaños'='019','Cabo Corrientes'='020',
             'Casimiro Castillo'='021','Cihuatlán'='022','Zapotlán el Grande'='023','Cocula'='024',
             'Colotlán'='025','Concepción de Buenos Aires'='026','Cuautitlán de García Barragán'='027',
             'Cuautla'='028','Cuquío'='029','Chapala'='030','Chimaltitán'='031','Chiquilistlán'='032',
             'Degollado'='033','Ejutla'='034','Encarnación de Díaz'='035','Etzatlán'='036','El Grullo'='037',
             'Guachinango'='038','Guadalajara'='039','Hostotipaquillo'='040','Huejúcar'='041',
             'Huejuquilla el Alto'='042','La Huerta'='043','Ixtlahuacán de los Membrillos'='044',
             'Ixtlahuacán del Río'='045','Jalostotitlán'='046','Jamay'='047','Jesús María'='048',
             'Jilotlán de los Dolores'='049','Jocotepec'='050','Juanacatlán'='051','Juchitlán'='052',
             'Lagos de Moreno'='053','El Limón'='054','Magdalena'='055','Santa María del Oro'='056',
             'La Manzanilla de la Paz'='057','Mascota'='058','Mazamitla'='059','Mexticacán'='060',
             'Mezquitic'='061','Mixtlán'='062','Ocotlán'='063','Ojuelos de Jalisco'='064','Pihuamo'='065',
             'Poncitlán'='066','Puerto Vallarta'='067','Villa Purificación'='068','Quitupan'='069','El Salto'='070',
             'San Cristóbal de la Barranca'='071','San Diego de Alejandría'='072','San Juan de los Lagos'='073',
             'San Julián'='074','San Marcos'='075','San Martín de Bolaños'='076','San Martín Hidalgo'='077',
             'San Miguel el Alto'='078','Gómez Farías'='079','San Sebastián del Oeste'='080',
             'Santa María de los Ángeles'='081','Sayula'='082','Tala'='083','Talpa de Allende'='084',
             'Tamazula de Gordiano'='085','Tapalpa'='086','Tecalitlán'='087','Tecolotlán'='088',
             'Techaluta de Montenegro'='089','Tenamaxtlán'='090','Teocaltiche'='091','Teocuitatlán de Corona'='092',
             'Tepatitlán de Morelos'='093','Tequila'='094','Teuchitlán'='095','Tizapán el Alto'='096',
             'Tlajomulco de Zúñiga'='097','San Pedro Tlaquepaque'='098','Tolimán'='099','Tomatlán'='100',
             'Tonalá'='101','Tonaya'='102','Tonila'='103','Totatiche'='104','Tototlán'='105','Tuxcacuesco'='106',
             'Tuxcueca'='107','Tuxpan'='108','Unión de San Antonio'='109','Unión de Tula'='110','Valle de Guadalupe'='111',
             'Valle de Juárez'='112','San Gabriel'='113','Villa Corona'='114','Villa Guerrero'='115','Villa Hidalgo'='116',
             'Cañadas de Obregón'='117','Yahualica de González Gallo'='118','Zacoalco de Torres'='119',
             'Zapopan'='120','Zapotiltic'='121','Zapotitlán de Vadillo'='122','Zapotlán del Rey'='123',
             'Zapotlanejo'='124','San Ignacio Cerro Gordo'='125')

# Variables: ----


vrbls <- list("Índice" = "indice",
             "Sin limitaciones" = "sin_disc",
              "Escolaridad promedio" = "escolaridad",
              "Personas ocupadas" = "ocupados",
              "Con afiliación (salud)" = "salud",
              "Hijos p/c mujer" = "fecundidad",
              "Hacinamiento" = "hacinamiento", 
              "Piso diferente a tierra" = "piso",
              "Con energía elec." = "energia",
              "Con agua entubada" = "agua",
              "Con drenaje" = "drenaje",
              "Con celular" = "celular",
              "Con internet" = "internet",
              "Con tinaco" = "tinaco",
              "Con escusado" = "escusado",
              "Con refrigerador" = "refri",
              "Con lavadora" = "lavadora",
              "Con auto/camioneta" = "auto"
)

# Descripción: ----

descr <- list("indice" = "Valores más altos muestran directa o indirectamente una mayor capacidad de ejercer derechos por parte de los individuos.",
             "sin_disc" = "Porcentaje de habitantes sin discapacidades, limitaciones, problemas o condiciones mentales.",
              "escolaridad" = "Suma del total de años escolares aprobados por las personas mayores a 15 años de edad, entre las personas del mismo grupo de edad.",
              "ocupados" = "Proporción de personas mayores a 12 años que tenían trabajo en la semana de referencia.",
              "salud" = "Proporción de personas con derecho a recibir servicios médicos en alguna institución de salud pública o privada.",
              "fecundidad" = "Total de hijos nacidos vivos de las mujeres de 12 o más años de edad, entre el total de mujeres del mismo grupo.",
              "hacinamiento" = "Número de personas que residen en viviendas particulares habitadas entre el número de cuartos de esas viviendas.",
              "piso" = "Porcentaje de viviendas particulares habitadas con piso de cemento o firme, madera, mosaico u otro material.",
              "energia" = "Proporción de viviendas particulares habitadas que tienen luz eléctrica.",
              "agua" = "Porcentaje de viviendas particulares habitadas que tienen disponibilidad de agua entubada dentro de la vivienda, o fuera de la vivienda, pero dentro del terreno.",
              "drenaje" = "Proporción de viviendas particulares habitadas que tienen drenaje conectado a la red pública, fosa séptica, barranca, grieta, río, lago o mar.",
              "celular" = "Proporción de viviendas particulares habitadas que tienen teléfono celular.",
              "internet" = "Porcentaje de viviendas particulares habitadas que tienen servicio de internet.",
              "tinaco" = "Proporción de viviendas particulares habitadas que disponen de tinaco.",
              "escusado" = "Porcentaje de viviendas particulares habitadas que tienen escusado, retrete, sanitario, letrina u hoyo negro.",
              "refri" = "Proporción de viviendas particulares habitadas que tienen refrigerador.",
              "lavadora" = "Proporción de viviendas particulares habitadas que tienen lavadora.",
              "auto" = "Proporción de viviendas particulares habitadas que tienen automóvil o camioneta."
)

Lista.ordinales <- list("primer" = 1,
                        "segundo" = 2,
                        "tercer" = 3,
                        "cuarto" = 4,
                        "quinto" = 5,
                        "sexto" = 6,
                        "séptimo" = 7,
                        "octavo" = 8,
                        "noveno" = 9,
                        "décimo" = 10)

### ui:----

ui <- fluidPage(
                tags$head(
                    tags$link(rel="stylesheet", type="text/css", href="style.css"),
                    tags$title("Distribución espacial de la desigualdad en Jalisco"),
                    tags$header(busy_start_up(
                        loader = spin_epic("orbit", color = "#ffffff"),
                        text = "",
                        timeout = 5000,
                        color = "#fbbb27",
                        background = "#4f4f4f"
                    ))
                ),
                
                fluidRow(id = "fila1",
                    column(width = 5,
                           tags$div(class = "col1",
                               selectInput("x",
                                           "¿Qué variable te interesa visualizar?",
                                           vrbls,
                                           selected = "indice"),
                               
                               textOutput("descrip"),
                               
                               uiOutput("munics"),
                               
                               actionButton("amg", "Seleccionar Área Metropolitana de Guadalajara"),
                               
                               sliderInput("slider", 
                                           "¿En cuántos grupos quieres dividir a la población, a partir de la variable seleccionada?",
                                           min = 2, 
                                           max = 10,
                                           value = 5
                               ),
                               #¿Cuántas y cuáles pestañas puede ver el usuario?
                               uiOutput("caja_grupos")
                           )
                           
                           
                           ),
                    column(width = 7, offset = 0,
                           tags$div(class = "col2",
                               leafletOutput("map", height = "73rem")
                           )
                    )
                )
)



#server:----

server <- function(input, output, session) {
    
    amg_muns <- c("002", "039", "044", "051", "070", "097",
                 "098", "101", "120", "124")
    
    output$munics <- renderUI({
            pickerInput(
                inputId = "mun",
                label = "¿Quieres seleccionar alguno o varios municipios?", 
                choices = muns,
                options = list(
                    `actions-box` = TRUE,
                    `deselect-all-text` = "Eliminar selección",
                    `select-all-text` = "Seleccionar todo",
                    `none-selected-text` = "Ningún municipio seleccionado"), 
                multiple = TRUE,
                selected = c("002", "039", "044", "051", "070", "097",
                             "098", "101", "120", "124")
            )
    })
    
    observeEvent(input$amg, {
        output$munics <- renderUI({
            pickerInput(
                inputId = "mun",
                label = "¿Quieres seleccionar alguno o varios municipios?", 
                choices = muns,
                options = list(
                    `actions-box` = TRUE,
                    `deselect-all-text` = "Eliminar selección",
                    `select-all-text` = "Seleccionar todo",
                    `none-selected-text` = "Ningún municipio seleccionado"), 
                multiple = TRUE,
                selected = c("002", "039", "044", "051", "070", "097",
                             "098", "101", "120", "124")
            )
        })
    })
    
    final <- read_rds("data/data.rds")
    
    
    # Descripción de la variable elegida:
    
    output$descrip <- renderText({
        descr[[which(vrbls==input$x)]]
    })
    

    # Mapas: ----
    agebs <- st_read("data/AGEBs-Geo/AGEBs_Geogr.shp") %>%
        select(-c("CVEGEO","CVE_ENT", "CVE_LOC")) %>%
        setNames(c("mun", "ageb", "geometry")) %>%
        mutate(agebmun = paste0(ageb, "_", as.numeric(mun)))
    
    db <- reactive({
        inner_join(agebs, final, by = "agebmun")
    })
    
    
    ## Mapa base
    output$map <- renderLeaflet({
        leaflet() %>%
            addTiles()
    })
    
    ##Depende de la opción elegida por el usuario
    observe({
    if(!is.null(input$mun)){
        
        Factor_e <- input$x
        cuantiles <- input$slider
        
       
            if(input$x != "indice"){
                    
                    
                    datos_mapa <- db() %>%
                        filter(mun %in% input$mun) %>%
                        mutate(cuantil = NA) %>% 
                        drop_na(Factor_e)
                    
                    for (i in 0:(cuantiles-1)) {
                        limite <- wtd.quantile(select_at(datos_mapa, Factor_e)[[1]],
                                               q = (i/cuantiles),
                                               weight = datos_mapa$poblacion,
                                               na.rm = T)
                        datos_mapa$cuantil[select_at(datos_mapa, Factor_e)[[1]]>=limite[[1]]] <- i+1
                    }
                    
            } else {
                    datos_mapa <- db() %>%
                        filter(mun %in% input$mun)
                    
                    for (i in 0:(cuantiles-1)) {
                        limite <- wtd.quantile(datos_mapa$indice,
                                               q = (i/cuantiles),
                                               weight = datos_mapa$poblacion,
                                               na.rm = T)
                        datos_mapa$cuantil[datos_mapa$indice>=limite[[1]]] <- i+1
                    }
                
            }
 
        # Para el zoom automático
        zoom <- st_bbox(datos_mapa) %>%
            as.vector()
        
        
        paleta <- colorFactor(as.character(wes_palette("Zissou1",  type = "continuous")),
                              domain = 1:cuantiles, reverse = T)
        lab_legend <- 1:cuantiles
        
        # El mapa
        leafletProxy("map", data = datos_mapa) %>%
            clearMarkers() %>%
            clearControls() %>%
            clearShapes() %>%
            addPolygons(color = ~paleta(cuantil), opacity = .3, weight = .4,
                        fillColor = ~paleta(cuantil),fillOpacity = 0.6,
                        popup = paste0('<strong>Valor:</strong> ', 
                                       round(as.data.frame(datos_mapa)[,Factor_e],2),'<br/>',
                                       '<strong>Grupo: </strong>',datos_mapa$cuantil),
                        highlightOptions = highlightOptions(color = "white", weight = 2,
                                                            bringToFront = TRUE)) %>%
            addLegend("bottomleft",
                      pal = paleta,
                      values = as.factor(lab_legend), 
                      title = "") %>%
            fitBounds(zoom[1],
                      zoom[2],
                      zoom[3],
                      zoom[4])
        
        # Pestañas condicionales ¿cuántas quiere ver el usuario?
        output$caja_grupos <- renderUI({
            do.call(tabsetPanel, 
                        c(id='grupos',
                          lapply(1:cuantiles, function(i) {
                              tabPanel(
                                  title=paste0(i), 
                                  htmlOutput(paste0('t_grupo',i)),
                                  if (i < 6) {
                                      #uiOutput(paste0("videin_",i))
                                  }
                                  )
                          }
                          )
                        )
                )
            })
        
        
        # Cuadro de texto que depende de los grupos
        Texto_descr <- vector("list", max(unique(datos_mapa$cuantil[datos_mapa$cuantil != "NA"]), na.rm = T))
        for (i in unique(datos_mapa$cuantil[!is.na(datos_mapa$cuantil)])) {
            Texto_descr[[i]] <- paste0(
                "<div align = 'justify'><br></br>El grupo ", i, ", con índice promedio igual a ", "<strong>",
                round(weighted.mean(datos_mapa$indice[datos_mapa$cuantil==i],
                                    datos_mapa$poblacion[datos_mapa$cuantil==i], na.rm = T),2),
                "</strong>"," representa, aproximadamente, a <strong>", 
                percent(sum(datos_mapa$poblacion[datos_mapa$cuantil==i], na.rm = T)/
                            sum(datos_mapa$poblacion, na.rm = T), .1),
                "</strong> de la población. Está conformado por ",
                "<strong>",
                comma(sum(datos_mapa$poblacion[datos_mapa$cuantil==i], na.rm = T)),
                "</strong>",
                " habitantes, que en promedio cuentan con las siguientes caracteríticas:<br/><br/>",
                "<ul><li>En sus viviendas habitan ",
                "<strong>",
                round(weighted.mean(datos_mapa$hacinamiento[datos_mapa$cuantil==i],
                                    datos_mapa$poblacion[datos_mapa$cuantil==i], na.rm = T),2),
                "</strong>",
                " personas por cuarto.</li>
                <li>Cuentan con ",
                "<strong>",
                round(weighted.mean(datos_mapa$escolaridad[datos_mapa$cuantil==i],
                                    datos_mapa$poblacion[datos_mapa$cuantil==i], na.rm = T),2),
                "</strong>",
                " años de escolaridad promedio.</li>
                <li>El ",
                "<strong>",
                percent(weighted.mean(datos_mapa$salud[datos_mapa$cuantil==i],
                                      datos_mapa$poblacion[datos_mapa$cuantil==i], na.rm = T), .01),
                "</strong>",
                " están afiliados a algún servicio de salud.</li>",
                "<li><strong>",
                percent(weighted.mean(datos_mapa$internet[datos_mapa$cuantil==i],
                                      datos_mapa$poblacion[datos_mapa$cuantil==i], na.rm = T), .01),
                "</strong>",
                " habita una vivienda con acceso a internet.</li>",
                "</li>
                <li>Las mujeres mayores de 12 años en su AGEB tienen ",
                "<strong>",
                round(weighted.mean(datos_mapa$fecundidad[datos_mapa$cuantil==i],
                                      datos_mapa$poblacion[datos_mapa$cuantil==i], na.rm = T),2),
                "</strong>",
                " hijos.</li></ul></div>"
            )
    
        }
        
        output$t_grupo1 <- renderText({
            Texto_descr[[1]]
        })
        output$t_grupo2 <- renderText({
            Texto_descr[[2]]
        })
        output$t_grupo3 <- renderText({
            Texto_descr[[3]]
        })
        output$t_grupo4 <- renderText({
            Texto_descr[[4]]
        })
        output$t_grupo5 <- renderText({
            Texto_descr[[5]]
        })
        output$t_grupo6 <- renderText({
            Texto_descr[[6]]
        })
        output$t_grupo7 <- renderText({
            Texto_descr[[7]]
        })
        output$t_grupo8 <- renderText({
            Texto_descr[[8]]
        })
        output$t_grupo9 <- renderText({
            Texto_descr[[9]]
        })
        output$t_grupo10 <- renderText({
            Texto_descr[[10]]
        })
    
        
        # Aparecerá el video si se cumple la condición deseada:
        if (cuantiles == 5 & input$mun == c("002", "039", "044", "051", "070", "097",
                                            "098", "101", "120", "124")) {
            output$videin_1 <- renderUI({
                conditionalPanel(condition = "input.slider == '5'",
                                 actionButton("Video1", "",
                                              icon = icon("play-circle"))
                )
            })
            output$videin_2 <- renderUI({
                conditionalPanel(condition = "input.slider == '5'",
                                 actionButton("Video2", "",
                                              icon = icon("play-circle"))
                )
            })
            output$videin_3 <- renderUI({
                conditionalPanel(condition = "input.slider == '5'",
                                 actionButton("Video3", "",
                                              icon = icon("play-circle"))
                )
            })
            output$videin_4 <- renderUI({
                conditionalPanel(condition = "input.slider == '5'",
                                 actionButton("Video4", "",
                                              icon = icon("play-circle"))
                )
            })
            output$videin_5 <- renderUI({
                conditionalPanel(condition = "input.slider == '5'",
                                 actionButton("Video5", "",
                                              icon = icon("play-circle"))
                )
            })
        } else {
            output$videin_1 <- renderUI({})
            output$videin_2 <- renderUI({})
            output$videin_3 <- renderUI({})
            output$videin_4 <- renderUI({})
            output$videin_5 <- renderUI({})
        }
        
    } else {
        leafletProxy("map") %>%
            clearMarkers() %>%
            clearControls() %>%
            clearShapes()
    }
    })
    
}



# Run the application 
shinyApp(ui = ui, server = server)
