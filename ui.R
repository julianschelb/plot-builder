#
# Diese Applikation soll dabei helfen schnell einen Überblick über Datensätze zu gewinnen.
#

library(shiny)
library(readxl)
library(tidyverse)

# GUI Definition

ui <- fluidPage(
    
    titlePanel("Explorative Datenanalyse"),
    
    sidebarLayout(
        
        sidebarPanel(
            selectInput("inputSelect_Var1", "Variable 1 (x-Achse):",
                        selected = "Alter",
                        choices = listVars),
            selectInput("inputSelect_Var2", "Variable 2 (y-Achse):",
                        selected = "keine",
                        choices = c("keine", listVars)),
            p("Zum Anzeigen von der Verteilung von Variable 1 leer lassen. 
              Sobald eine zweite Variable ausgewählt wird, wird eine Heatmap 
              oder ein Scatter-Plot angezeigt"),
            hr(),
            selectInput("inputSelect_fill", "Variable für Füllfarbe:",
                        selected = "keine",
                        choices = c("keine", listVars)),
            # selectInput("inputSelect_color", "Variable für Umrandung:",
            #             selected = "keine",
            #            choices = c("keine", listVars)),
            # selectInput("inputSelect_size", "Variable für Größe:",
            #             selected = "keine",
            #             choices = c("keine", listVars)),
            # selectInput("inputSelect_shape", "Variable für Form:",
            #             selected = "keine",
            #             choices = c("keine", listVars)),
            selectInput("inputSelect_facet", "Plot pro Wert von Variable:",
                        selected = "keine",
                        choices = c("keine", listVars)),
            hr(),
            fluidRow(
                column(6,
                       selectInput("inputSelect_theme", "Theme:",
                                   selected = "default",
                                   choices = listThemes)
                ),
                column(6,
                        selectInput("inputSelect_palette", "Farbpalette:",
                                    selected = "Paired",
                                    choices = listColorPalettes)
                )
            ),
            hr(),
            fluidRow(
                column(6,
                       selectInput("inputSelect_legendPosition", "Position der Legende:",
                                   selected = "right",
                                   choices = listLegendPosition)
                ),
                column(6,
                       checkboxInput("inputCheck_FlipCoord", label = "Achsen tauschen", value = FALSE),
                       checkboxInput("inputCheck_rmNA", label = "NA Werte entf.", value = FALSE)
                )
            ),
            fluidRow(
                column(6,
                       textInput("inputText_xDesc", "x-Achsen Label"),
                       selectInput("inputSelect_xLabelRotation", "x-Achsen Beschriftung drehen um",
                                   selected = 0,
                                   choices = listAxisRotation)
                ),
                column(6,
                       textInput("inputText_yDesc", "y-Achsen Label"),
                       selectInput("inputSelect_yLabelRotation", "y-Achsen Beschriftung drehen um",
                                   selected = 0,
                                   choices = listAxisRotation)
                )
            ),
            checkboxInput("inputCheck_Density", label = "Density Plot anstelle Histogramm", value = FALSE),
            checkboxInput("inputCheck_Pie", label = "Pie Plot anstelle Balkendiagramm", value = FALSE),
            hr(),
            textInput("inputText_Title", "Titel"),
            textInput("inputText_Subtitle", "Untertitel"),
            textInput("inputText_Caption", "Beschreibung"),
            textInput("inputText_Tag", "Numerierung")
        ),
        
        mainPanel(
            tabsetPanel(type = "pill",
                        tabPanel("Plot",
                            #br(),
                            plotOutput("plotGenerator")   
                        ),
                        tabPanel("Tabelle",
                                 br(),
                                 tableOutput("tableGenerator"),
                                 br(),
                                 downloadButton('buttonDownloadData', 'Download')
                        )
            )
        )
    )
)