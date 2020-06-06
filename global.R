#
# Diese Applikation soll dabei helfen schnell einen Überblick über Datensätze zu gewinnen.
#

library(readxl)
library(tidyverse)
library(summarytools)
library(RColorBrewer)
library(kableExtra)
library(treemapify)
library(shiny)
library(shinycssloaders)

# Für Senkey Diagramm
library(viridis)
library(patchwork)
library(hrbrthemes)
library(circlize)
library(networkD3)

source("lib.R")


# Daten einlesen aus Excel Datei

if (file.exists("../data/Daten.xlsx")) {
  df <- read_excel("../data/Daten.xlsx")
} else{
  df <- mpg
}

# Globale Listen definieren
listVars <- colnames(df)
listVarsChar <- colnames(df %>% select_if(~!is.numeric(.)))
listThemes <- c("classic", "schwarz-weiß", "minimal", "light", "default")
listColorPalettes <- rownames(brewer.pal.info)
listStat <- c("sum", "mean")
listLegendPosition <- c("bottom", "right", "left", "top")
listAxisRotation <- c(0, 45, 90)

# Optionen für die Bibliothek "summarytools" 
st_options(bootstrap.css     = FALSE,       # Already part of the theme so no need for it
           plain.ascii       = FALSE,       # One of the essential settings
           style             = "rmarkdown", # Idem.
           dfSummary.silent  = TRUE,        # Suppresses messages about temporary files
           footnote          = NA,          # Keeping the results minimalistic
           subtitle.emphasis = FALSE)       # For the vignette theme, this gives better results.