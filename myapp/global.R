library(shiny)
library(dplyr)
library(ggplot2)
library(gt)
library(gtExtras)
library(formattable)
library(RColorBrewer)
library(shinythemes)
library(shinyjs)
library(scales)
library(shinydashboard)
library(data.table)
library(stringr)

up_arrow <- "<span style=\"color:green\">&#9650;</span>"
down_arrow <- "<span style=\"color:red\">&#9660;</span>"

# Fonction pour générer les images selon les valeurs
generate_images <- function(x) {
  if (!grepl("P|D", x)) {
    return("-")
  }
  
  images <- list()
  
  # Exemple : utiliser un chemin complet
  if (grepl("D", x)) {
    d_count <- stringr::str_count(x, "D")
    images <- append(images, rep(local_image(filename = "shoes-D.svg", height = 15), d_count))
  }
  
  if (grepl("P", x)) {
    p_count <- stringr::str_count(x, "P")
    images <- append(images, rep(local_image(filename = "shoes-P.svg", height = 15), p_count))
  }
  
  return(paste(images, collapse = " "))
}


