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

generate_images <- function(x) {
  # Cas où ni "P" ni "D" ne sont présentes, retourner un tiret '-'
  if (!grepl("P|D", x)) {
    return("-")
  }
  
  # Initialisation de la liste d'images
  images <- list()
  
  # Si "D" est présent, ajouter "shoes-D.svg" autant de fois qu'il y a "D"
  if (grepl("D", x)) {
    d_count <- stringr::str_count(x, "D")  # Compter le nombre de "D"
    images <- append(images, rep(local_image(filename = "shoes-D.svg", height = 15), d_count))
  }
  
  # Si "P" est présent, ajouter "shoes-P.svg" autant de fois qu'il y a "P"
  if (grepl("P", x)) {
    p_count <- stringr::str_count(x, "P")  # Compter le nombre de "P"
    images <- append(images, rep(local_image(filename = "shoes-P.svg", height = 15), p_count))
  }
  
  # Retourner les images sous forme de HTML
  return(paste(images, collapse = " "))
}