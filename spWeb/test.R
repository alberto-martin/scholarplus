# to store functions and other code
library(shiny)
library(shinyjs)
library(shinydashboard)
library(bootstrap)
library(data.table)
library(dplyr)
library(rjson)
library(purrr)
library(tidyverse)
library(jsonlite)
library(plotly)

# READ JSON
authorData <- fromJSON("data/kyTHOh0AAAAJ.json")


# AUTHOR IMAGE
# COMPROBAMOS SI EL DIRECTORIO PARA GUARDAR LAS IMAGENES EXISTE
# Y SI NO EXISTE SE CREA
profImgDir <- 'ScholarP/www/profile_images'
if (!dir.exists(profImgDir)) {
  dir.create(profImgDir)
}

# ALMACENAMOS LA URL LA IMAGEN
srcImage <- authorData$author$thumbnail

# DESCARGAMOS LA IMAGEN COMPROBANDO PRIMERO SI YA LA TENEMOS
# EN TAL CASO NO LA VOLVEMOS A DESCARGAR
pathAuthImage <- paste0(profImgDir,'/',
                    authorData$search_parameters$author_id,
                    '.jpg')

if(!file.exists(pathAuthImage)) {
  download.file(srcImage,
                pathAuthImage,
                mode = 'wb')
}

# CREAMOS LA VARIABLE PARA PASARLE AL SERVER LA IMAGEN DESCARGADA
pathImage <- paste0('profile_images/',
                    authorData$search_parameters$author_id,
                    '.jpg')

# AUTHOR NAME
authorName <- authorData$author$name

# AUTHOR AFF
authorAff <- authorData$author$affiliations

# SERPAPI NO EXTRAE EL ENLACEDE LA UNIVERSIDAD

# AUTHOR EMAIL
emailPrevText <- authorData$author$email

emailsURLs <- authorData$author$website

# AUTHOR AREAS
areaNames <- authorData$author$interests$title

areaURLs <- authorData$author$interests$link

# CO-AUTHORS
top5Coll <- head(authorData$co_authors$name, 5)

# METRICS TABLE
metTabCitTot <- authorData$cited_by$table$citations$all[1]
metTabCit5y <- authorData$cited_by$table$citations[2][[1]][1]
metTabHiTot <- authorData$cited_by$table$h_index$all[2]
metTabHi5y <- authorData$cited_by$table$h_index[2][[1]][2]
metTabHi10Tot <- authorData$cited_by$table$i10_index$all[3]
metTabHi105y <- authorData$cited_by$table$i10_index[2][[1]][3]

# DOCUMENTS
authorArticles <- authorData$articles
authorData$articles$title[1]
authorData$articles$link[1]
authorData$articles$authors[1]
authorData$articles$publication[1]
authorData$articles$cited_by$value[1]
authorData$articles$cited_by$link[1]

# CITES PLOT
plotData <- tail(authorData$cited_by$graph, 7)

# CITED BY
id_cita <- strsplit(authorData$articles$cited_by$cites_id[1], ",")[[1]][1]
citesData <- fromJSON(paste0("cites/", id_cita, ".json"), flatten = FALSE)

# CITING AUTHORS
citingAuthors <- list()
citingAuthorsIds <- list()
for (a in 1:length(citesData$organic_results$publication_info$authors)) {
  citingAuthor <- citesData$organic_results$publication_info$authors[[a]]$name
  citingAuthorId <- citesData$organic_results$publication_info$authors[[a]]$author_id
  for (i in 1:length(citingAuthor)) {
    author <- citingAuthor[[i]]
    if (!(citingAuthorId %in% citingAuthorsIds)) {
      citingAuthorsIds <- c(citingAuthorsIds, citingAuthorId)
      citingAuthors <-  c(citingAuthors, author)
    }
    
  }
}

top5citingAuthors <- head(citingAuthors, 5)

p <- plot_ly(mtcars, x = ~mpg, y = ~wt, type = 'scatter', mode = 'markers')
