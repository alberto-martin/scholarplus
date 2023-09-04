if (interactive()) {

server <- function(input, output) {

  # INDEX.HTML
    # Fila 1, Google Scholar
      r1HTitle <- "Google Scholar"
      output$r1HTitle <- renderText({
        r1HTitle
      })
  
    # Fila 2, CrossRef y OpenAlex
      r2HTitle1 <- "CrossRef"
      r2HTitle2 <- "OpenAlex"
      
      output$r2HTitle1 <- renderText({
        r2HTitle1
      })
      
      output$r2HTitle2 <- renderText({
        r2HTitle2
      })
    
    # Fila 3, Web of Science
      r3HTitle <- "Web of Science"
      output$r3HTitle <- renderText({
        r3HTitle
      })
    
    # Fila 4, Scopus
      r4HTitle <- "Scopus"
      output$r4HTitle <- renderText({
        r4HTitle
      })
  
  # AUTHORPROFILE.HTML
    # Cabecera del Documento
      # Foto del autor
        # Creación de la Etiqueta
          pathImage <- pathImage
          
          compoImage <- paste('<img id="authorPic" src="',
                              pathImage,
                              '" alt="Foto del autor" title="Foto del autor">')
        
        # Output
          output$c1AuthorPic <- renderUI({
            HTML(compoImage)
          })
      
      # Cuerpo central
        # Nobmre del Autor
          authorName <- authorName
          output$authorName <- renderText({
            authorName
          })
        
        # Afiliación del Autor
          authorAff <- authorAff
          output$authorAff <- renderText({
            authorAff
          })
        
        # # Universidades del Autor
        #   # Nombre de las Universidades
        #     univsName <- c(
        #       "Universidad 1",
        #       "Universidad 2"
        #     )
        # 
        #   # URLs de las Universidades
        #     univsURLs <- c(
        #       "https://scholar.google.com/citations?view_op=view_org&hl=es&org=12444751036330984032",
        #       "https://scholar.google.com/citations?view_op=view_org&hl=es&org=3385481936956910416"
        #     )
        # 
        #   # Clase de los botones
        #     uniBtn <- "btn btn-outline-dark"
        # 
        #   # Bucle para crear un boton por cada universidad
        #     univs <- ""
        #     for (n in 1:length(univsName)) {
        #       univ <- a(univsName[n], href=univsURLs[n], class=uniBtn)
        #       univs <- paste(univs, univ)
        #     }
        # 
        #   # Output de los botones de las universidades
        #     output$authorUni <- renderUI({
        #       HTML(univs)
        #     })
            
        # Dirección de Correo Electrónico del Autor
          # Texto previo
            emailPrevText <- "Direcciones de correo verificadas -"
            output$emailPrevText <- renderText({
              emailPrevText
            })
            
          # Links de los emails
            # Nombre de los emails
              emailsName <- c(
                "Página Principal"
              )
            
            # URLs de los emails
              emailsURLs <- emailsURLs
            
            # Clase de los enlaces
              emailBtn <- "btn btn-sm link-dark link-first-level pb-2"
            
            # Bucle para crear un boton por cada universidad
              emails <- ""
              for (n in 1:length(emailsName)) {
                email <- a(emailsName[n], href=emailsURLs[n], class=emailBtn, title="Email verificado")
                emails <- paste(emails, email)
              }
            
            # Output de los emails
              output$authorEmailLink <- renderUI({
                HTML(emails)
              })
          
        # Areas temáticas donde publica el Autor
          # Nombre de las areas
            areaNames <- areaNames
            
          # URLs de las Universidades
            areaURLs <- areaURLs
            
          # Clase de los botones
            areaBtn <- "btn btn-outline-dark btn-second-level"
            
          # Bucle para crear un boton por cada universidad
            areas <- ""
            for (n in 1:length(areaNames)) {
              area <- a(areaNames[n], href=areaURLs[n], class=areaBtn)
              areas <- paste(areas, area)
            }
            
          # Output de los botones de las universidades
            output$authorAreas <- renderUI({
              HTML(areas)
            })
        
      # Nivel de extracción de datos y Confiabilidad del perfil
        # Extracción
          # Comprobación del nivel de extracción de datos
            extrLevel <- 1
            
            if(extrLevel == 1) {
              btnGroup <- '<button class="btn btn-warning disabled" id="enriched" type="button">
                                Enriquecido
                              </button>
                              <button class="btn btn-success disabled" id="complet" type="button">
                                Completo
                              </button>
                              <button class="btn btn-primary active" id="basic" type="button" style="font-weight: bold;">
                                Básico
                              </button>'
            } else if(extrLevel == 2) {
              btnGroup <- '<button class="btn btn-warning disabled" id="enriched" type="button">
                                Enriquecido
                              </button>
                              <button class="btn btn-success active" id="complet" type="button" style="font-weight: bold;">
                                Completo
                              </button>
                              <button class="btn btn-primary disabled" id="basic" type="button">
                                Básico
                              </button>'
            } else if(extrLevel == 3) {
              btnGroup <- '<button class="btn btn-warning active" id="enriched" type="button" style="font-weight: bold;">
                                Enriquecido
                              </button>
                              <button class="btn btn-success disabled" id="complet" type="button">
                                Completo
                              </button>
                              <button class="btn btn-primary disabled" id="basic" type="button">
                                Básico
                              </button>'
            } else {
              btnGroup <- '<button class="btn btn-secondary active" id="enriched" type="button" style="font-weight: bold;">
                                No determinado
                              </button>'
            }
            
          # Output
            output$dataCompLevel <- renderUI({
              HTML(btnGroup)
            })
            
        # Confiabilidad
          # Comprobación del nivel de confiabildiad
            reliability <- 1
            
            if(reliability == 1){
              reliabilityName <- "CONFIABLE"
              reliabilityURL <- "###"
              reliabilityClass <- "btn btn-success active border rounded shadow-sm"
            } else if (reliability == 2) {
              reliabilityName <- "DUDOSO"
              reliabilityURL <- "###"
              reliabilityClass <- "btn btn-warning active border rounded shadow-sm"
            } else if (reliability == 3) {
              reliabilityName <- "NO CONFIABLE"
              reliabilityURL <- "###"
              reliabilityClass <- "btn btn-danger active border rounded shadow-sm"
            } else {
              reliabilityName <- "NO SE HA PODIDO DETERMINAR"
              reliabilityURL <- "###"
              reliabilityClass <- "btn btn-secondary active border rounded shadow-sm"
            }
          
          # Output
            realiabilityLink<-  as.character(a(reliabilityName, href=reliabilityURL, class=reliabilityClass))
            
            output$realiabilityBtn <- renderUI({
              HTML(realiabilityLink)
            })
        
    # Cuerpo principal del documento
      # Columna publicaciones
        # Ventana de Publicación
          output$startdate <- renderText({
            as.character(input$pubWindow[1])
          })
          output$enddate <- renderText({
            as.character(input$pubWindow[2])
          })
          citWindowForm <- dateRangeInput("pubWindow", label=h3("Ventana de Publicación:"),
                                          start="2001-01-01",
                                          end="2010-12-31")
        # Colaboradores
          # Datos
            # colabDataExample <- c('Colaborador 1 (#)',
            #                       'Colaborador 2 (#)',
            #                       'Colaborador 3 (#)',
            #                       'Colaborador 4 (#)',
            #                       'Colaborador 5 (#)')
            colabDataExample <- top5Coll
            colabTable <- data.table(colaboradores = colabDataExample)

          # Output
            output$collTable <- renderTable({colabTable})

        # Revistas
          # Datos
            jourDataExample <- c('Revista 1 (#)',
                                  'Revista 2 (#)',
                                  'Revista 3 (#)',
                                  'Revista 4 (#)',
                                  'Revista 5 (#)')
            journalTable <- data.table(revistas = jourDataExample)
              
          # Output
            output$jourTable <- renderTable({journalTable})
         
        
      # Columna central
        # Tabla Documentos
          documents <- ""
          for (n in 1:count(authorArticles)[[1]]) {
            document <- paste0('<tr><td><h6><a href="',
                               authorArticles$link[n],
                               '"class="text-decoration-none link-dark link-first-level">',
                               authorArticles$title[n],
                               '</a></h6><p class="alternative-text-third-level">',
                               authorArticles$authors[n],
                               '</p><p class="text-third-level">',
                               authorArticles$publication[n],
                               '<div id="divMixDataR1">
                                            <p id="pMixDataR1" class="text-second-level"><span id="plusDataR1"><img id="plusDataPicR1" src="assets/img/allicon.png" style="width: 20px;height: 20px;">&nbsp;<span id="plusDataDigitR1">##</span></span>&nbsp;|&nbsp;<span id="gsDataR1"><img id="gsDataPicR1" src="assets/img/googleicon.png" style="width: 20px;height: 20px;">&nbsp;<span id="gsDataDigitR1">##</span></span>&nbsp;|&nbsp;<span id="wosDataR1"><img id="wosDataPicR1" src="assets/img/wosicon.png" style="width: 20px;height: 20px;">&nbsp;<span id="wosDataDigitR1">##</span></span>&nbsp;|&nbsp;<span id="scDataR1"><img id="scDataPicR1" src="assets/img/scopusicon.png" style="width: 20px;height: 20px;">&nbsp;<span id="scDataDigitR1">##</span></span></p>
                                        </div>
                                    </td><td class="text-end pe-2"><a href="',
                               authorArticles$cited_by$link[n],
                               '"class="link-dark alternative-link-second-level">',
                               authorArticles$cited_by$value[n],
                               '</a></td><td class="text-end ps-2">',
                               authorArticles$year[n],
                               '</td></tr>')
            documents <- paste(documents, document)
          }

          output$docTableBody <- renderUI({
            HTML(documents)
          })
            
      # Columna Impacto Personal
        # Table de Metricas
          # Citas Totales
            metTabCitTot <- metTabCitTot
            output$metTabCitTot <- renderText({
              metTabCitTot
            })
            
          # Citas Ultimos 5 años
            metTabCit5y <- metTabCit5y
            output$metTabCit5y <- renderText({
              metTabCit5y
            })
            
          # H index total
            metTabHiTot <- metTabHiTot
            output$metTabHiTot <- renderText({
              metTabHiTot
            })
            
          # H index ultimos 5 años
            metTabHi5y <- metTabHi5y
            output$metTabHi5y <- renderText({
              metTabHi5y
            })
            
          # H index i10 total
            metTabHi10Tot <- metTabHi10Tot
            output$metTabHi10Tot <- renderText({
              metTabHi10Tot
            })
            
          # H index i10 ultimos 5 años
            metTabHi105y <- metTabHi105y
            output$metTabHi105y <- renderText({
              metTabHi105y
            })
            
        # Grafica de Citas
          output$plot1 <- renderPlotly({p})
          output$plot2 <- renderUI(HTML(paste(htmltools::tagList(list(p)))))
          outputOptions(output, "plot1", suspendWhenHidden = FALSE)
          outputOptions(output, "plot2", suspendWhenHidden = FALSE)

          
        # Autores Citantes
          citingDataExample <- top5citingAuthors
          citingAuthTable <- data.table(autoresCitantes = citingDataExample)

          # Output
          output$citingAuthTable <- renderTable({citingAuthTable})
            
          
}

}