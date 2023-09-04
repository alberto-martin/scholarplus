library(shiny)
library(shinyjs)
library(data.table)
library(urltools)
library(magrittr)
library(ggplot2)
library(plotly)
library(stringr)
#library(readr)
source('../functions.R')

# check whether data directory exists, create if not
if (!dir.exists('data')) {
    dir.create('data')
}

valid_id_gsp <- function(x) {
    if (x == '') {
        clean_id_gsp <- NA
    } else if (grepl('^[a-zA-Z0-9_-]{12}$', x)) {
        clean_id_gsp <- x
    } else if (regexpr('user=', x, fixed = T) > 0) {
        clean_id_gsp <- x %>% 
            param_get(parameter_names = 'user') %>% 
            .[1,1]
    } else {
        clean_id_gsp <- NA
    }
    clean_id_gsp
}

get_available_profiles <- function(path = 'data/') {
    list.files(path, recursive = T, full.names = T) %>% 
        .[grepl('profile\\.[a-zA-Z0-9_-]{12}\\.html', .)] %>% 
        lapply(., get_profile_info) %>% 
        rbindlist() %>%
        setorder(-extraction_date, id_gsp)
}

detect_citation_files <- function(path) {
  list.files(path, full.names = T) %>% 
    file.info() %>% 
    data.table(filepath = rownames(.), mtime = .$mtime) %>% 
    .[order(-mtime)] %>% 
    .[, .(filepath, mtime)]
}

download_citations <- function(documents, path) {
  citing_urls <- generate_gs_citing_urls(documents)
  print('ha generado citing_urls')
  write(path, 'gs_citing_urls.txt')
  print('ha escrito la ruta')
  write(citing_urls, 'gs_citing_urls.txt', append = T)
  print('ha escrito las URLs')
  source('../gs_download_citations.R')
}

process_citations <- function(path) {
  
  start_time <- Sys.time()
  citing_docs <- lapply(list.files(path, full.names = T), process_gs_citations) %>%
    rbindlist(fill = T) %>%
    .[!is.na(id_did_citing)] %>%
    .[, aut_pub_year := str_remove(aut_pub_year, '<div class=\"gs_a\">\n')] %>%
    .[, aut_pub_year := str_remove(aut_pub_year, '<div class=\"gs_a\">')] %>%
    .[, aut_pub_year := str_remove(aut_pub_year, '\n</div>')] %>%
    .[, aut_pub_year := str_remove(aut_pub_year, '</div>')] %>%
    unique(by = c('id_gs_cited', 'id_did_citing'))
  end_time <- Sys.time()
  print(end_time - start_time)
  
  citing_authors <- citing_docs[, .(id_did_citing, aut_pub_year)] %>%
    .[, authors := str_split(aut_pub_year, "\\s-")[[1]][1], by = id_did_citing] %>%
    .[, lapply(.SD, function(x) unlist(tstrsplit(x, ","))),
      .SDcols = "authors",by = id_did_citing] %>% 
    .[, c('id_author', 'name_author') := process_author(authors), by = list(id_did_citing, authors)] %>%
    unique(by = c('id_did_citing', 'id_author', 'name_author')) %>%
    .[, authors := str_trim(authors)] %>%
    .[, name_author := str_remove(name_author, '…')] %>%
    .[, authors := NULL] %>% 
    rbind(.[regexpr('，', name_author)<0],
          .[regexpr('，', name_author)>0, .(id_did_citing, name_author)] %>%
            .[, lapply(.SD, function(x) unlist(tstrsplit(x, '，'))),
              .SDcols = "name_author",by = id_did_citing],
          fill = T) %>%
    unique(by = c('id_did_citing', 'id_author', 'name_author')) %>%
    .[, name_author := str_trim(name_author)] %>%
    .[regexpr('，', name_author)<0]
  
  rds_dir <- paste0(path, '/../rds')
  if (!dir.exists(rds_dir)) {
    dir.create(rds_dir)
  }
  
  #View(citing_docs)
  #View(citing_authors)
  
  saveRDS(citing_docs, paste0(rds_dir, '/citing_docs.rds'))
  saveRDS(citing_authors, paste0(rds_dir, '/citing_authors.rds'))
}

shinyInput <- function(FUN, len, id, ...) {
    inputs <- character(len)
    for (i in seq_len(len)) {
        inputs[i] <- as.character(FUN(paste0(id, i), ...))
    }
    inputs
}

jscode <- "shinyjs.toTop = function() {window.scrollTo(0, 0);}"

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  useShinyjs(),
  extendShinyjs(text = jscode, functions = c("toTop")),
  
  tags$head(HTML('<link href="plotly/plotly-htmlwidgets.css" rel="stylesheet" />
<script src="plotly/plotly-latest.min.js"></script>
<script src="plotly/plotly.js"></script>')),

    tags$style(HTML("
      .navbar-default .navbar-brand { color: #000000; 
                                      font-size: 30px;}
                                      
                                      
      .local_profiles th, .local_profiles tr td{
        height: 90px;
        padding: 5px;
      }
      
      .local_profiles th{
        height: 50px;
        background-color: #bebebe;
      }
      
      .profile_img_small {
        width:70px;
        height:70px;
        border-radius: 50%;
        overflow:hidden;
        padding:0px;
        background-color:#ececec;
      }

      .profile_img_small img {
        height:70px;
        display:block;
        margin-left:auto;
        margin-right:auto;
      }   
      
      .local_profiles tr {
        border: 1px solid #939393;
        border-radius: 50%;
        background-color: #ececec;
      }
      
      .local_profiles tr:hover {
        background-color: #fdffad;
      }
    
      .local_profiles {
        margin:20px 0px 20px 0px;
      }
      
      .author_name {
        font-size:22px;
      }
      
      .author_affiliation {
        font-size:16px;
      }
      
      .profile_img_big {
        width:128px;
        height:128px;
        border-radius: 50%;
        overflow:hidden;
        padding:0px;
        background-color:#ececec;
      }
      
      .profile_img_big img {
        display:block;
        margin-left:auto;
        margin-right:auto;
      }
      
      .document_list thead tr {
        vertical-align:middle;
        background-color:#ececec;
        font-size:16px;
        line-height:30px;
      }
      
      .document_list thead tr th {
        padding:5px 3px 5px 5px;
        text-align:left;
      }
      
      .document_list tbody tr {
        text-align:left;
      }
      
      .doc_info {
        vertical-align:top;
        width:80%;
        padding:8px 10px 8px 5px;
        font-size:14px;
        color: #707070;
      }
      
      .doc_info a {
        font-size:15px;
      }
      
      .cit_counts, .pub_year {
        font-size:15px;
        width:15%;
        vertical-align:top;
        text-align:center;
        padding: 10px 10px 10px 0px;
      }
      
      div {
        #border:solid 1px black;
      }
      
      .pers_doc {
        padding: 15px;
        border-right: solid 1px #ececec;
      }
      
      .cited_by {
        padding-left:5px;
      }
      

  ")),
    navbarPage(id = 'workflow',
               title = 'scholar+',
               tabPanel('1. Select',
                        div(class = 'jumbotron',
                            div(class = 'container',
                                div(class = 'row',
                                    div(class = 'col-xs-12',
                                        textInput("id_gsp",
                                                  "Type a Google Scholar profile URL",
                                                  placeholder = "https://scholar.google.com/citations?user=YlPd48UAAAAJ",
                                                  width = '400px'))),
                                div(class = 'row',
                                    div(class = 'col-xs-12',
                                        conditionalPanel(condition = 'input.id_gsp.length > 0 & output.n_rows > 0',
                                                         actionButton('get_profile_again', 'Download profile again', icon = icon('download'))
                                        )
                                    ),
                                    div(class = 'col-xs-12',
                                        conditionalPanel(condition = 'output.n_rows == 0',
                                                         actionButton('get_profile_now', 'Download profile now', icon = icon('download'))
                                        ))
                                ))
                            ),
                        div(class = 'container',
                            div(class = 'row',
                                div(class = 'col-xs-12',
                                    conditionalPanel(condition = 'output.n_rows > 0',
                                                     htmlOutput('downloaded_profiles'))
                                    )
                                    )
                            )
                        ),
               tabPanel('2. Profile',
                        div(class = 'container',
                            # div(class = 'row',
                            #     div(class = 'col-xs-12',
                            #         ))
                            div(class = "row",
                                div(class = "col-12 col-lg-9",
                                    div(class = 'row pers_doc',
                                        div(class = 'col-12 personal_info',
                                            htmlOutput('gsp_personal_info', class = 'row', style = 'margin:0px 0px 30px 0px;')),
                                        div(class = 'col-12',
                                            htmlOutput('gspd')))),
                                div(class = 'col-12 col-lg-3',
                                    div(class = 'row cited_by',
                                        div(class = 'col-12',
                                            h4('Cited by'),
                                            htmlOutput('author_indicators')),
                                        div(class = 'col-12',
                                            plotlyOutput('citations_bar', height = '200px')),
                                        div(class = 'col-12',
                                            h4('Citation data'),
                                            htmlOutput('citation_html_status'),
                                            actionButton('download_citations', 'Download citations now', icon = icon('download')),
                                            conditionalPanel(condition = 'output.citation_html_exists > 0',
                                                             style = 'margin: 10px 0px 0px 0px;',
                                                             htmlOutput('citation_rds_status'),
                                                             actionButton('process_citations', 'Process citations'))
                                            )))
                                )
                            )
                        ),
               tabPanel('3. Citations'),
               tabPanel('4. Dashboards')
    )
)


server <- function(input, output, session) {
    
    ## PAGE 1. SELECT
  
    # ID/URL of Google Scholar Citations profile typed in search box
    id_gsp <- reactiveValues(selected_id = '')
    
    # Check downloaded profiles
    rv <- reactiveValues(downloaded_folders = get_available_profiles())
    
    observeEvent(input$id_gsp, {
        selected_id_gsp <- valid_id_gsp(input$id_gsp)
        if (!is.na(selected_id_gsp)) {
            rv$downloaded_folders <- rv$downloaded_folders[id_gsp == selected_id_gsp]
        } else {
            rv$downloaded_folders <- get_available_profiles()
        }
        rv$downloaded_folders <- rv$downloaded_folders %>% 
            .[, action := shinyInput(actionButton, .N, 'button_', label = '', icon = icon('glyphicon-ok-circle', lib = 'glyphicon'), onclick = 'Shiny.onInputChange(\"select_button\",  this.id)' )]
    })
    
    # Display list of downloaded profiles
    n_rows <- reactive({
        rv$downloaded_folders %>%
            dim(.) %>% 
            .[1]
        })
    
    output$n_rows <- reactive(n_rows())
    
    prof_downloaded_rows <- reactive({
      rv$downloaded_folders[, html_row := paste0('<tr><td><div class="profile_img_small"><img src="', id_gsp, '.jpeg"/></div></td>',
                                                 '<td><div>',
                                                 id_gsp, 
                                                 '</div></td>',
                                                 '<td><div>', 
                                                 author_name,
                                                 '</div><div>',
                                                 author_affiliation,
                                                 '</div></td><td><div>',
                                                 extraction_date,
                                                 '</div></td>',
                                                 '<td>', action, '</td>',
                                                 '</tr>')] %>% 
        .[, html_row] %>% 
        paste0(collapse = '') %>% 
        HTML()
    })
    
    output$downloaded_profiles <- renderUI({
      tags$table(class = 'local_profiles',
                 tags$thead(
                   tags$tr(
                     tags$th(''),
                     tags$th('PROFILE ID'),
                     tags$th('PROFILE INFO'),
                     tags$th('DATE OF DATA EXTRACTION'),
                     tags$th('SELECT ONE')
                   )
                 ),
                 tags$tbody(
                   prof_downloaded_rows()
                 )
      )
    })
    
    # Download new profile
    download_profile <- function() {
      good_id_gsp <- valid_id_gsp(input$id_gsp)
      if (!is.na(good_id_gsp)) {
        id_gsp$selected_id <<- good_id_gsp
        new_file_path <- paste0('data/', good_id_gsp, '.', as.character(Sys.Date()), '/profile.', good_id_gsp, '.html')
        if (!file.exists(new_file_path)) {
          write(good_id_gsp, 'profile_to_download.txt')
          source('../gs_download_profile.R')
          rv$downloaded_folders <- get_available_profiles()[id_gsp == good_id_gsp] %>% 
            .[, action := shinyInput(actionButton, .N, 'button_', label = '', icon = icon('glyphicon-ok-circle', lib = 'glyphicon'), onclick = 'Shiny.onInputChange(\"select_button\",  this.id);document.body.scrollTop = 0;document.documentElement.scrollTop = 0' )]
        }
      }
    }
    
    observeEvent(input$get_profile_again, download_profile())
    observeEvent(input$get_profile_now, download_profile())
    
    
    # Select and go to profile
    observeEvent(input$select_button, {
        selectedRow <- as.numeric(strsplit(input$select_button, "_")[[1]][2])
        lapply(seq_along(rv$downloaded_folders$id_gsp), function(x) updateActionButton(session, paste0('button_', x), icon('glyphicon-ok-circle', lib = 'glyphicon')))
        updateActionButton(session, paste0('button_', selectedRow), icon = icon("check-circle"))
        id_gsp$selected_id <<- paste0(rv$downloaded_folders[selectedRow][, .(id_gsp, extraction_date)], collapse = '.')
    })
    
    observeEvent(input$select_button, {
        updateTabsetPanel(session, 'workflow', selected = '2. Profile')
        js$toTop()
    })
    
    
    ## PAGE 2. PROFILE
    # Processing data from profile
    file_path <- reactive({
        paste0('data/', id_gsp$selected_id, '/profile.', substring(id_gsp$selected_id, 1, 12), '.html')
    })
    
    gsp <- reactive({
        gsp <- process_gsp(file_path())
        #View(gsp[['documents']])
        #View(gsp[['id_cited']])
        gsp
    })
    
    # Formatting personal info in profile (name, affiliation, image)
    gsp_personal_info <- reactive({
      gsp()[['personal_info']][, html_row := paste0('<div class="col-xs-2" style="padding:0px;"><div class="profile_img_big"><img src="', id_gsp, '.jpeg"/></div></div>',
                                                    '<div class="col-xs-10"><div class="row author_name">', author_name, '</div>',
                                                    '<div class="row author_affiliation">', author_affiliation, '</div></div>'
                                                    )] %>% 
        .[, html_row]
    })
    
    output$gsp_personal_info <- renderUI({
      HTML(gsp_personal_info())
    })
    
    # Formatting list of documents in profile in profile (and bib info)
    rows_gspd <- reactive({
        gsp()[['documents']][, html_row := paste0('<tr><td class="doc_info"><a href="">', title, '</a><div>', authors, '</div><div>', publication, '</div></td>',
                                                  '<td class="cit_counts"><a href="https://scholar.google.com/scholar?oi=bibs&hl=en&authuser=1&cites=', id_gsd_list, '">', ncit_gs, '</a></td>',
                                                  '<td class="pub_year"><span>', pub_year, '</span></td></tr>')] %>% 
            .[, html_row] %>% 
            paste0(collapse = '') %>% 
            HTML()
    })
    
    output$gspd <- renderUI({
        tags$table(class = 'document_list',
            tags$thead(
                tags$tr(
                    tags$th('TITLE'),
                    tags$th('CITED BY'),
                    tags$th('YEAR')
                )
            ),
            tags$tbody(
                rows_gspd()
            )
        )
    })
    
    # Formatting author-level indicators in profile
    author_indicators <- reactive({
      gsp()[['indicators']] %>% 
        .[, .(ncit, ncit_last5, h, h_last5, i10, i10_last5)] %>% 
        transpose(keep.names = 'indicator') %>%
        .[, last5 := rep.int(c(F,T),3)] %>%
        list(.[last5 == F, V1],.[last5 == T, V1]) %>%
        data.table(indicator = c('Citations', 'h-index', 'i10-index'),
                   total = .[[2]],
                   last5 = .[[3]]) %>%
        .[, .(indicator, total, last5)]
    })
    
    output$author_indicators <- renderUI({
      tb <- author_indicators()
      
      tags$table(class = 'author_indicators table',
                 tags$thead(
                   tags$tr(
                     tags$th(''),
                     tags$th('All'),
                     tags$th(paste0('Since ', as.character(max(gsp()[['histogram']][, year])-5)))
                   )
                 ),
                 tags$tbody(
                   tags$tr(
                     tags$td('Citations'),
                     tags$td(as.character(tb[1,2])),
                     tags$td(as.character(tb[1,3])),
                   ),
                   tags$tr(
                     tags$td('h-index'),
                     tags$td(as.character(tb[2,2])),
                     tags$td(as.character(tb[2,3])),
                   ),
                   tags$tr(
                     tags$td('i10-index'),
                     tags$td(as.character(tb[3,2])),
                     tags$td(as.character(tb[3,3])),
                   )
                 )
            )
    })
    
    # Generating citation bar plot
    output$citations_bar <- renderPlotly({
      dt <- gsp()[['histogram']][, .(year, ncit_gs)] %>%
        .[, year := as.character(year)] %>% 
        setorder(-year) %>% 
        .[1:8] %>% 
        setorder(year) %>% 
        setnames(c('year', 'ncit_gs'),
                 c('Year', 'Citations'))

      gs_bar_plot <- ggplot(dt, aes(x = Year, y = Citations)) +
                      geom_bar(stat='identity', width = 0.45, fill = '#777777') +
                      theme(axis.title = element_blank(),
                            axis.text=element_text(size=8),
                            axis.ticks = element_blank(),
                            panel.background = element_rect(fill = "white"),
                            panel.grid.major.y = element_line(size = 0.5, linetype = 'solid', colour = '#ececec'))
      ggplotly(gs_bar_plot, height = 200, width = 301) %>% 
        config(displayModeBar=FALSE) %>% 
        layout(yaxis = list(side = 'right'),
               margin = list(
                 l = 0,
                 r = 0,
                 b = 0,
                 t = 0,
                 pad = 0
               ))
    }
    )
    
    # Detect citation data
    citation_files <- reactive({
      detect_citation_files(path = paste0('data/', id_gsp$selected_id, '/citing_html/'))
    })
    
    citation_html_exists <- reactive({
      dim(citation_files())[1]
    })
    
    output$citation_html_exists <- reactive({
      dim(citation_files())[1]
      })
    
    citation_html_status <- reactive({
      if(citation_html_exists() == 0) {
        message <- 'Citation data not available.'
      } else {
        message <- paste0('Last downloaded on ',
                          citation_files()[1, mtime],
                          ' (',
                          dim(citation_files())[1],
                          ' HTML files).')
        message
      }
      })
    
    citation_rds_exists <- reactive({
      file.exists(paste0('data/', id_gsp$selected_id, '/rds/citing_docs.rds')) %>% 
        as.integer()
    })
    
    citation_rds_status <- reactive({
      if(citation_rds_exists() == 1L) {
        last_processed <- file.info(paste0('data/', id_gsp$selected_id, '/rds/citing_docs.rds'))$mtime[1]
        message <- paste0('Citation data last processed on ', last_processed, '.')
      } else {
        message <- 'Citation data has not yet been processed.'
      }
    })
      
    output$citation_html_status <- renderUI({
      HTML(paste0('<p>',citation_html_status(),'</p>'))
    })
    
    output$citation_rds_status <- renderUI({
      HTML(paste0('<p>',citation_rds_status(),'</p>'))
    })
    
    # Download citation data
    observeEvent(input$download_citations, download_citations(documents = gsp()[['id_cited']],
                                                              path = paste0('data/', id_gsp$selected_id)))
    
    # Process citation data
    observeEvent(input$process_citations, process_citations(path = paste0('data/', id_gsp$selected_id, '/citing_html')))
    
    
    # output values are only calculated if displayed. The following is necessary
    # in conditional panels
    outputOptions(output, "n_rows", suspendWhenHidden = FALSE)
    outputOptions(output, "citation_html_exists", suspendWhenHidden = FALSE)
}

# Run the application 
shinyApp(ui = ui, server = server)
