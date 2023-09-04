library(data.table)
library(rvest)
library(RSelenium)
library(magrittr)
library(urltools)
library(stringr)

#id_profile <- 'https://scholar.google.com/citations?user=kyTHOh0AAAAJ'

#get_html_gs_profile('https://scholar.google.com/citations?user=kyTHOh0AAAAJ')

# Get profile info
get_profile_info <- function(path) {
  prof_html <- read_html(path)
  id_gsp <- html_node(prof_html, xpath = '//form[@id="gsc_fol_f"]') %>% 
    html_attr('action') %>% 
    paste0('https://scholar.google.com', .) %>%
    param_get(parameter_names = 'user') %>% 
    .[1,1]
  extraction_date <- substring(path, regexpr('\\d{4}-\\d{2}-\\d{2}', path), regexpr('\\d{4}-\\d{2}-\\d{2}', path) + 9)
  name <- html_node(prof_html, xpath = '//div[@id="gsc_prf_in"]') %>% 
    html_text()
  affiliation <- html_node(prof_html, xpath = '//div[@id="gsc_prf_i"]/div[@class="gsc_prf_il"]') %>% 
    html_text()
  data.table(id_gsp = id_gsp,
             extraction_date = extraction_date,
             author_name = name,
             author_affiliation = affiliation)
}

# Get raw html of author profile (all documents)
get_id_gsp <- function(x) {
  param_get(urls = x, parameter_names = 'user') %>% 
    .[1,1]
}

process_gspd <- function(el_gspd, id_gsp) {
  # title
  title <- html_nodes(el_gspd, xpath = 'td[@class="gsc_a_t"]/a') %>% 
    html_text()

  # title url
  title_url <- html_nodes(el_gspd, xpath = 'td[@class="gsc_a_t"]/a') %>% 
    html_attr('href') %>% 
    paste0('https://scholar.google.com', .) %>% 
    param_get(parameter_names = 'citation_for_view') %>% 
    .[1,1]
  
  # authors
  authors <- html_nodes(el_gspd, xpath = 'td[@class="gsc_a_t"]/div[@class="gs_gray"][1]') %>% 
    html_text()
  
  # publication
  publication <- html_nodes(el_gspd, xpath = 'td[@class="gsc_a_t"]/div[@class="gs_gray"][2]') %>% 
    html_text()

  # citation count
  ncit <- html_nodes(el_gspd, xpath = 'td[@class="gsc_a_c"]/a') %>% 
    html_text() %>% 
    as.integer()
  
  # citation count url
  ncit_url <- html_nodes(el_gspd, xpath = 'td[@class="gsc_a_c"]/a') %>% 
    html_attr('href') %>% 
    paste0('https://scholar.google.com', .) %>% 
    param_get(parameter_names = 'cites') %>% 
    .[1,1]
    
  # publication year
  pub_year <- html_nodes(el_gspd, xpath = 'td[@class="gsc_a_y"]/span') %>% 
    html_text()

  # Insertamos la Fila en el Data.Table
  dt_row <- data.table(id_gsp = id_gsp,
                       id_gspd = title_url,
                       title = title,
                       authors = authors,
                       publication = publication,
                       pub_year = pub_year,
                       ncit_gs = ncit,
                       id_gsd_list = ncit_url)
  return(dt_row)
}

process_gspc <- function(el_gspc, id_gsp) {
  image_url <- html_nodes(el_gspc, xpath = 'div/span[1]/img') %>% 
    html_attr('src')
  coauthor_name <- html_nodes(el_gspc, xpath = 'div/span[2]/a') %>% 
    html_text()
  coauthor_id <- html_nodes(el_gspc, xpath = 'div/span[2]/a') %>% 
    html_attr('href') %>% 
    paste0('https://scholar.google.com', .) %>% 
    get_id_gsp()
  coauthor_affiliation <- html_nodes(el_gspc, xpath = 'div/span[2]/span[@class="gsc_rsb_a_ext"]') %>% 
    html_text()
  coauthor_mail <- html_nodes(el_gspc, xpath = 'div/span[2]/span[contains(@class, "gsc_rsb_a_ext2")]') %>% 
    html_text() %>% 
    gsub('Verified email at ', '', ., fixed = T)
  data.table(id_gsp,
             id_coauthor = coauthor_id,
             coauthor_name = coauthor_name,
             coauthor_affiliation = coauthor_affiliation,
             coauthor_mail = coauthor_mail,
             image_url = image_url)
}

process_gsp <- function(html_gsp_path) {
  
  path <- dirname(html_gsp_path)
  
  id_gsp <- str_remove(html_gsp_path, path) %>% 
    substring(10, 21)

  id_gsp_date <- str_remove(html_gsp_path, path) %>% 
    substring(23, 32)

  
  # id_gsp_positions <- regexpr('\\/[a-zA-Z0-9_-]{12}\\.\\d{4}-\\d{2}-\\d{2}\\.html', html_gsp_path)
  # id_gsp <- str_sub(html_gsp_path,
  #                   start = id_gsp_positions + 1,
  #                   end = id_gsp_positions + attr(id_gsp_positions, 'match.length') + 1)
  #print(id_gsp)
  
  # Process personal information
  gsppi <- get_profile_info(html_gsp_path)
  
  # Process list of documents
  l_el_gspd <- read_html(html_gsp_path, encoding = "UTF-8") %>% 
    html_nodes(xpath = '/html/body/div[@id="gs_top"]/div[@id="gs_bdy"]/div[@id="gs_bdy_ccl"]/div[@id="gsc_bdy"]/div[@id="gsc_art"]/form/div[@id="gsc_a_tw"]/table/tbody/tr')
  
  gspd <- lapply(l_el_gspd, process_gspd, id_gsp) %>% 
    rbindlist()
  
  # Process histogram (citations received per year)
  gsph_years <- read_html(html_gsp_path, encoding = "UTF-8") %>% 
    html_nodes(xpath = '//div[@class="gsc_md_hist_b"]/span') %>% 
    html_text()
  gsph_ncit <- read_html(html_gsp_path, encoding = "UTF-8") %>% 
    html_nodes(xpath = '//div[@class="gsc_md_hist_b"]/a/span') %>% 
    html_text()
  gsph <- data.table(id_gsp = id_gsp,
                     year = gsph_years,
                     ncit_gs = gsph_ncit) %>% 
    .[, ncit_gs := as.numeric(ncit_gs)] %>% 
    .[, year := as.numeric(year)]
  
  # Process author-level indicators
  el_gspi <- read_html(html_gsp_path, encoding = "UTF-8") %>% 
    html_nodes(xpath = '//table[@id="gsc_rsb_st"]/tbody/tr')
  gspi <- data.table(id_gsp = id_gsp,
                     ncit = el_gspi[[1]] %>% html_nodes(xpath = 'td[2]') %>% html_text(),
                     ncit_last5 = el_gspi[[1]] %>% html_nodes(xpath = 'td[3]') %>% html_text(),
                     h = el_gspi[[2]] %>% html_nodes(xpath = 'td[2]') %>% html_text(),
                     h_last5 = el_gspi[[2]] %>% html_nodes(xpath = 'td[3]') %>% html_text(),
                     i10 = el_gspi[[3]] %>% html_nodes(xpath = 'td[2]') %>% html_text(),
                     i10_last5 = el_gspi[[3]] %>% html_nodes(xpath = 'td[3]') %>% html_text())
  
  # Process co-authors
  l_el_gspc <- read_html(html_gsp_path, encoding = "UTF-8") %>% 
    html_nodes(xpath = '//ul[@class="gsc_rsb_a"]/li')
  gspc <- lapply(l_el_gspc, process_gspc, id_gsp) %>% 
    rbindlist()
  
  # Process cited doc ids
  gspdc <- gspd %>% 
    .[, .(id_gspd, id_gsd_list)] %>% 
    .[, lapply(.SD, function(x) unlist(tstrsplit(x, ","))),
       .SDcols = "id_gsd_list",by = id_gspd] %>% 
    .[!is.na(id_gsd_list)] %>% 
    .[, c('id_gsd', 'id_gsd_list') := list(id_gsd_list, NULL)]
  
  # Pack data into list
  result <- list(personal_info = gsppi,
                 documents = gspd,
                 id_cited = gspdc,
                 indicators = gspi,
                 histogram = gsph,
                 coauthors = gspc)
  
  # Saving data
  # id_gsp_date <- str_sub(html_gsp_path,
  #                        start = id_gsp_positions + 6,
  #                        end = id_gsp_positions + attr(id_gsp_positions, 'match.length') - 1)
  # check whether rds directory exists, create if not
  # profile_dir <- 'data/rds'
  # if (!dir.exists(profile_dir)) {
  #   dir.create(profile_dir)
  # }
  # write_path <- paste(profile_dir, '/', id_gsp_date, '.rds', sep="")
  # print(write_path)
  # saveRDS(result, write_path)
  # print(paste('Data saved to', write_path))
  
  result
}

generate_gs_citing_urls <- function(dt) {
  id_gsd_list <- dt$id_gsd %>% 
    unique() %>% 
    paste0('https://scholar.google.com/scholar?cites=', ., '&as_sdt=2005&sciodt=0,5&hl=en')
  
  id_gsd_list
}

process_gs_documents <- function(el_doc) {
  
  # did
  id_did <- el_doc %>% 
    html_attr('data-did')

  # id_gs (cluster)
  id_gs <- html_nodes(el_doc, xpath = 'div[@class="gs_ri"]/h3[@class="gs_rt"]/a') %>% 
    html_attr('data-clk') %>% 
    paste0('http://scholar.google.com?', .) %>% 
    param_get(parameter_names = 'd') %>%
    .[1,1]
  
  # Record is type-citation
  type_citation <- F

  # Title
  title <- html_nodes(el_doc, xpath = 'div[@class="gs_ri"]/h3[@class="gs_rt"]/a') %>% 
    html_text()
  if (identical(character(0), title)) {
    title <- html_nodes(el_doc, xpath = 'div[@class="gs_ri"]/h3[@class="gs_rt"]/span[2]') %>% 
      html_text()
    type_citation <- T
  }
  
  # Title URL
  title_url <- html_nodes(el_doc, xpath = 'div[@class="gs_ri"]/h3[@class="gs_rt"]/a') %>% 
    html_attr('href')
  
  # aut_pub_year
  aut_pub_year <- html_nodes(el_doc, xpath = 'div[@class="gs_ri"]/div[@class="gs_a"]') %>% 
    as.character()
  
  # ncit count
  ncit_gs <- html_nodes(el_doc, xpath = 'div[@class="gs_ri"]/div[@class="gs_fl"]/a[contains(@href,"/scholar?cites")]') %>%
    html_text() %>% 
    str_remove('Cited by ') %>% 
    as.integer()
  
  # version count
  nver <- html_nodes(el_doc, xpath = 'div[@class="gs_ri"]/div[@class="gs_fl"]/a[contains(@href,"/scholar?cluster")]') %>%
    html_text() %>% 
    str_remove('All ') %>% 
    str_remove(' versions') %>% 
    as.integer()
  
  # ncit_wos
  ncit_wos <- html_nodes(el_doc, xpath = 'div[@class="gs_ri"]/div[@class="gs_fl"]/a[contains(@href,"gateway.webofknowledge")]') %>%
    html_text() %>%
    ifelse(test = identical(character(0), .),
           yes = NA,
           no = .) %>% 
    str_remove('Web of Science: ') %>% 
    as.integer()
  
  # id_wos
  id_wos <- html_nodes(el_doc, xpath = 'div[@class="gs_ri"]/div[@class="gs_fl"]/a[contains(@href,"gateway.webofknowledge")]') %>%
    html_attr('href') %>%
    ifelse(test = identical(character(0), .),
            yes = 'https://dummy.com?UT=NA',
            no = .) %>% 
    param_get(parameter_names = 'UT') %>%
    .[1,1] %>% 
    ifelse(. == 'NA', NA, .)
  
  # full text url
  ft_url <- html_nodes(el_doc, xpath = 'div[@class="gs_ggs gs_fl"]/div/div/a[1]') %>%
    html_attr('href')
  
  data.table(id_did_citing = id_did,
             id_gs_citing = id_gs,
             id_wos_citing = id_wos,
             title_url = title_url,
             type_citation = type_citation,
             title = title,
             aut_pub_year = aut_pub_year,
             ncit_gs = ncit_gs,
             ncit_wos = ncit_wos,
             nver = nver,
             ft_url = ft_url)
}

process_gs_citations <- function(file_path) {
  id_gs <- file_path %>% 
    substr(start = regexpr('\\/\\d+\\.', .) + 1,
           stop = regexpr('\\/\\d+\\.', .) + attr(regexpr('\\/\\d+\\.', .), 'match.length') - 2)
  
  l_el_docs <- read_html(file_path, encoding = "UTF-8", options = "HUGE") %>% 
    html_nodes(xpath = '//div[@id="gs_res_ccl_mid"]/div')
  
  l_docs <- lapply(l_el_docs, process_gs_documents) %>% 
    rbindlist() %>% 
    .[, id_gs_cited := id_gs]
}

parse_pub_info <- function(x) {
  dt <- data.table(raw_pub_info = x, 
                   type = 0,
                   start_details = 0,
                   publication = '',
                   year = '',
                   publisher = '',
                   s = list(),
                   sl = 0)
  
  for (x in 1) {
    dt[, start_details := regexpr('\\- .+[\\,] [0-9]{4} \\- .+$', raw_pub_info)]
    if (dt[1, start_details] > 0) { # Authors - publication, year - publisher
      dt[, s := transpose(strsplit(gsub('\\, ([0-9]{4})', ' - \\1', substring(raw_pub_info, start_details+2)), split = ' - ')) %>% list(.)] %>% 
        .[, sl := length(s[[1]])] %>% 
        .[, publisher := s[[1]][[sl]]] %>% 
        .[, year := s[[1]][[sl - 1]]] %>% 
        .[, publication := paste(s[[1]][1:(sl-2)], collapse = ', ')]
      next
    }
    dt[, start_details := regexpr('\\- .+[\\,] [0-9]{4}$', raw_pub_info)] # Authors - publication - year
    if (dt[1, start_details] > 0) { 
      dt[, c('publication', 'year') := transpose(strsplit(substring(raw_pub_info, start_details+2), split = ', '))]
      next
    } 
    dt[, start_details := regexpr('\\- [0-9]{4} \\- .+$', raw_pub_info)] # Authors - year - publisher
    if (dt[1, start_details] > 0) { 
      dt[, c('year', 'publisher') := transpose(strsplit(substring(raw_pub_info, start_details+2), split = ' - '))]
      next
    } 
    dt[, start_details := regexpr('\\- [0-9]{4}$', raw_pub_info)] # Authors - year
    if (dt[1, start_details] > 0) { 
      dt[, year := transpose(strsplit(substring(raw_pub_info, start_details+2), split = ' - '))]
      next
    } 
    dt[, start_details := regexpr('\\- .+ \\- .+$', raw_pub_info)] # Authors - publication - publisher
    if (dt[1, start_details] > 0) { 
      dt[, c('publication', 'publisher') := transpose(strsplit(substring(raw_pub_info, start_details+2), split = ' - '))]
      next
    } 
    dt[, start_details := regexpr('\\- .+$', raw_pub_info)] # Authors - publisher
    if (dt[1, start_details] > 0) { 
      dt[, publisher := substring(raw_pub_info, start_details+2)]
      next
    }
  }
  
  dt[, year := substr(year, regexpr('[0-9]{4}', year), stop = regexpr('[0-9]{4}', year) + 4)]
  dt[, year := gsub("[^0-9]", "", year)]
  
  dt[, .(publication, year, publisher)]
  
}

process_author <- function(author) {
  is_html <- tryCatch(read_html(author) %>% html_text(), error = function(e) 'no_html')
  if (is_html != 'no_html') {
    id_author <- author %>%
      read_html() %>% 
      html_nodes(xpath = '//a') %>% 
      html_attr('href') %>% 
      paste0('http://scholar.google.com', .) %>% 
      param_get(parameter_names = 'user') %>% 
      .[1,1]
    name_author <- author %>% 
      read_html() %>% 
      html_nodes(xpath = '//a') %>% 
      html_text()
  } else {
    id_author <- NA
    name_author <- author
  }
  list(id_author = id_author, name_author = name_author)
}

# Document details (at least for those withoud doc id)
# id_gsp <- gsp[['documents']] %>%
#   .[, id_gsp] %>% 
#   unique()
# doc_details_urls <- gsp[['documents']] %>%
#   .[is.na(id_gsd_list), id_gspd] %>%
#   paste0('https://scholar.google.com/citations?view_op=view_citation&hl=en&user=',
#          id_gsp,
#          '&citation_for_view=',
#          id_gsp,
#          ':',
#          .)
# write(doc_details_urls, 'scholarplus_selfcit/data/kyTHOh0AAAAJ.2021-09-14/doc_details_urls.txt')
# 
# rm(list = ls())
# citing_urls_path <- 'scholarplus_selfcit/data/kyTHOh0AAAAJ.2021-09-14/gs_citing_urls.txt' %>%
#   dirname()
# source('gs_download_doc_details.R')


extract_id_gsd_list <- function(schol_art_list) {
  id_gsd_list <- schol_art_list %>% 
    html_nodes(xpath = 'div[1]/a') %>% 
    html_attr('href') %>% 
    paste0('https://scholar.google.com') %>% 
    param_get(parameter_names = 'cluster') %>% 
    .[,1]
}

extract_id_did_cited_list <- function(schol_art_list) {
  id_gsd_list <- schol_art_list %>% 
    html_nodes(xpath = 'div/a[@class="gsc_oms_link" and contains(@href, "related")]') %>% 
    html_attr('href') %>% 
    paste0('https://scholar.google.com') %>% 
    param_get(parameter_names = 'q') %>% 
    .[,1]
}

process_doc_details <- function(file_path) {
  id_gspd <- file_path %>% 
    substr(start = regexpr('\\/[a-zA-Z0-9_-]+\\.html', .) + 1,
           stop = regexpr('\\/[a-zA-Z0-9_-]+\\.html', .) + attr(regexpr('\\/[a-zA-Z0-9_-]+\\.html', .), 'match.length') - 6)

  schol_art_list <- read_html(file_path) %>% 
    html_nodes(xpath = '//div[@class="gsc_oci_value"]/div[@class="gsc_oci_merged_snippet"]')
  
  id_gsd_list <- lapply(schol_art_list, extract_id_gsd_list) %>%
    unlist()

  id_did_cited_list <- lapply(schol_art_list, extract_id_did_cited_list) %>%
    unlist() %>% 
    substring(first = 9, last = regexpr(':scholar', .) - 1)
  
  data.table(id_gspd = id_gspd,
             id_gsd = id_gsd_list,
             id_did_cited = id_did_cited_list)
}

# doc_details <- lapply(list.files('scholarplus_selfcit/data/kyTHOh0AAAAJ.2021-09-14/doc_details', full.names = T), process_doc_details) %>%
#   rbindlist(fill = T) %>%
#   .[!is.na(id_gsd)]
# 
# gsp[['documents']][, id_gspd := substring(id_gspd, 14)]
# gsp[['id_cited']][, id_gspd := substring(id_gspd, 14)]
# 
# gsp[['id_cited']] <- rbind(gsp[['id_cited']], doc_details, fill = T) %>%
#   unique(by = c('id_gspd', 'id_gsd'))

# # No, because doc_details is an index, it can duplicate rows in gsp[['documents']]
# gsp[['documents']] <- merge(gsp[['documents']],
#                             doc_details[, .(id_gspd, id_gsd_list2 = id_gsd)],
#                             by = 'id_gspd',
#                             all.x = T) %>% 
#   .[is.na(id_gsd_list), id_gsd_list := id_gsd_list2] %>% 
#   .[, id_gsd_list2 := NULL]

# Identifying author self-citations
# citing[, author_selfcit := id_gs_citing %in% gsp[['id_cited']]$id_gsd]
# citing[author_selfcit == F, author_selfcit := id_did_citing %in% gsp[['id_cited']]$id_did_cited]
# citing[author_selfcit == F, author_selfcit := id_did_citing %in% citing_authors[id_author == 'kyTHOh0AAAAJ', id_did_citing]]
# 
# citing[, .N, author_selfcit][, .(self_cit_perc = N / sum(N) * 100)][1]
