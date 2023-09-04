library(data.table)
library(openxlsx)
library(magrittr)
library(stringr)
library(urltools)
library(rvest)

source('./functions.R')

gsp <- process_gsp("scholarplus_selfcit/data/j7syrnkAAAAJ.2022-02-04/profile.j7syrnkAAAAJ.html")

# citing_urls <- generate_gs_citing_urls(gsp[['id_cited']])
# write(citing_urls, 'scholarplus_selfcit/data/kyTHOh0AAAAJ.2021-09-14/gs_citing_urls.txt')
# citing_urls_path <- 'scholarplus_selfcit/data/kyTHOh0AAAAJ.2021-09-14/gs_citing_urls.txt' %>%
#   dirname()
#source('gs_download_citations.R')

start_time <- Sys.time()
citing <- lapply(list.files('scholarplus_selfcit/data/j7syrnkAAAAJ.2022-02-04/citing_html/', full.names = T), process_gs_citations) %>%
  rbindlist(fill = T) %>%
  .[!is.na(id_did_citing)] %>%
  .[, aut_pub_year := str_remove(aut_pub_year, '<div class=\"gs_a\">\n')] %>%
  .[, aut_pub_year := str_remove(aut_pub_year, '<div class=\"gs_a\">')] %>%
  .[, aut_pub_year := str_remove(aut_pub_year, '\n</div>')] %>%
  .[, aut_pub_year := str_remove(aut_pub_year, '</div>')] %>%
  unique(by = c('id_gs_cited', 'id_did_citing'))
end_time <- Sys.time()
end_time - start_time
citing <- cbind(citing, lapply(citing$aut_pub_year, parse_pub_info) %>% rbindlist())

citing_authors <- citing[, .(id_did_citing, aut_pub_year)] %>%
  .[, authors := str_split(aut_pub_year, "\\s-")[[1]][1], by = id_did_citing] %>%
  .[, authors := str_replace_all(authors, '，', ', ')] %>% 
  .[, lapply(.SD, function(x) unlist(tstrsplit(x, ","))),
    .SDcols = "authors",by = id_did_citing] %>%
  .[, c('id_author', 'name_author') := ''] %>% 
  .[, c('id_author', 'name_author') := process_author(authors), by = list(id_did_citing, authors)] %>%
  .[, authors := str_trim(authors)] %>%
  unique(by = c('id_did_citing', 'id_author', 'name_author')) %>%
  .[, name_author := str_remove(name_author, '…')] %>%
  .[, authors := NULL] %>%
  unique(by = c('id_did_citing', 'id_author', 'name_author')) %>%
  .[, name_author := str_trim(name_author)] %>%
  .[regexpr('，', name_author)<0]

citing_authors_dup <- merge(citing_authors, citing[, .(id_did_citing)], by = 'id_did_citing', allow.cartesian = T)

freq_cit_authors <- citing_authors_dup[, .N, name_author][order(-N)]

# Find name variants of authors with gsc id, and look for variants of the same name without id
author_variants <- citing_authors[!is.na(id_author), .N, list(id_author, name_author)] %>%
  .[order(id_author, name_author, -N)]

candidate_orphan_authors <- citing_authors[is.na(id_author), .N, name_author] %>% 
  .[order(-N)] %>% 
  merge(., author_variants[, .(id_author, name_author)] %>% unique(), by = 'name_author', all.x = T)


