library(data.table)
library(RSelenium)
library(magrittr)
library(urltools)
library(stringr)

print(list.files())
path <- readLines('gs_citing_urls.txt')[1]
citing_urls <- readLines('gs_citing_urls.txt') %>% 
  .[2:length(.)]
#file.copy('gs_citing_urls.txt', path)
#file.remove('gs_citing_urls.txt')

# Check if page shows captcha
checkload <- function(url, element_id) {
  loaded_correctly <- FALSE
  
  print(url)
  brClient$navigate(url)
  html <- brClient$findElement(using = 'xpath', '//html')$getElementAttribute('innerHTML')
  if (grepl("Please show you're not a robot", html)){
  } else if (grepl("your computer or network may be sending automated queries", html)) {
  } else if (grepl("Demuestra que no eres un robot", html)) {
  } else if (grepl("inusual procedente de tu red de ordenadores", html)) {
  } else if (grepl("Our systems have detected unusual traffic from your computer", html)) {
  } else {
    loaded_correctly <- TRUE
  }
  loaded_correctly

}

gc()
br <- rsDriver(browser = "firefox")
brClient <- br[["client"]]

# Check if citing doc directory exists, create if not
art_dir <- paste0(path, '/citing_html')
if (!dir.exists(art_dir)) {
  dir.create(art_dir)
}

for (c_url in citing_urls) {
  id_doc <- param_get(urls = c_url, parameter_names = 'cites') %>% 
    .[1,1]
  
  brClient$navigate(c_url)
  
  loaded_correctly <- checkload(c_url, 'gs_rt')
  if (loaded_correctly == F) {
    readline(prompt='Please, solve the CAPTCHA and press ENTER: ')
  }
  more_results <- TRUE
  while (more_results == TRUE) {
    html <- brClient$findElement(using = 'xpath', '//html')$getElementAttribute('innerHTML')
    tryCatch(
      {
        brClient$findElement(using = 'class', 'gs_rt')
      },
      error = function(e) {
        more_results <- FALSE
        next
      }
    )
    c_url <- brClient$getCurrentUrl()
    if (grepl('start=', c_url) == FALSE) {
      start_page <- '0'
    } else {
      start_page <- c_url %>%
        str_sub(str_locate(., 'start=')[,2]+1) %>%
        ifelse(is.na(str_locate(., '\\&')[1]),
               .,
               str_sub(., 1, str_locate(., '\\&')[,1]-1))
    }
    
    write_path <- paste(art_dir, '/', id_doc, '.', start_page, '.html', sep="")
    
    write(html[[1]], write_path)
    write_path
    
    next_page <- try(
      brClient$findElement(using = 'xpath', '//span[contains(@class, "gs_ico_nav_next")]/..')$getElementAttribute('href')[[1]]
    )
    
    if (grepl('Error', next_page)) {
      more_results <- FALSE
    } else {
      loaded_correctly <- checkload(next_page, 'gs_rt')
      if (loaded_correctly == F) {
        readline(prompt='Please, solve the CAPTCHA and press ENTER: ')
      }
    }
  }
}

# Closing browser
brClient$close()
br[["server"]]$stop()