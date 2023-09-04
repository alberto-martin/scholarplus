library(RSelenium)
library(magrittr)
library(urltools)
library(stringr)

urls <- readLines(paste0(citing_urls_path, '/doc_details_urls.txt'))


# Check if page shows captcha
checkload <- function(url) {
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
art_dir <- paste0(citing_urls_path, '/doc_details')
if (!dir.exists(art_dir)) {
  dir.create(art_dir)
}

for (c_url in urls) {
  id_doc <- param_get(urls = c_url, parameter_names = 'citation_for_view') %>% 
    .[1,1] %>% 
    substring(14)
  
  brClient$navigate(c_url)
  
  html <- brClient$findElement(using = 'xpath', '//html')$getElementAttribute('innerHTML')
  loaded_correctly <- checkload(c_url)
  if (loaded_correctly == F) {
    readline(prompt='Please, solve the CAPTCHA and press ENTER: ')
  }
  
  write_path <- paste(art_dir, '/', id_doc, '.html', sep="")
  
  write(html[[1]], write_path)
  write_path
}

# Closing browser
close(html_connection)
brClient$close()
br[["server"]]$stop()