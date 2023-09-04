library(RSelenium)
library(magrittr)
library(urltools)
library(stringr)

id_gsp <- readLines('profile_to_download.txt')[1]
file.remove('profile_to_download.txt')

# Check if page shows captcha
checkload <- function(url) {
  loaded_correctly <- FALSE
  
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

# Get raw html of author profile (all documents)
get_id_gsp <- function(x) {
  param_get(urls = x, parameter_names = 'user') %>% 
    .[1,1]
}

id_profile <- ifelse(is.na(get_id_gsp(id_gsp)), id_gsp, get_id_gsp(id_gsp))
url_profile <- paste0('https://scholar.google.com/citations?user=', id_profile)

gc()
br <- rsDriver(browser = "firefox", port = 4445L)
brClient <- br[["client"]]

brClient$navigate(url_profile)
loaded_correctly <- checkload(url_profile)
if (loaded_correctly == F) {
  readline(prompt='Please, solve the CAPTCHA and press ENTER: ')
}

# check whether download directory exists, create if not
profile_dir <- paste0('data/', id_gsp, '.', as.character(Sys.Date()))
if (!dir.exists(profile_dir)) {
  dir.create(profile_dir)
}

# Displaying all documents in profile
show_more <- T
show_more_el <- brClient$findElement(using = 'id', 'gsc_bpf_more')
while (show_more == T) {
  Sys.sleep(2)
  if(length(show_more_el$getElementAttribute("disabled")) == 0) {
    show_more_el$clickElement()
  } else {
    show_more <- F
  }
}

# Getting HTML
html <- brClient$findElement(using = 'xpath', '//html')$getElementAttribute('innerHTML')

# Saving file
write_path <- paste(profile_dir, '/profile.', id_profile, '.html', sep="")
html_connection <- file(write_path, encoding = 'UTF-8')
write(html[[1]], html_connection)

# Saving profile image
img_url <- brClient$findElement(using = 'xpath', '//div[@id="gsc_prf_pua"]/img')$getElementAttribute('src')[[1]]
print(img_url)
download.file(img_url, paste0('www/', id_profile, '.jpeg'))

# Closing browser
close(html_connection)
brClient$close()
br[["server"]]$stop()

write_path
