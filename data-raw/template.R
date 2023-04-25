## code to prepare `template` dataset goes here
here::i_am("data-raw/template.R")
loc = here::here("data-raw/soap-get-olap-data.xml")
if(!fs::file_exists(loc)) stop("Could not find: ",loc)
template = readr::read_file(file = loc)
Encoding(template) <- "latin1"
template <- iconv(
  template, 
  "latin1", 
  "UTF-8"
)
usethis::use_data(template, overwrite = TRUE, internal = TRUE)
