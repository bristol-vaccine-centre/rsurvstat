## code to prepare `template` dataset goes here
here::i_am("data-raw/template.R")
loc = here::here("data-raw/soap-get-olap-data.xml")
if(!fs::file_exists(loc)) stop("Could not find: ",loc)
template = readr::read_file(file = loc)

usethis::use_data(template, overwrite = TRUE, internal = TRUE)
