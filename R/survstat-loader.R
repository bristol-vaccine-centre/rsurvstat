.get_template = function(disease, measure, age_group) {
  body = template %>% stringr::str_replace(
    stringr::fixed("{{disease}}"), disease
  ) %>% stringr::str_replace(
    stringr::fixed("{{measure}}"), measure
  ) %>% stringr::str_replace(
    stringr::fixed("{{age_group}}"), age_group
  )
  return(body)
}

.do_soap_curl = function(request, quiet = FALSE) {
  if(!quiet) message("Making survstat request...",appendLF = FALSE)
  req = httr2::request("https://tools.rki.de/SurvStat/SurvStatWebService.svc") %>%
    httr2::req_headers(
      Accept = "text/xml",
      Accept = "multipart/*",
      Action = '"http://tools.rki.de/SurvStat/SurvStatWebService/GetOlapData"'
    ) %>%
    httr2::req_body_raw(body = request, type = "application/soap+xml;charset=utf-8")
  
  resp = tryCatch(
    req %>% httr2::req_perform(),
    error = function(e) stop("SSL problem: ", curl::curl_version(), e)
  )
  
  if(!quiet) message("Data downloaded.")
  resp %>% httr2::resp_body_xml() %>% return()
}

.process_result = function(response) {
  
  tmp = response
  rows = as.character(xml2::xml_find_all(tmp, "//b:QueryResultRow/b:Caption/text()"))
  cols = as.character(xml2::xml_find_all(tmp, "//b:Columns//b:Caption/text()"))
  values = xml2::xml_find_all(tmp, "//b:QueryResultRow/b:Values/*") %>% xml2::xml_text()
  values = values %>% stringr::str_remove_all("\\.") %>% stringr::str_replace(",",".") %>% as.numeric()
  if (length(values) != length(rows)*length(cols)) stop("SurvStat response is not an expected format")
  
  df = tibble::tibble(
    value = values,
    col = rep(cols, times=length(rows)), 
    row = rep(rows, each=length(cols))
  )
  
  df2 = df %>% 
    # Exclude total columns
    dplyr::filter(col != "Gesamt" & row != "Gesamt") %>%
    dplyr::mutate(
    age_start = col %>% stringr::str_remove(stringr::fixed("A")) %>% stringr::str_extract("^[0-9]+") %>% as.numeric(),
    age_end = col %>% stringr::str_remove(stringr::fixed("A")) %>% stringr::str_extract("[0-9]+$") %>% as.numeric(),
    age_cat = dplyr::case_when(
      is.na(age_start) ~ "Unknown",
      is.na(age_end) ~ sprintf("%d+", age_start),
      age_end == age_start ~ sprintf("%d", age_start),
      TRUE ~ sprintf("%d\u2013%d", age_start, age_end)
    ),
    year = row %>% stringr::str_extract("^[0-9]+") %>% as.numeric(),
    week = row %>% stringr::str_extract("[0-9]+$") %>% as.numeric()
  ) %>% dplyr::mutate(
    elapsed_week = (year-2001)*52+ week+ (year-2001)%/%7
  ) %>% dplyr::group_by(
    elapsed_week,
    age_start, age_end, age_cat
  ) %>% dplyr::summarise(
    value = sum(value,na.rm = TRUE),
    .groups = "drop"
  ) %>% dplyr::mutate(
    date = as.Date("2001-01-01")+elapsed_week*7
  ) %>% dplyr::filter(
    # Get rid of extra zeros at end
    is.na(date) | date < Sys.Date()
  )
  
  levels = df2 %>% dplyr::select(age_start, age_cat) %>% 
    dplyr::distinct() %>%
    dplyr::arrange(age_start) %>%
    dplyr::pull(age_cat) %>%
    unique()
  
  df2 = df2 %>% dplyr::mutate(age_cat = factor(age_cat,levels,ordered = TRUE))
  
  return(df2)
}


#' Extract age stratified case count of disease positive cases.
#' 
#' N.b. its not possible to differentiate between missing data and zero counts
#' in the source dataset.
#'
#' @param disease the disease of interest, see `rsurvstat::diseases`
#' @param measure one of "Count" or "Incidence"
#' @param age_group the age_group of interest, see `rsurvstat::age_groups`
#' @param quiet suppress loading messages
#' @param trim_zeros get rid of zero counts. Either "both" (from start and end),
#'   "leading" (from start only - the default) or "none".
#'
#' @return a data frame with age_cat (ordered factor), age_start, age_end, elapsed_week (weeks since 2020-12-31), date
#'   (start of week date approximate) and one of count, incidence or population
#'   columns
#' @export
#'
#' @examples
#' get_timeseries(diseases$`COVID-19`, measure = "Count", age_group = age_groups$zero_fifteen_sixty)
get_timeseries = function(
    disease = diseases$`COVID-19`, 
    measure = c("Count","Incidence"), 
    age_group = age_groups$single_year,
    quiet = FALSE, 
    trim_zeros = c("leading","both","none")) {
  
  measure = match.arg(measure)
  trim_zeros = match.arg(trim_zeros)
  
  tmp = .get_template(disease, measure, age_group) %>%
    .do_soap_curl(quiet) 
  
  tmp = tmp %>%
    .process_result() 
  
  if (trim_zeros != "none") {
    
    first = tmp %>% dplyr::filter(value > 0) %>% dplyr::pull(date) %>% 
      min() %>% as.Date("1970-01-01")
    tmp = tmp %>% dplyr::filter(date >= first)
    
    if (trim_zeros != "leading") {
      last = tmp %>% dplyr::filter(value > 0) %>% dplyr::pull(date) %>% 
        max() %>% as.Date("1970-01-01")
      tmp = tmp %>% dplyr::filter(date <= last)
    }
    
  }
  
  tmp = tmp %>% dplyr::rename(!!(tolower(measure)) := value) 
  
  return(tmp)
}

