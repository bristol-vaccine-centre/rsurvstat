.get_template = function(disease) {
  body = template %>% stringr::str_replace(
    stringr::fixed("{{disease}}"),
    disease
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
  
  if ("ssl_op_all" %in% names(curl::curl_options())) {
    # attempt to detect and fix error:0A000126:SSL routines::unexpected eof while reading
    req = req %>%
      httr2::req_options(ssl_op_all = TRUE)
  }
  
  resp = tryCatch(
    req %>% httr2::req_perform(),
    error = function(e) stop("SSL problem: ", curl::curl_version(), e)
  )
  
  if(!quiet) message("Data downloaded.")
  resp %>% httr2::resp_body_xml() %>% return()
}

.process_result = function(response) {
  response = xml2::as_list(response)
  
  # data
  tmp = response$Envelope$Body$GetOlapDataResponse$GetOlapDataResult$QueryResults
  
  # column headers
  tmp2 = response$Envelope$Body$GetOlapDataResponse$GetOlapDataResult$Columns
  cols = unname(unlist(lapply(tmp2,function(x) unlist(x$ColumnName))))
  
  df = dplyr::bind_rows(lapply(tmp, function(x) {
    tmp3 = sapply(x$Values, function(x) if(length(x)==0) return(0) else return(as.numeric(unlist(x))))
    names(tmp3) = cols
    tibble::enframe(tmp3) %>% dplyr::mutate(caption = unlist(x$Caption))
  }))
  
  df2 = df %>% dplyr::mutate(
    age = name %>% stringr::str_remove(stringr::fixed("[AlterPerson80].[AgeGroupName8].&[A")) %>% stringr::str_extract("^[0-9]+") %>% as.numeric(),
    year = caption %>% stringr::str_extract("^[0-9]+") %>% as.numeric(),
    week = caption %>% stringr::str_extract("[0-9]+$") %>% as.numeric()
  ) %>% dplyr::mutate(
    elapsed_week = (year-2001)*52+week
  ) %>% dplyr::group_by(
    elapsed_week,
    age
  ) %>% dplyr::summarise(
    count = sum(value),
    .groups = "drop"
  ) %>% dplyr::mutate(
    date = as.Date("2001-01-01")+elapsed_week*7
  ) %>% dplyr::filter(
    !is.na(age) & !is.na(date) &
      date < Sys.Date()
  )
  return(df2)
}


#' Extract age stratified case count of disease positive cases.
#' 
#' N.b. its not possible to differentiate between missing data and zero counts
#' in the source dataset.
#'
#' @param disease the disease of interest, see `rsurvstat::diseases`
#' @param quiet suppress loading messages
#'
#' @return a data frame with age, elapsed_week (weeks since 2020-12-31), date
#'   (start of week date approximate) and count columns
#' @export
#'
#' @examples
#' get_timeseries(diseases$`COVID-19`)
get_timeseries = function(disease = diseases$`COVID-19`, quiet = FALSE) {
  .get_template(disease) %>%
    .do_soap_curl(quiet) %>%
    .process_result() %>%
    return()
}

