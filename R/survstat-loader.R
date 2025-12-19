# .process_result = function(response) {
#   tmp = response
#   rows = as.character(xml2::xml_find_all(
#     tmp,
#     "//b:QueryResultRow/b:Caption/text()"
#   ))
#   cols = as.character(xml2::xml_find_all(tmp, "//b:Columns//b:Caption/text()"))
#   values = xml2::xml_find_all(tmp, "//b:QueryResultRow/b:Values/*") %>%
#     xml2::xml_text()
#   values = values %>%
#     stringr::str_remove_all("\\.") %>%
#     stringr::str_replace(",", ".") %>%
#     as.numeric()
#   if (length(values) != length(rows) * length(cols)) {
#     stop("SurvStat response is not an expected format")
#   }
#
#   df = tibble::tibble(
#     value = values,
#     col = rep(cols, times = length(rows)),
#     row = rep(rows, each = length(cols))
#   )
#
#   df2 = df %>%
#     # Exclude total columns
#     dplyr::filter(col != "Gesamt" & row != "Gesamt") %>%
#     dplyr::mutate(
#       age_start = col %>%
#         stringr::str_remove(stringr::fixed("A")) %>%
#         stringr::str_extract("^[0-9]+") %>%
#         as.numeric(),
#       age_end = col %>%
#         stringr::str_remove(stringr::fixed("A")) %>%
#         stringr::str_extract("[0-9]+$") %>%
#         as.numeric(),
#       age_cat = dplyr::case_when(
#         is.na(age_start) ~ "Unknown",
#         is.na(age_end) ~ sprintf("%d+", age_start),
#         age_end == age_start ~ sprintf("%d", age_start),
#         TRUE ~ sprintf("%d\u2013%d", age_start, age_end)
#       ),
#       year = row %>% stringr::str_extract("^[0-9]+") %>% as.numeric(),
#       week = row %>% stringr::str_extract("[0-9]+$") %>% as.numeric()
#     ) %>%
#     dplyr::mutate(
#       elapsed_week = (year - 2001) * 52 + week + (year - 2001) %/% 7
#     ) %>%
#     dplyr::group_by(
#       elapsed_week,
#       age_start,
#       age_end,
#       age_cat
#     ) %>%
#     dplyr::summarise(
#       value = sum(value, na.rm = TRUE),
#       .groups = "drop"
#     ) %>%
#     dplyr::mutate(
#       date = as.Date("2001-01-01") + elapsed_week * 7
#     ) %>%
#     dplyr::filter(
#       # Get rid of extra zeros at end
#       is.na(date) | date < Sys.Date()
#     )
#
#   levels = df2 %>%
#     dplyr::select(age_start, age_cat) %>%
#     dplyr::distinct() %>%
#     dplyr::arrange(age_start) %>%
#     dplyr::pull(age_cat) %>%
#     unique()
#
#   df2 = df2 %>% dplyr::mutate(age_cat = factor(age_cat, levels, ordered = TRUE))
#
#   return(df2)
# }

#' Retrieve time series data from the RKI SurvStat api.
#'
#' This function gets a weekly timeseries of disease count or incidence data
#' from the RKI `SurvStat` web API. The timeseries can be stratified by any
#' combination of age, geography, disease, disease subtype. Queries to the
#' API are cached and paged, but obviously multidimensional extracts have the
#' potential to need a lot of downloading.
#'
#' @param disease the disease of interest, see `rsurvstat::diseases`. This is
#'   technically optional, and if omitted the counts of all diseases will be
#'   returned.
#' @param measure one of `"Count"` (default) or `"Incidence"`
#' @param ... not used, must be empty.
#' @param age_group (optional) the age group of interest, see `rsurvstat::age_groups`
#' @param age_range (optional) a length 2 vector with the minimum and maximum ages to consider
#' @param disease_subtype if `TRUE` the returned count will be broken down by
#'   disease or pathogen subtype (assuming `disease` was provided).
#' @param years (optional) a vector of years to limit the response to.
#' @param geography (optional) a geographical breakdown can be given as a
#'   character where it must be one of `state`, `nuts`, or `county` which
#'   align to the 16 region `FedStateKey71Map`, 38 region `NutsKey71Map`,
#'   or 411 region `CountyKey71Map` data respectively. Alternatively it can be
#'   given as a subset of one of these maps, as a `sf` dataframe in which case
#'   only that subset of regions will be returned.
#' @param trim_zeros get rid of zero counts. Either "both" (from start and end),
#'   "leading" (from start only - the default) or "none".
#' @param .progress by default a progress bar is shown, which may be important
#'   if many downloads are needed to fulfil the request. It can be disabled here.
#'
#' @return a data frame with at least `date` (weekly), and one of `count` or
#'   `incidence` columns. Most likely it will also have `disease_name` and
#'   `disease_code` columns, and some of `age_name`, `age_code`, `age_low`,
#'   `age_high`, `geo_code`, `geo_name`, `disease_subtype_code`,
#'   `disease_subtype_name` depending on options. The dataframe will be grouped
#'   to make sure each group is a unique timeseries.
#' @export
#' @concept survstat
#'
#' @examples
#' get_timeseries(
#'   diseases$`COVID-19`,
#'   measure = "Count",
#'   age_group = age_groups$children_coarse
#' )
#'
#' get_timeseries(
#'   diseases$`COVID-19`,
#'   measure = "Count",
#'   age_group = age_groups$children_coarse,
#'   geography = rsurvstat::FedStateKey71Map[1:10,]
#' )
#'
#' get_timeseries(
#'   measure = "Count",
#'   years = 2024
#' )
get_timeseries = function(
  disease = NULL,
  measure = c("Count", "Incidence"),
  ...,
  age_group = NULL,
  age_range = c(0, Inf),
  disease_subtype = FALSE,
  years = NULL,
  geography = NULL,
  trim_zeros = c("leading", "both", "none"),
  .progress = TRUE
) {
  rlang::check_dots_empty()
  measure = match.arg(measure)
  trim_zeros = match.arg(trim_zeros)

  # The API can handle 2 dimensions per page. One dimension will always be time
  # in weeks. The second dimension is decided here. Usually it will be disease / disease_subtype
  # if > 1 of these, or geography or age category.
  # decide which dimension is going to be queried for as a column
  if (is.null(disease)) {
    coltype = "disease"
    colhier = as.character(
      hierarchy_list$notification_category$disease_pathogen$disease
    )
  } else if (isTRUE(disease_subtype)) {
    coltype = "disease_subtype"
    colhier = as.character(
      hierarchy_list$notification_category$disease_pathogen$disease$pathogenlevel_1
    )
  } else if (is.character(geography)) {
    coltype = "geo"
    if (geography %in% names(geography_resolution)) {
      geography = geography_resolution[[geography]]
    }
    colhier = geography
  } else if (is.character(age_group) && identical(age_range, c(0, Inf))) {
    coltype = "age"
    colhier = if (age_group %in% names(age_groups)) {
      age_groups[[age_group]]
    } else {
      age_group
    }
  }

  # Anything that is not the row dimension (time) or the column dimension (as
  # decided above), Has to be retrieved as a set of pages. There will be one
  # query for each of the pages.
  page_filters = NULL
  if (coltype != "disease") {
    page_filters = .cross_join_filters(page_filters, .disease_filter(disease))
  }
  if (coltype != "geo") {
    page_filters = .cross_join_filters(page_filters, .place_filter(geography))
  }
  if (coltype != "age") {
    page_filters = .cross_join_filters(
      page_filters,
      .age_filter(age_group, age_range)
    )
  }
  if (!is.null(years)) {
    page_filters = .cross_join_filters(
      page_filters,
      .year_filter(years)
    )
  }

  # This is the output dataframe:
  collect = NULL

  if (.progress) {
    cli::cli_progress_bar(total = length(page_filters))
  }
  if (is.null(page_filters)) {
    page_filters = list(NULL)
  }
  for (page in page_filters) {
    # Each item in the page filters list is itself a list of filters
    # that are added to the request for one `page` of results.
    # The page filters depend on options but might be a selection of
    # geography, age categories, or similar.

    tmp2 = .get_request(
      commands$olap_data,
      cube = cubes$survstat,
      language = languages$german,
      column_hierarchy = as.character(colhier),
      measure = measure,
      filters = page,
      row_hierarchy = "[ReportingDate].[YearWeek]"
    )

    tmp = try(tmp2 %>% .do_survstat_command(quiet = TRUE), silent = TRUE)

    # Do query and halt on error
    if (inherits(tmp, "try-error")) {
      cat(as.character(tmp2))
      stop(
        "Aborting as the SurvStat query returned an error.\n",
        "It may be because too much data was requested in one go.\n",
        "you can try chunking the data by year (using `years`)\n",
        "The error was:\n",
        tmp
      )
      break
    }

    if (.progress) {
      cli::cli_progress_update()
    }

    # Basically extrac the XML into a dataframe
    tmp = tmp %>% .process_olap_data_result()

    # Post process dates:
    # Firstly make sure weeks are unique:
    # because of the use of epidemic weeks the same week can be split across
    # year ends, and count as week 53, and week 1. We convert epiweeks to weeks
    # elapsed from "2021-01-01" and aggregate. We need to do this before being
    # combined with other pages
    tmp = tmp %>%
      dplyr::mutate(
        year = row_name %>% stringr::str_extract("^[0-9]+") %>% as.numeric(),
        week = row_name %>% stringr::str_extract("[0-9]+$") %>% as.numeric()
      ) %>%
      dplyr::mutate(
        elapsed_week = (year - 2001) * 52 + week + (year - 2001) %/% 7,
        value = ifelse(is.na(value), 0, value),
      ) %>%
      dplyr::group_by(col_name, col_code, elapsed_week) %>%
      dplyr::summarise(
        value = sum(value)
      ) %>%
      dplyr::mutate(
        date = as.Date("2001-01-01") + elapsed_week * 7 # This is a monday
      ) %>%
      dplyr::select(-elapsed_week)

    # Extract the values from the filters used to get this page of results
    # add add them into the dataframe before combining with other pages.

    if (!is.null(names(page))) {
      # Somehow this gets flattened when only one option. I cannot find out where
      # so I;ve put in an explicit check for it.
      page = list(page)
    }
    values = unlist(lapply(page, function(dim) dim$values), recursive = FALSE)
    tmp = tmp %>% dplyr::mutate(!!!values)

    collect = if (is.null(collect)) tmp else dplyr::bind_rows(tmp, collect)
  }

  # The column data will be different depending on the configuration maybe age,
  # maybe geography
  # Here we rename columns to whatever it was we set as the column dimension.
  colnames(collect) = gsub("col", coltype, colnames(collect), fixed = TRUE)

  # Fix age codes and break into age_name, age_low and age_high
  if ("age_code" %in% colnames(collect)) {
    tmp = .fmt_range(collect$age_code)
    collect = collect %>% dplyr::mutate(!!!tmp)
  }

  # Fix age codes and break into age_name, age_low and age_high
  if ("disease_name" %in% colnames(collect)) {
    collect = collect %>%
      dplyr::mutate(
        disease_name = ifelse(
          disease_name %in% diseases,
          names(diseases)[match(disease_name, diseases)],
          disease_name
        )
      )
  }

  # Fix grouping:
  collect = collect %>%
    dplyr::group_by(dplyr::across(
      -dplyr::any_of(c("date", "value", "year"))
    ))

  # Get rid of leading zeros
  if (trim_zeros != "none") {
    collect = collect %>%
      dplyr::filter(
        as.numeric(date) >
          suppressWarnings(min(as.numeric(date)[value != 0], na.rm = TRUE))
      )
  }
  # Get rid of trailing zeros
  if (trim_zeros == "both") {
    collect = collect %>%
      dplyr::filter(
        as.numeric(date) <=
          suppressWarnings(max(as.numeric(date)[value != 0], na.rm = TRUE))
      )
  }

  # rename "value" column to "count" or "incidence"
  collect = collect %>% dplyr::rename(!!(tolower(measure)) := value)

  if (.progress) {
    cli::cli_progress_done()
  }

  return(collect)
}


# Utility functions ----

# Take the output from the olap data quesies and extracts rows and columns
# into a long format dataframe, fixing escaping.
.process_olap_data_result = function(response) {
  tmp = response
  rows = xml2::xml_text(xml2::xml_find_all(
    tmp,
    "//b:QueryResultRow/b:Caption/text()"
  ))
  rowIds = xml2::xml_text(xml2::xml_find_all(
    tmp,
    "//b:QueryResultRow/b:RowName/text()"
  ))
  cols = xml2::xml_text(xml2::xml_find_all(
    tmp,
    "//b:Columns//b:Caption/text()"
  ))
  colIds = xml2::xml_text(xml2::xml_find_all(
    tmp,
    "//b:Columns//b:ColumnName/text()"
  ))

  values = xml2::xml_find_all(tmp, "//b:QueryResultRow/b:Values/*") %>%
    xml2::xml_text()
  values = values %>%
    stringr::str_remove_all("\\.") %>%
    stringr::str_replace(",", ".") %>%
    as.numeric()

  if (length(rows) == 0) {
    rows = FALSE
    rowIds = NA
  }
  if (
    length(cols) == 0 ||
      (length(cols) == 1 && startsWith(colIds, "[Measures]."))
  ) {
    cols = FALSE
    colsIds = NA
  }
  if (length(values) != length(rows) * length(cols)) {
    stop("SurvStat response is not an expected format")
  }

  df = tibble::tibble(
    value = values,
    col_name = rep(cols, times = length(rows)),
    col_code = rep(colIds, times = length(rows)),
    row_name = rep(rows, each = length(cols)),
    row_code = rep(rowIds, each = length(cols)),
  )

  df2 = df %>%
    # Exclude total columns
    dplyr::filter(col_name != "Gesamt" & row_name != "Gesamt") %>%
    dplyr::filter(!is.na(col_name) & !is.na(row_name))

  if (isFALSE(rows)) {
    df2 = df2 %>% dplyr::select(-row_name, -row_code)
  }
  if (isFALSE(cols)) {
    df2 = df2 %>% dplyr::select(-col_name, -col_code)
  }

  return(df2)
}


# Format the age code into a prettier format.
.fmt_range = function(v) {
  l = as.numeric(stringr::str_extract(v, "\\[A([0-9]+)", 1))
  h = as.numeric(stringr::str_extract(v, "([0-9]+)\\]$", 1)) + 1
  f = dplyr::case_when(
    is.na(h) ~ sprintf("%d+", l),
    l == h - 1 ~ sprintf("%d", l),
    TRUE ~ sprintf("%d\u2013%d", l, h - 1)
  )
  return(list(age_low = l, age_high = h, age_name = f))
}

# Filters ----

# Construct a filter for selecting one disease
.disease_filter = function(disease) {
  if (is.null(disease)) {
    return(NULL)
  }
  disease_nm = disease
  if (disease %in% names(diseases)) {
    disease = diseases[[disease]]
  }

  disease_code = sprintf(
    "[KategorieNz].[Krankheit DE].&[%s]",
    disease
  )

  list(list(
    values = list(
      disease_name = disease_nm,
      disease_code = disease_code
    ),
    dimension_id = "[PathogenOut].[KategorieNz]",
    hierarchy_id = "[PathogenOut].[KategorieNz].[Krankheit DE]",
    hierarchy_value = disease_code
  ))
}


# .match_values(hierarchy_list$time$seasonweek_27_)
.year_filter = function(years) {
  if (is.null(years)) {
    return(NULL)
  }
  lapply(years, function(v) {
    list(
      values = list(year = v),
      dimension_id = "[ReportingDate]",
      hierarchy_id = "[ReportingDate].[WeekYear]",
      hierarchy_value = sprintf("[ReportingDate].[WeekYear].&[%d]", v)
    )
  })
}

# Create a filter for a set of ages where the standard grop is being
# reduced (or this is just being done as a multipage where each page has a
# single age group)
# .match_values(age_groups$single_year)
# give use the kind of output codes we would be expecting
.age_filter = function(
  age_group,
  age_range = c(0, Inf)
) {
  if (is.null(age_group)) {
    return(NULL)
  }
  if (age_group %in% names(age_groups)) {
    age_group = age_groups[[age_group]]
  }
  age_group_values = .match_values(age_group)
  age_data = .fmt_range(age_group_values)

  low = age_data$age_low
  high = age_data$age_high
  high[is.na(high)] = 120
  age_group_values = age_group_values[
    min(age_range) <= as.numeric(low) &
      max(age_range) > as.numeric(high)
  ]

  unname(lapply(age_group_values, function(v) {
    list(
      values = list(
        age_code = v
      ),
      dimension_id = age_groups,
      hierarchy_id = age_groups,
      hierarchy_value = v
    )
  }))
}


# The geographic filters (if not primary row variable)
# map is either a character "state", "nuts2" or "county", or a map with one of
# FedStateKey71Map, NutsKey71Map, CountyKey71Map
.place_filter = function(
  map_sf
) {
  if (is.null(map_sf)) {
    return(NULL)
  }
  if (is.character(map_sf)) {
    map_sf = switch(
      map_sf,
      "state" = rsurvstat::FedStateKey71Map,
      "nuts" = rsurvstat::NutsKey71Map,
      "county" = rsurvstat::CountyKey71Map
    )
  }
  values = list()

  tmp = purrr::pmap(
    map_sf,
    function(Id, HierarchyId, ComponentId, Name, ...) {
      list(
        values = list(
          geo_name = Name,
          # geo_code = stringr::str_extract(ComponentId, "\\[([^\\]]+)\\]$", 1),
          # geo_code_type = stringr::str_extract(
          #   HierarchyId,
          #   "\\[([^\\]]+)\\]$",
          #   1
          # ),
          geo_code = Id
        ),
        dimension_id = as.character(
          hierarchy_list$place$state_territorial_unit_county
        ),
        hierarchy_id = HierarchyId,
        hierarchy_value = Id
      )
    }
  )
}


# Filter to specific time point(s) more generally than the year filter
# .time_filter = function() {
# }

# Filter utils ----

# Create a list with unique conbinations of input and
# devtools::load_all()
# l1 = list(a="A",b="B")
# l2 = list(c="C",d="D")
# l3 = list(e="E",f="F")
# l4 = list(g="G",h="H")
# l5 = list(i="I",j="J")
# tmp = .cross_join_filters(list(l1,l2), list(l3))
# tmp = .cross_join_filters(tmp, list(l4, l5))
# .tree(tmp)
# .tree(.cross_join_filters(l1,NULL))
# .tree(.cross_join_filters(NULL,list(l1,l2)))
.cross_join_filters = function(list1, list2) {
  if (is.null(list2)) {
    if (!is.null(names(list1))) {
      list1 = list(list1)
    }
    return(list1)
  }
  if (is.null(list1)) {
    if (!is.null(names(list2))) {
      list2 = list(list2)
    }
    return(list2)
  }
  return(
    unlist(
      lapply(list1, function(item1) {
        lapply(list2, function(item2) {
          # So not sure whether this is really working
          if (!is.null(names(item1))) {
            item1 = list(item1)
          }
          if (!is.null(names(item2))) {
            item2 = list(item2)
          }
          c(item1, item2)
        })
      }),
      recursive = FALSE
    )
  )
}
