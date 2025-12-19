library(tidyverse)
devtools::load_all()

# Dimensions
dims_req = .get_request(
  command = commands$dimensions,
  cube = cubes$survstat,
  language = languages$english
)
dims_xml = .do_survstat_command(command = commands$dimensions, dims_req)

tmp = xml2::as_list(dims_xml)
dims = tmp$Envelope$Body$GetAllDimensionsResponse$GetAllDimensionsResult$Dimensions
dimensions_df = dims %>% .transpose()

# dimensions_level_0 = dimensions_df %>%
#   dplyr::pull(Hierarchies) %>%
#   dplyr::bind_rows()
#
# dimensions_level_1 = dimensions_level_0 %>%
#   dplyr::pull(ChildHierarchies) %>%
#   dplyr::bind_rows()
#
# dimensions_level_2 = dimensions_level_1 %>%
#   dplyr::pull(ChildHierarchies) %>%
#   dplyr::bind_rows()
#
# dimensions_level_3 = dimensions_level_2 %>%
#   dplyr::pull(ChildHierarchies) %>%
#   dplyr::bind_rows()
#
# dimensions_flat = dplyr::bind_rows(
#   dimensions_level_0 %>% dplyr::select(-ChildHierarchies),
#   dimensions_level_1 %>% dplyr::select(-ChildHierarchies),
#   dimensions_level_2 %>% dplyr::select(-ChildHierarchies),
#   dimensions_level_3 %>% dplyr::select(-ChildHierarchies)
# )
# dimensions_flat %>% view()

# Dimension hierarchy Xmap
dim_hier_xmap_0 = dimensions_df %>%
  select(DimensionId = Id., DimensionCaption = Caption., Hierarchies) %>%
  unnest(Hierarchies)

dim_hier_xmap = NULL
dim_hier_xmap_i = dim_hier_xmap_0
i = 1
while (nrow(dim_hier_xmap_i) > 0) {
  dim_hier_xmap_i = dim_hier_xmap_i %>%
    select(
      starts_with("Dimension"),
      HierarchyId = Id.,
      HierarchyCaption = Caption.,
      ChildHierarchies
    )
  dim_hier_xmap = bind_rows(
    dim_hier_xmap,
    dim_hier_xmap_i %>% mutate(Level = i) %>% select(-ChildHierarchies)
  )
  i = i + 1
  dim_hier_xmap_i = dim_hier_xmap_i %>%
    select(-starts_with("Hierarchy")) %>%
    filter(sapply(ChildHierarchies, is.list)) %>%
    unnest(ChildHierarchies)
}

dim_hier_xmap

# Hierarchies
# Seems redundant as info in Dimensions hierarchy
# hierarchy_req = .get_request(
#   command = commands$hierarchies,
#   cube = cubes$survstat,
#   language = languages$english
# )
# hierarchy_xml = .do_survstat_command(
#   command = commands$hierarchies,
#   hierarchy_req
# )
#
# tmp = xml2::as_list(hierarchy_xml)
#
# hiers = tmp$Envelope$Body$GetAllHierarchiesResponse$GetAllHierarchiesResult$Hierarchies
#
# hierarchy_df = hiers %>% .transpose()

# Hierarchy members
hierarchy_members_req = .get_request(
  command = commands$members,
  cube = cubes$survstat,
  language = languages$english
)
hierarchy_members_xml = .do_survstat_command(
  command = commands$members,
  hierarchy_members_req
)

tmp = xml2::as_list(hierarchy_members_xml)

hier_members = tmp$Envelope$Body$GetAllHierarchyMembersResponse$GetAllHierarchyMembersResult$HierarchyMembers

hierarchy_members_df = hier_members %>% .transpose()
# hierarchy_members_df %>%
#   filter(stringr::str_detect(
#     Id.,
#     stringr::fixed("[KategorieNz].[Pathogen1 Nz]")
#   )) %>%
#   view()

do_pick = function(ids, index, from = "head", by = ".", join = by) {
  splits = strsplit(ids, by, fixed = TRUE)
  lengths = sapply(splits, length)
  if (from == "tail") {
    indexes = lapply(lengths, function(l) rev(rev(seq_len(l))[index]))
  } else {
    indexes = lapply(lengths, function(l) seq_len(l)[index])
  }
  picked = sapply(seq_along(splits), function(i) {
    paste0(splits[[i]][indexes[[i]]], collapse = join)
  })
  return(picked)
}

nest_hm = hierarchy_members_df %>%
  mutate(
    DimensionId = do_pick(Id., 1:2),
    ParentId = do_pick(Id., -1, from = "tail", by = ".&"),
    ComponentId = ifelse(
      stringr::str_detect(Id., ".&"),
      do_pick(Id., 1, from = "tail", by = ".&"),
      NA
    ),
    Level = stringr::str_match_all(Id., "\\].&\\[") %>%
      lapply(length) %>%
      unlist()
  ) %>%
  rename(Id = Id.) %>%
  filter(Level > 0) %>%
  glimpse()

lvl_0 = nest_hm %>%
  inner_join(dim_hier_xmap, by = c("DimensionId", "Level")) %>%
  rename(ComponentCaption = Caption.) %>%
  glimpse()
lvl_i = lvl_0
# i = max(nest_hm$Level)-1
for (i in rev(seq_len(max(nest_hm$Level) - 1))) {
  lvl_i = lvl_i %>%
    filter(Level == i + 1) %>%
    tidyr::nest(Subtypes = -c(ParentId))
  lvl_i = lvl_0 %>%
    filter(Level == i) %>%
    left_join(lvl_i, by = c("Id" = "ParentId"))
}

# lvl_0 %>% filter(Level == 4) %>% glimpse()
# lvl_i %>% glimpse()
# lvl_i %>% filter(!sapply(Subtypes, is.null))

nest_hm_2 = lvl_i %>%
  mutate(ParentId = DimensionId) %>%
  tidyr::nest(Options = -ParentId) %>%
  rename(DimensionId = ParentId)

# Measures
# Not really seud as
# measures_req = .get_request(
#   command = commands$measures,
#   cube = cubes$survstat,
#   language = languages$english
# )
# measures_xml = .do_survstat_command(command = commands$measures, measures_req)
#
# tmp = xml2::as_list(measures_xml)
#
# measures = tmp$Envelope$Body$GetAllMeasuresResponse$GetAllMeasuresResult$Measures

# Hierarchy members
# Cannot make work
# hierarchy_exist_req = .get_request(
#   command = commands$existing_members,
#   cube = cubes$survstat,
#   language = languages$german,
#   filters = list(list(
#     dimension_id = "[Datenstand].[Publikation]",
#     hierarchy_id = "[Datenstand].[Publikation].[Publikation]",
#     hierarchy_value = "[Datenstand].[Publikation].&[-1]"
#   )),
#   hierarchy_id = "[PathogenOut].[KategorieNz].[Krankheit DE]"
# )
#
# hierarchy_exist_xml = .do_survstat_command(
#   command = commands$existing_members,
#   hierarchy_exist_req
# )
#
# tmp = xml2::as_list(hierarchy_exist_xml)
#
# tmp$Envelope$Body

hiers = nest_hm_2 %>%
  inner_join(dimensions_df, by = c("DimensionId" = "Id.")) %>%
  rename(Subtypes = Options, ComponentCaption = Caption.) %>%
  mutate(
    Id = NA,
    Group = ifelse(is.na(Group), "Publication", Group)
  ) %>%
  glimpse()

.normalise = function(x) {
  tolower(gsub("[^a-zA-Z0-9]+", "_", trimws(x, whitespace = " |_")))
}

values_df = hiers %>%
  group_by(Group) %>%
  summarise(
    Options = list(map_option_data(
      dplyr::pick(everything()),
      children_fn = ~ .x[["Subtypes"]],
      names_fn = ~ .normalise(.x[["ComponentCaption"]]),
      values_fn = ~ .x[["Id"]]
    ))
  )

value_list = values_df$Options
names(value_list) = values_df$Group
value_list = .fix_names(value_list)

# usethis::use_data(value_list, internal = TRUE)

hierarchy_df = hiers %>%
  rename(ChildHierarchies = Hierarchies) %>%
  mutate(
    Id. = DimensionId,
    Caption. = ComponentCaption,
    Group = ifelse(is.na(Group), "Publication", Group)
  ) %>%
  group_by(Group) %>%
  summarise(
    Hierarchies = list(map_option_data(
      dplyr::pick(everything()),
      children_fn = ~ .x[["ChildHierarchies"]],
      names_fn = ~ .normalise(.x[["Caption."]]),
      values_fn = ~ .x[["Id."]]
    ))
  )

hierarchy_list = hierarchy_df$Hierarchies
names(hierarchy_list) = hierarchy_df$Group
hierarchy_list = .fix_names(hierarchy_list)

# usethis::use_data(hierarchy_list, internal = TRUE)

dimensions_df = hiers %>%
  group_by(Group) %>%
  # group_modify(function(d,g,...) {
  #   tmp = lapply(d$DimensionId, as.survstat_option, children=NULL)
  #   names(tmp) = .normalise(d$ComponentCaption)
  # })
  summarise(Dimension = {
    d = dplyr::pick(everything())
    tmp = lapply(d$DimensionId, as.survstat_option, children = NULL)
    names(tmp) = .normalise(d$ComponentCaption)
    list(tmp)
  })

dimensions_list = dimensions_df$Dimension
names(dimensions_list) = dimensions_df$Group
dimensions_list = .fix_names(dimensions_list)

usethis::use_data(
  value_list,
  hierarchy_list,
  dimensions_list,
  overwrite = TRUE,
  internal = TRUE
)
