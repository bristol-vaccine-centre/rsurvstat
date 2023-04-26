## code to prepare `age_groups` dataset goes here

age_groups = list(
  single_year = "[AlterPerson80].[AgeGroupName8]",
  children_coarse = "[AlterPerson80].[AgeGroupName3]", #from 0, 15, 20, 25, 30, 40, 50, 60, 70, 80 years
  children_medium = "[AlterPerson80].[AgeGroupName2]", #from 0, 5, 10, 15, 20, 25, 30, 40, 50, 60, 70, 80 years
  children_fine = "[AlterPerson80].[AgeGroupName1]", #from 0, 1, 2, 3, 4, 5, 10, 15, 20, 25, 30, 40, 50, 60, 70, 80 years
  five_year = "[AlterPerson80].[AgeGroupName6]", #from 0, 1, 5, 10, 15, 20, … , 75, 80 years
  zero_fifteen  = "[AlterPerson80].[AgeGroupName4]", #from 0, 15+
  zero_fifteen_sixty  = "[AlterPerson80].[AgeGroupName5]", #from 0, 15, 60+
  zero_one_4_20_40_60_80 = "[AlterPerson80].[AgeGroupName7]" #from 0, 15, 60+
)

usethis::use_data(age_groups, overwrite = TRUE)
