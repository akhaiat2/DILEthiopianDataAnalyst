#Import library to translate .dta file to R
install.packages('tidyverse')
library('haven')

#Inspecting data

#Reading Conversion ET_Local_area_unit.dta
dtafile <- file.path(getwd(), "Desktop", "data", "ET_local_area_unit_conversion.dta")
study_df <- read_dta(dtafile) 

#Reading Conversion Crop_CF_Wave4.dta
dtafileCropCFWave4Path <- file.path(getwd(), "Desktop", "all data", "Crop_CF_Wave4.dta")
dtafileCropCFWave4 <- read_dta(dtafileCropCFWave4Path) 

#Reading Conversion ETH_HouseholdGeovariables_Y4.dta
dtafileETH_HH_Path <- file.path(getwd(), "Desktop", "all data", "ETH_HouseholdGeovariables_Y4.dta")
dtafileETH_HH <- read_dta(dtafileETH_HH_Path) 

#Reading Conversion ETH_PlotGeovariables_Y4.dta
dtafileETH_PlotGeovariables_Y4_Path <- file.path(getwd(), "Desktop", "all data", "ETH_PlotGeovariables_Y4.dta")
dtafileETH_PlotGeovariables_Y4 <- read_dta(dtafileETH_PlotGeovariables_Y4_Path) 

#Reading Conversion sect_cover_hh_w4.dta
dtafilesect_cover_hh_w4_Path <- file.path(getwd(), "Desktop", "all data", "sect_cover_hh_w4.dta")
dtafilesect_cover_hh_w4 <- read_dta(dtafilesect_cover_hh_w4_Path) 

#Reading Conversion sect_cover_ls_w4.dta
dtafilesect_cover_ls_w4_Path <- file.path(getwd(), "Desktop", "all data", "sect_cover_ls_w4.dta")
dtafilesect_cover_ls_w4 <- read_dta(dtafilesect_cover_ls_w4_Path) 

#Reading Conversion sect_cover_ph_w4.dta
dtafilesect_cover_ph_w4_Path <- file.path(getwd(), "Desktop", "all data", "sect_cover_ph_w4.dta")
dtafilesect_cover_ph_w4 <- read_dta(dtafilesect_cover_ph_w4_Path) 

#Reading Conversion sect_cover_pp_w4.dta
dtafilesect_cover_pp_w4_Path <- file.path(getwd(), "Desktop", "all data", "sect_cover_pp_w4.dta")
dtafilesect_cover_pp_w4 <- read_dta(dtafilesect_cover_pp_w4_Path) 

#Reading Conversion sect1_hh_w4.dta
dtafilesect1_hh_w4_Path <- file.path(getwd(), "Desktop", "all data", "sect1_hh_w4.dta")
dtafilesect1_hh_w4 <- read_dta(dtafilesect1_hh_w4_Path) 

#Reading Conversion sect06_com_w4.dta
dtafilesect06_com_w4_Path <- file.path(getwd(), "Desktop", "all data", "sect06_com_w4.dta")
dtafilesect06_com_w4 <- read_dta(dtafilesect06_com_w4_Path) 

#Reading Conversion sect6a_hh_w4.dta
dtafilesect6a_hh_w4_Path <- file.path(getwd(), "Desktop", "all data", "sect06_com_w4.dta")
dtafilesect6a_hh_w4 <- read_dta(dtafilesect6a_hh_w4_Path) 

#Reading Conversion sect6b1_hh_w4.dta
dtafilesect6b1_hh_w4_Path <- file.path(getwd(), "Desktop", "all data", "sect6b1_hh_w4.dta")
dtafilesect6b1_hh_w4 <- read_dta(dtafilesect6b1_hh_w4_Path) 

#Reading Conversion sect6b3_hh_w4.dta
dtafilesect6b3_hh_w4_Path <- file.path(getwd(), "Desktop", "all data", "sect6b3_hh_w4.dta")
dtafilesect6b3_hh_w4 <- read_dta(dtafilesect6b3_hh_w4_Path) 

##############################################################################################################

# Unit Conversion Function 

#WARNING: THIS FUNCTION MAY NOT INCLUDE SOME CONVERSIONS AS IT WAS NOT GIVEN IN THE CONVERSION DATASET (Food_CF_Wave4.dta for weight conversion to KGs)
unit_converter <- function(filtered_data, unit_code_column) {
  for (i in 1:nrow(filtered_data)) {
    
    filtered_unit_code = as.double(filtered_data[i, unit_code_column])
    filtered_quantity = filtered_data[i,unit_code_column - 1]
    
    if (filtered_unit_code != "1")
    {
      else if (filtered_unit_code == "2") {
        filtered_data[i, unit_code_column - 1] <- filtered_quantity / 1000
      }
      else if (filtered_unit_code == "3") {
        filtered_data[i, unit_code_column - 1] <- filtered_quantity * 100
      }
      elseif (filtered_unit_code == "8") {
        filtered_data[i, unit_code_column - 1] <- filtered_quantity * 0.988
      }
      else if (filtered_unit_code == "21") {
        filtered_data[i, unit_code_column - 1] <- filtered_quantity * 27.633
      }
      else if (filtered_unit_code == "22") {
        filtered_data[i, unit_code_column - 1] <- filtered_quantity * 50.333
      }
      else if (filtered_unit_code == "81") {
        filtered_data[i, unit_code_column - 1] <- filtered_quantity * 37.813  
      }
      else if (filtered_unit_code == "82") {
        filtered_data[i, unit_code_column - 1] <- filtered_quantity * 70.346
      }
      else if (filtered_unit_code == "83") {
        filtered_data[i, unit_code_column - 1] <- filtered_quantity * 106.446
      }
      else if (filtered_unit_code == "92") {
        filtered_data[i, unit_code_column - 1] <- filtered_quantity * 21.070
      }
      else if (filtered_unit_code == "113") {
        filtered_data[i, unit_code_column - 1] <- filtered_quantity * 24.438
      }
      else if (filtered_unit_code == "121") {
        filtered_data[i, unit_code_column - 1] <- filtered_quantity * 37.813
      }
      else if (filtered_unit_code == "123") {
        filtered_data[i, unit_code_column - 1] <- filtered_quantity * 106.446
      }
      else if (filtered_unit_code == "131") {
        filtered_data[i, unit_code_column - 1] <- filtered_quantity * 1.500
      }
      else if (filtered_unit_code == "132") {
        filtered_data[i, unit_code_column - 1] <- filtered_quantity * 2.000
      }
      else if (filtered_unit_code == "151") {
        filtered_data[i, unit_code_column - 1] <- filtered_quantity * 0.911
      }
      else if (filtered_unit_code == "152") {
        filtered_data[i, unit_code_column - 1] <- filtered_quantity * 1.429
      }
      else if (filtered_unit_code == "153") {
        filtered_data[i, unit_code_column - 1] <- filtered_quantity * 2.325
      }
      else if (filtered_unit_code == "181") {
        filtered_data[i, unit_code_column - 1] <- filtered_quantity * 0.438
      }
      else if (filtered_unit_code == "182") {
        filtered_data[i, unit_code_column - 1] <- filtered_quantity * 0.790
      }
      else if (filtered_unit_code == "183") {
        filtered_data[i, unit_code_column - 1] <- filtered_quantity * 1.073
      }
      #Using an arbitrary number like 0.001 so that I can filter the dataset for HH without any conversions
      else {
        filtered_data[i, unit_code_column - 1] <- 0.001
      }
    }
  }
  return(filtered_data)
}


##############################################################################################################

#1. Average and SD of Yield of Maize Per HH in KG

#Reading Conversion Food_CF_Wave4.dta
dtafileFood_CF_Wave4_Path <- file.path(getwd(), "Desktop", "data", "Food_CF_Wave4.dta")
dtafileFood_CF_Wave4 <- read_dta(dtafileFood_CF_Wave4_Path)

#Reading Conversion sect9_ph_w4.dta
dtafilesect9_ph_w4_Path <- file.path(getwd(), "Desktop", "all data", "sect9_ph_w4.dta")
dtafilesect9_ph_w4 <- read_dta(dtafilesect9_ph_w4_Path)

filtCropID = filter(dtafilesect9_ph_w4, dtafilesect9_ph_w4$crop_id == "2")
filtCropIDforNA = filter(filtCropID, filtCropID$s9q05a > 0)

converted_data_sect9 = unit_converter(filtCropIDforNA, 25) 

#Filter the dataset for HH without conversions 
converted_data_sect9_filter_for_hh_without_conversion = filter(converted_data_sect9, converted_data_sect9$s9q05a > 0.001)

mean(converted_data_sect9$s9q05a)
sd(converted_data_sect9$s9q05a)

##############################################################################################################

#2. Price/Kg Sold Maize

dtafilesect11_ph_w4_Path <- file.path(getwd(), "Desktop", "all data", "sect11_ph_w4.dta")
dtafilesect11_ph_w4 <- read_dta(dtafilesect11_ph_w4_Path) 

filtCropIDsect11 = filter(dtafilesect11_ph_w4, as.double(dtafilesect11_ph_w4$s11q01) == "2")
filtCropIDforNAsect11 = filter(filtCropIDsect11, filtCropIDsect11$s11q11a > 0)

converted_data_sect11 = unit_converter(filtCropIDforNAsect11, 35)

total_sale = sum(converted_data_sect11[ , 37]) #in local currency
total_quantity = sum(converted_data_sect11[ , 34]) #in kg

price_per_kg = total_sale / total_quantity #in local currency

##############################################################################################################

#3. Maize Average Cost of production / acre

dtafilesect10_ph_w4_Path <- file.path(getwd(), "Desktop", "all data", "sect10_ph_w4.dta")
dtafilesect10_ph_w4 <- read_dta(dtafilesect10_ph_w4_Path) 

filtCropIDsect10 = filter(dtafilesect10_ph_w4, as.double(dtafilesect10_ph_w4$s9q00b) == "2")

total_expenditures = 0

for (i in 1:nrow(filtCropIDsect10)) {
  #For Men
  number_of_men = as.double(filtCropIDsect10[i, 21])
  number_of_days_men = as.double(filtCropIDsect10[i, 22])
  total_wage_BIRR_men = as.double(filtCropIDsect10[i, 23])
  
  if (is.na(number_of_men) != TRUE) {
    if (is.na(number_of_days_men) != TRUE) {
      if (is.na(total_wage_BIRR_men) != TRUE) {
        total_expenditures = total_expenditures + number_of_men*total_wage_BIRR_men*number_of_days_men
      }
    }
  }
  #For Women
  number_of_women = as.double(filtCropIDsect10[i, 24])
  number_of_days_women = as.double(filtCropIDsect10[i, 25])
  total_wage_BIRR_women = as.double(filtCropIDsect10[i, 26])
  
  if (is.na(number_of_women) != TRUE) {
    if (is.na(number_of_days_women) != TRUE) {
      if (is.na(total_wage_BIRR_women) != TRUE) {
        total_expenditures = total_expenditures + number_of_women*total_wage_BIRR_women*number_of_days_women
      }
    }
  }
  #For Children
  
  number_of_children = as.double(filtCropIDsect10[i, 27])
  number_of_days_children = as.double(filtCropIDsect10[i, 28])
  total_wage_BIRR_children = as.double(filtCropIDsect10[i, 29])
  
  if (is.na(number_of_children) != TRUE) {
    if (is.na(number_of_days_children) != TRUE) {
      if (is.na(total_wage_BIRR_children) != TRUE) {
        total_expenditures = total_expenditures + number_of_children*total_wage_BIRR_children*number_of_days_children
      }
    }
  }
  
  if (is.na(filtCropIDsect10[i, 53]) != TRUE) {
    if (as.double(filtCropIDsect10[i, 53]) == "2") {
      if (is.na(filtCropIDsect10[i, 54]) != TRUE) {
        total_expenditures = total_expenditures + as.double(filtCropIDsect10[i, 54])
      }
    }
  }
  
  if (is.na(filtCropIDsect10[i, 55]) != TRUE) {
    total_expenditures = total_expenditures + as.double(filtCropIDsect10[i, 55])
  }
}

#Total expenditures of maize for all HH were calculated; I was unable to find 
#the total # of acres used for maize in this dataset

##############################################################################################################

#4. Avg Cultivated Land Size in Ethiopia

dtafileET_local_area_unit_conversion_Path <- file.path(getwd(), "Desktop", "data", "ET_local_area_unit_conversion.dta")
dtafileET_local_area_unit_conversion <- read_dta(dtafileET_local_area_unit_conversion_Path) 

total_timad_sqmeter = 0
total_timad_rows = 0 

total_boy_sqmeter = 0
total_boy_rows = 0 

total_senga_sqmeter = 0
total_senga_rows = 0 

total_kert_sqmeter = 0
total_kert_rows = 0 

for (i in 1:nrow(dtafileET_local_area_unit_conversion)) {
  if (as.double(dtafileET_local_area_unit_conversion[i, 6]) == "3") {
    total_timad_sqmeter = total_timad_sqmeter + as.double(dtafileET_local_area_unit_conversion[i, 7])
    total_timad_rows = total_timad_rows + 1
  }
  if (as.double(dtafileET_local_area_unit_conversion[i, 6]) == "4") {
    total_boy_sqmeter = total_boy_sqmeter + as.double(dtafileET_local_area_unit_conversion[i, 7])
    total_boy_rows = total_boy_rows + 1
  }
  if (as.double(dtafileET_local_area_unit_conversion[i, 6]) == "5") {
    total_senga_sqmeter = total_senga_sqmeter + as.double(dtafileET_local_area_unit_conversion[i, 7])
    total_senga_rows = total_senga_rows + 1
  }
  if (as.double(dtafileET_local_area_unit_conversion[i, 6]) == "6") {
    total_kert_sqmeter = total_kert_sqmeter + as.double(dtafileET_local_area_unit_conversion[i, 7])
    total_kert_rows = total_kert_rows + 1
  }
}

#Average square meter conversion of a timad/boy/senga/kert
avg_timad_sqmeter = total_timad_sqmeter / total_timad_rows
avg_boy_sqmeter = total_boy_sqmeter / total_boy_rows
avg_senga_sqmeter = total_senga_sqmeter / total_senga_rows
avg_kert_sqmeter = total_kert_sqmeter / total_kert_rows


dtafilesect10b_hh_w4_Path <- file.path(getwd(), "Desktop", "all data", "sect10b_hh_w4.dta")
dtafilesect10b_hh_w4 <- read_dta(dtafilesect10b_hh_w4_Path) 

#Parcels used for agriculture
filtCropIDsect10_b = filter(dtafilesect10b_hh_w4, as.double(dtafilesect10b_hh_w4$s10bq07) == "1")
#Doesn't contain N/A values for total area of parcels
filtCropIDsect10_b = filter(dtafilesect10b_hh_w4, is.na(dtafilesect10b_hh_w4$s10bq08a) != TRUE)

land_unit_converter <- function(filtered_data, unit_code_column) {
  total_rows_filtered_data = 0
  total_cultivated_land_area = 0
  
  for (i in 1:nrow(filtered_data)) {
    
    filtered_unit_code = as.double(filtered_data[i, unit_code_column])
    filtered_quantity = filtered_data[i,unit_code_column - 1]
    
    #0.000247105 acres = 1 square meter
    sqmeter_to_acre = 0.000247105
    
    if (filtered_unit_code == "1") {
      filtered_data[i, unit_code_column - 1] <- filtered_quantity * 2.47105
      total_rows_filtered_data = total_rows_filtered_data + 1
      total_cultivated_land_area = total_cultivated_land_area + as.double(filtered_data[i, unit_code_column - 1])
    }
    if (filtered_unit_code == "2") {
      filtered_data[i, unit_code_column - 1] <- filtered_quantity * sqmeter_to_acre
      total_rows_filtered_data = total_rows_filtered_data + 1
      total_cultivated_land_area = total_cultivated_land_area + as.double(filtered_data[i, unit_code_column - 1])
    }
    if (filtered_unit_code == "3") {
      filtered_data[i, unit_code_column - 1] <- filtered_quantity * avg_timad_sqmeter * sqmeter_to_acre
      total_rows_filtered_data = total_rows_filtered_data + 1
      total_cultivated_land_area = total_cultivated_land_area + as.double(filtered_data[i, unit_code_column - 1])
    }
    if (filtered_unit_code == "4") {
      filtered_data[i, unit_code_column - 1] <- filtered_quantity * avg_boy_sqmeter * sqmeter_to_acre
      total_rows_filtered_data = total_rows_filtered_data + 1
      total_cultivated_land_area = total_cultivated_land_area + as.double(filtered_data[i, unit_code_column - 1])
    }
    if (filtered_unit_code == "5") {
      filtered_data[i, unit_code_column - 1] <- filtered_quantity * avg_senga_sqmeter * sqmeter_to_acre
      total_rows_filtered_data = total_rows_filtered_data + 1
      total_cultivated_land_area = total_cultivated_land_area + as.double(filtered_data[i, unit_code_column - 1])
    }
    if (filtered_unit_code == "6") {
      filtered_data[i, unit_code_column - 1] <- filtered_quantity * avg_kert_sqmeter * sqmeter_to_acre
      total_rows_filtered_data = total_rows_filtered_data + 1
      total_cultivated_land_area = total_cultivated_land_area + as.double(filtered_data[i, unit_code_column - 1])
    }
    #Other unit conversions could not be found, although the number of HH using 
    #other conversions is very small
  }
  my_list <- list(total_rows_filtered_data, total_cultivated_land_area)
  return(my_list)
}

land_converted_sect10_b = land_unit_converter(filtCropIDsect10_b, 23)

average_cultivated_land_area = land_converted_sect10_b[[2]] / land_converted_sect10_b[[1]]

#Returns average cultivated land area given the sect10_b dataset in acres




