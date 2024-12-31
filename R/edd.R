#' Remove lab duplicates and other duplicate rows.
#'
#' @param data The "data" table produced by getCCALData().
#'
#' @return The data after lab QC duplicates have been removed and any duplicate rows resulting from the presence of multiple flags for one measurement have been removed.
#' @export
#'
#' @examples
#' tidy_ccal_no_dups <- getCCALData(system.file("extdata", "SPAC_080199.xlsx", package = "imdccal"))[[1]][[1]] %>%
#'   handle_duplicates()
handle_duplicates <- function(data) {

  # Drop lab duplicates
  data <- data %>%
    dplyr::filter(is.na(repeat_measurement))

  # Concatenate duplicated rows due to different qa flags from CCAL
  data <- data %>%
    dplyr::group_by(dplyr::across(-c(qa_description, qa_within_precision_limits))) %>%
    dplyr::summarise(qa_description = paste(qa_description, collapse = "... ")) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(qa_description = dplyr::if_else(qa_description == "NA", NA, qa_description)) # fix NA issue

  return(data)
}

#' Assign quality control flags to values less than or equal to the minimum level of quantification but greater than the MDL.
#'
#' @param data The "data" table produced by getCCALData() after duplicates have been removed with handle_duplicates().
#' @param limits Table with detection limits. By default, uses the version in the package. User-defined versions must have the same columns.
#'
#' @return The input data with the addition of a new flag field.
#' The only flag included in this routine is the J-R flag.
#' Any other flags will need to be defined separately by the user.
#' @export
#'
#' @examples
#' tidy_ccal_flagged <- getCCALData(system.file("extdata", "SPAC_080199.xlsx", package = "imdccal"))[[1]][[1]] %>%
#'   handle_duplicates() %>%
#'   assign_detection_flags()
assign_detection_flags <- function(data, limits = imdccal::limits) {
  data <- data %>%
    dplyr::left_join(limits %>% dplyr::select(analyte_code, Method_Detection_Limit, Lower_Quantification_Limit, StartDate, EndDate), # join MDL and ML to data
                     by = dplyr::join_by(parameter == analyte_code,
                                         date >= StartDate,
                                         date <= EndDate)) %>%
    dplyr::mutate(flag = dplyr::case_when(
      value %<=% Method_Detection_Limit ~ NA,
      value %<=% Lower_Quantification_Limit ~ "J-R",
      TRUE ~ NA
    )) %>%
    dplyr::select(-c(Method_Detection_Limit, Lower_Quantification_Limit, StartDate, EndDate)) # remove so that there are no unexpected side effects. User expects the addition of just the flag field.

  return(data)
}

#' Create the results table of the EQuIS EDD.
#'
#' @param file_paths Path to .xlsx file delivered by CCAL.
#' @param limits Table with detection limits. By default, uses the version in the package. User-defined versions must have the same columns.
#' @param qualifiers Table with flags and their meanings. By default, uses the version in the package. User-defined versions must have the same columns.
#' @param concat If concat is set to TRUE, the output contains one table rather than one for every input file.
#' By default, concat is set to FALSE, so the output contains separate tables for each file.
#' If only one file path is supplied to the file_paths argument, this parameter does not affect the output.
#'
#' @return The data formatted for input into the EQuIS system as the "Results" table.
#' Note that after running this function, the user must still complete some of their own processing to prepare their data for EQuIS.
#' For example, users must define Activity_ID, assign any additional flags, and modify Result_Status depending on the outcome of their quality control process.
#' @export
#'
#' @examples
#' # Edit limits table to work with example data
#' limits <- imdccal::limits |>
#'   dplyr::mutate(EndDate = dplyr::if_else(EndDate == "2024-12-31", lubridate::ymd("2099-12-31"), EndDate))
#'
#' # Create results table
#' results <- format_results(file_paths = system.file("extdata", "SPAC_080199.xlsx", package = "imdccal"),
#'                           limits = limits)
format_results <- function(file_paths, limits = imdccal::limits,
                           qualifiers = imdccal::qualifiers, concat = FALSE){

  edd_results <- getCCALData(file_paths, concat) %>%
    lapply(function(x) {
    x[[1]] %>% # process CCAL data with function from original package
      handle_duplicates() %>% # remove duplicates
      assign_detection_flags(limits) %>% # assign J-R flag
      dplyr::left_join(limits %>% dplyr::select(-analysis), # join data about detection limits
                         by = dplyr::join_by(parameter == analyte_code,
                                             date >= StartDate,
                                             date <= EndDate)) %>%
      dplyr::rename("#Org_Code" = "project_code") %>%
      dplyr::mutate(Activity_ID = NA,
             Result_Detection_Condition = dplyr::if_else(value %<=% Method_Detection_Limit,
                                                         "Not Detected", "Detected And Quantified"),
             Result_Type = dplyr::if_else(flag == "J-R", "Estimated", "Actual", missing = "Actual"),
             Result_Text = dplyr::if_else(Result_Detection_Condition == "Detected And Quantified", value, NA),
             Reportable_Result = NA) %>%
      dplyr::left_join(qualifiers %>% dplyr::rename("Result_Comment" = "remark"),
                       by = c("flag" = "lookup_code")) %>%
      dplyr::rename("Result_Qualifier" = "flag") %>%
      dplyr::mutate(Result_Status = "Pre-Cert") %>%
      dplyr::mutate(Upper_Quantification_Limit = NA,
             Limit_Comment = "Detection Limit Type = MDL",
             Temperature_Basis = NA,
             Statistical_Basis = NA,
             Time_Basis = NA,
             Weight_Basis = NA,
             Particle_Size_Basis = NA,
             Precision = NA,
             Bias = NA,
             Confidence_Interval = NA,
             Upper_Confidence_Limit = NA,
             Lower_Confidence_Limit = NA,
             Result_Sampling_Point_Name = NA,
             Result_Depth_Height_Measure = NA,
             Result_Depth_Height_Measure_Unit = NA,
             Result_Depth_Altitude_Reference_Point = NA,
             Analytical_Remark = NA,
             Lab_ID = "CCAL_LAB",
             Lab_Remark_Code = NA) %>%
      dplyr::rename("Analysis_Start_Date" = "date") %>%
      dplyr::mutate(Analysis_Start_Date = as.Date(Analysis_Start_Date),
             Analysis_Start_Time = NA,
             Analysis_Start_Time_Zone = NA,
             Lab_Accreditation_Indicator = NA,
             Lab_Accreditation_Authority_Name = NA) %>%
      dplyr::rename("Lab_Batch_ID" = "lab_number") %>%
      dplyr::mutate(Lab_Sample_Preparation_ID =  NA,
             Lab_Sample_Preparation_Start_Date = NA,
             Lab_Sample_Preparation_Start_Time = NA,
             Lab_Sample_Preparation_Start_Time_Zone = NA,
             Dilution_Factor = NA,
             Num_of_Replicates = NA,
             Data_Logger_Line_Name = NA,
             Biological_Intent = NA,
             Biological_Individual_ID = NA,
             Subject_Taxon = NA,
             Unidentified_Species_ID = NA,
             Tissue_Anatomy = NA,
             Group_Summary_Count_or_Weight = NA,
             Group_Summary_Count_or_Weight_Unit = NA,
             Cell_Form = NA,
             Cell_Shape = NA,
             Habit_Name_1 = NA,
             Habit_Name_2 = NA,
             Habit_Name_3 = NA,
             Voltinism = NA,
             Pollution_Tolerance = NA,
             Pollution_Tolerance_Scale = NA,
             Trophic_Level = NA,
             Functional_Feeding_Group_1 = NA,
             Functional_Feeding_Group_2 = NA,
             Functional_Feeding_Group_3 = NA,
             File_Name_or_Resource_ID = NA,
             Resource_Date = NA,
             Resource_Title_Name = NA,
             Resource_Creator_Name = NA,
             Resource_Publisher_Name = NA,
             Resource_Publication_Year = NA,
             Resource_Volume_Pages = NA,
             Resource_Subject_Text = NA,
             Frequency_Class_Descriptor_1 = NA,
             Frequency_Class_Bounds_Unit_1 = NA,
             Frequency_Class_Lower_Bound_1 = NA,
             Frequency_Class_Upper_Bound_1 = NA,
             Frequency_Class_Descriptor_2 = NA,
             Frequency_Class_Bounds_Unit_2 = NA,
             Frequency_Class_Lower_Bound_2 = NA,
             Frequency_Class_Upper_Bound_2 = NA,
             Frequency_Class_Descriptor_3 = NA,
             Frequency_Class_Bounds_Unit_3 = NA,
             Frequency_Class_Lower_Bound_3 = NA,
             Frequency_Class_Upper_Bound_3 = NA,
             Taxonomist_Accreditation_Indicator = NA,
             Taxonomist_Accreditation_Authority_Name = NA,
             Result_File_Name = NA,
             Lab_Reported_Result = value,
             Source_Flags = {if("comment" %in% names(.)) comment else NA},
             # Source_Flags = paste(comment, flag_symbol, qa_description, sep = "... "),
             # Source_Flags = str_replace_all(Source_Flags, "NA... ", ""),
             # Source_Flags = str_replace_all(Source_Flags, "... NA", ""),
             # Source_Flags = if_else(Source_Flags == "NA", NA, Source_Flags),
             Logger_Standard_Difference = NA,
             Logger_Percent_Error = NA,
             Analytical_Method_ID_Context = NA,
             Predicted_Result = NA) %>%
      dplyr::select(`#Org_Code`, Activity_ID, Characteristic_Name, Method_Speciation,  # ORDER TO MATCH EDD
             Filtered_Fraction, Result_Detection_Condition, Result_Text,
             Result_Unit, Result_Qualifier, Result_Status, Result_Type, Result_Comment,
             Method_Detection_Limit, Lower_Quantification_Limit, Upper_Quantification_Limit, Limit_Comment,
             Temperature_Basis, Statistical_Basis, Time_Basis, Weight_Basis, Particle_Size_Basis,
             Precision, Bias, Confidence_Interval, Upper_Confidence_Limit,
             Lower_Confidence_Limit, Result_Sampling_Point_Name, Result_Depth_Height_Measure,
             Result_Depth_Height_Measure_Unit, Result_Depth_Altitude_Reference_Point,
             Analytical_Method_ID, Analytical_Remark, Lab_ID, Lab_Remark_Code, Analysis_Start_Date,
             Analysis_Start_Time, Analysis_Start_Time_Zone, Lab_Accreditation_Indicator,
             Lab_Accreditation_Authority_Name, Lab_Batch_ID, Lab_Sample_Preparation_ID,
             Lab_Sample_Preparation_Start_Date, Lab_Sample_Preparation_Start_Time,
             Lab_Sample_Preparation_Start_Time_Zone, Dilution_Factor, Num_of_Replicates,
             Data_Logger_Line_Name, Biological_Intent, Biological_Individual_ID, Subject_Taxon,
             Unidentified_Species_ID, Tissue_Anatomy, Group_Summary_Count_or_Weight,
             Group_Summary_Count_or_Weight_Unit, Cell_Form, Cell_Shape, Habit_Name_1,
             Habit_Name_2, Habit_Name_3, Voltinism, Pollution_Tolerance, Pollution_Tolerance_Scale,
             Trophic_Level, Functional_Feeding_Group_1, Functional_Feeding_Group_2,
             Functional_Feeding_Group_3, File_Name_or_Resource_ID, Resource_Date,
             Resource_Title_Name, Resource_Creator_Name, Resource_Publisher_Name,
             Resource_Publication_Year, Resource_Volume_Pages, Resource_Subject_Text,
             Frequency_Class_Descriptor_1, Frequency_Class_Bounds_Unit_1,
             Frequency_Class_Lower_Bound_1, Frequency_Class_Upper_Bound_1,
             Frequency_Class_Descriptor_2, Frequency_Class_Bounds_Unit_2,
             Frequency_Class_Lower_Bound_2, Frequency_Class_Upper_Bound_2,
             Frequency_Class_Descriptor_3, Frequency_Class_Bounds_Unit_3,
             Frequency_Class_Lower_Bound_3, Frequency_Class_Upper_Bound_3,
             Taxonomist_Accreditation_Indicator, Taxonomist_Accreditation_Authority_Name,
             Result_File_Name, Lab_Reported_Result, Reportable_Result, Source_Flags,
             Logger_Standard_Difference, Logger_Percent_Error, Analytical_Method_ID_Context,
             Predicted_Result)
    })

  return(edd_results)
}

#' Write the results table of the EQuIS EDD.
#'
#' @param files Path to .xlsx file delivered by CCAL. Use a character vector to specify multiple files.
#' @param limits Table with detection limits. By default, uses the version in the package. User-defined versions must have the same columns.
#' @param qualifiers Table with flags and their meanings. By default, uses the version in the package. User-defined versions must have the same columns.
#' @inheritParams openxlsx::write.xlsx
#' @param format File format to export machine readable data to - either "xlsx" or "csv"
#' @param destination_folder Folder to save the data in. Defaults to current working directory. Folder must already exist.
#' @param concat If concat is set to TRUE, the function creates one file rather than one for every CCAL deliverable.
#' By default, concat is set to FALSE, so the output contains separate files for each CCAL deliverable.
#' If only one file path is supplied to the files argument, this parameter does not affect the output.
#'
#' @return Invisibly returns a list containing the data that were written to file.
#' @export
#'
#' @examples
#' \dontrun{
#' # Edit limits table to work with example data
#' limits <- imdccal::limits |>
#'   dplyr::mutate(EndDate = dplyr::if_else(EndDate == "2024-12-31", lubridate::ymd("2099-12-31"), EndDate))
#'
#' # Get file paths
#' all_files <- list.files(system.file("extdata", package = "imdccal"),
#'                         pattern = "*.xlsx$", full.names = TRUE)
#'
#' # Write to xlsx
#' write_results(files = all_files,
#'               limits = limits,
#'               destination_folder = "ccal_tidy",
#'               overwrite = TRUE)  # Write one file of tidied data per input file
#'
#' # Write to csv
#' write_results(files = all_files,
#'               limits = limits,
#'               format = "csv",
#'               destination_folder = "ccal_tidy",
#'               overwrite = TRUE)  # Write one folder of tidied CSV data per input file
#' }
write_results <- function(files, limits = imdccal::limits, qualifiers = imdccal::qualifiers, format = c("xlsx", "csv"),
                          destination_folder = "./", overwrite = FALSE, concat = FALSE) {
  format <- match.arg(format)
  destination_folder <- normalizePath(destination_folder, winslash = .Platform$file.sep)

  all_data <- format_results(files, limits, qualifiers, concat)  # Read in data

  write_data(all_data, format, destination_folder, overwrite, suffix = "_edd_results", num_tables = 1)

  return(invisible(all_data))
}
