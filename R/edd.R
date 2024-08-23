#' Remove lab duplicates and other duplicate rows.
#'
#' @param data The "data" table produced by getCCALData().
#'
#' @return The data after lab QC duplicates have been removed and any duplicate rows resulting from the presence of multiple flags for one measurement have been removed.
#' @export
#'
#' @examples
#' \dontrun{
#' getCCALData(file_path)[[1]][[1]] %>%
#'   handle_duplicates()
#' }
handle_duplicates <- function(data) {

  # Drop lab duplicates
  data <- data %>%
    dplyr::filter(is.na(repeat_measurement))

  # Concatenate duplicated rows due to different qa flags from CCAL
  data <- data %>%
    dplyr::group_by(dplyr::across(-c(qa_description, qa_within_precision_limits))) %>%
    dplyr::summarise(qa_description = paste(qa_description, collapse = "... ")) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(qa_description = dplyr::if_else(qa_description == "NA", NA, qa_description)) # fix NA handling issue

  return(data)
}

#' Assign quality control flags.
#'
#' @param data The "data" table produced by getCCALData() after duplicates have been removed with handle_duplicates().
#'
#' @return The input data with the addition of a new flag field.
#' Flags included in this automated routine are NFNSU, KRMDL, J-R, and NFNSI (in that order of priority).
#' Any other flags will need to be defined separately by the user.
#' @export
#'
#' @examples
#' \dontrun{
#' getCCALData(file_path)[[1]][[1]] %>%
#'   handle_duplicates() %>%
#'   assign_flags()
#' }
assign_flags <- function(data) {
  # Define constants
  data$flag <- NA

  utp_mdl <- limits %>%
    dplyr::filter(ccal_code == "UTP") %>%
    dplyr::select(method_detection_limit)

  tdp_mdl <- limits %>%
    dplyr::filter(ccal_code == "TDP") %>%
    dplyr::select(method_detection_limit)

  utn_mdl <- limits %>%
    dplyr::filter(ccal_code == "UTN") %>%
    dplyr::select(method_detection_limit)

  tdn_mdl <- limits %>%
    dplyr::filter(ccal_code == "TDN") %>%
    dplyr::select(method_detection_limit)

  po4p_mdl <- limits %>%
    dplyr::filter(ccal_code == "PO4-P") %>%
    dplyr::select(method_detection_limit)

  nh3_n_mdl <- limits %>%
    dplyr::filter(ccal_code == "NH3-N") %>%
    dplyr::select(method_detection_limit)

  no3_n_no2_n_mdl <- limits %>%
    dplyr::filter(ccal_code == "NO3-N+NO2-N") %>%
    dplyr::select(method_detection_limit)

  no3_mdl <- limits %>%
    dplyr::filter(ccal_code == "NO3") %>%
    dplyr::select(method_detection_limit)

  # Loop over every row of data to define NFNSU and NFNSI flags
  for(i in 1:nrow(data)) {
    # Assign NFNSU and NFNSI flags for orthophosphorus
    if (data[i,]$parameter == "PO4-P") {
      utp <- data %>%
        dplyr::filter(lab_number == data[i,]$lab_number, parameter == "UTP") %>%
        dplyr::select(value)

      tdp <- data %>%
        dplyr::filter(lab_number == data[i,]$lab_number, parameter == "TDP") %>%
        dplyr::select(value)

      if ( ((data[i,]$value - utp) %>>% (utp_mdl + po4p_mdl)) | ((data[i,]$value - tdp) %>>% (tdp_mdl + po4p_mdl)) )  { #
        data[i,]$flag = "NFNSU"
      }
      else if ( ((data[i,]$value - utp) %>>% 0) | ((data[i,]$value - tdp) %>>% 0) ) {
        data[i,]$flag = "NFNSI"
      }
    }
    # Assign NFNSU and NFNSI flags for total dissolved phosphorus
    else if (data[i,]$parameter == "TDP") {
      utp <- data %>%
        dplyr::filter(lab_number == data[i,]$lab_number, parameter == "UTP") %>%
        dplyr::select(value)

      if ( (data[i,]$value - utp) %>>% (utp_mdl + tdp_mdl) )  { #
        data[i,]$flag = "NFNSU"
      }
      else if ( (data[i,]$value - utp) %>>% 0) {
        data[i,]$flag = "NFNSI"
      }
    }
    # Assign NFNSU and NFNSI flags for total dissolved nitrogen
    else if (data[i,]$parameter == "TDN") {
      utn <- data %>%
        dplyr::filter(lab_number == data[i,]$lab_number, parameter == "UTN") %>%
        dplyr::select(value)

      if ( (data[i,]$value - utn) %>>% (utn_mdl + tdn_mdl) )  { #
        data[i,]$flag = "NFNSU"
      }
      else if ( (data[i,]$value - utn) %>>% 0) {
        data[i,]$flag = "NFNSI"
      }
    }
    # COMMENTED THIS OUT BECAUSE I'M NOT SURE WHETHER TO INCLUDE THESE.
    # RESULTS ARE MORE SIMILAR TO HISTORICAL EDDs WHEN THIS IS COMMENTED OUT.
    # Assign NFNSU and NFNSI flags for NH3-N, "NO3-N+NO2-N", "NO3"
    # else if (data[i,]$parameter %in% c("NH3-N", "NO3-N+NO2-N", "NO3")) {
    #   utn <- data %>%
    #     filter(lab_number == data[i,]$lab_number, parameter == "UTN") %>%
    #     select(value)
    #
    #   tdn <- data %>%
    #     filter(lab_number == data[i,]$lab_number, parameter == "TDN") %>%
    #     select(value)
    #
    #   local_mdl <- NA
    #
    #   if (data[i,]$parameter == "NH3-N") {
    #     local_mdl <- nh3_n_mdl
    #   }
    #   else if (data[i,]$parameter == "NO3-N+NO2-N") {
    #     local_mdl <- no3_n_no2_n_mdl
    #   }
    #   else if (data[i,]$parameter == "NO3") {
    #     local_mdl <- no3_mdl
    #   }
    #
    #   if ( ((data[i,]$value - utn) %>>% (utn_mdl + local_mdl)) | ((data[i,]$value - tdn) %>>% (tdn_mdl + local_mdl)) )  {
    #     data[i,]$flag = "NFNSU"
    #   }
    #   else if ( ((data[i,]$value - utn) %>>% 0) | ((data[i,]$value - tdn) %>>% 0) ) {
    #     data[i,]$flag = "NFNSI"
    #   }
    # }
  }

  # Assign QC flags according to Kirk's instructions in SOP
  # Case_when evaluates sequentially so flags included in order of priority
  data <- data %>%
    dplyr::left_join(limits %>% dplyr::select(ccal_code, method_detection_limit, min_level_quantification), # join MDL and ML to data
              by = c("parameter" = "ccal_code")) %>%
    dplyr::mutate(flag = dplyr::case_when( # add flags according to priority in SOP
      flag == "NFNSU" ~ "NFNSU",
      value %<=% method_detection_limit ~ "KRMDL",
      value %<=% min_level_quantification ~ "J-R",
      TRUE ~ flag
    )) %>%
    select(-method_detection_limit, -min_level_quantification) # remove so that there are no unexpected side effects. User expects the addition of just the flag field.

  return(data)
}


#' Create the results table of the EQuIS EDD.
#'
#' @param file_path Path to .xlsx file delivered by CCAL.
#'
#' @return The data formatted for input into the EQuIS system as the "Results" table. Note that Activity_ID is left blank and users must create it themselves.
#' @export
#'
#' @examples
#' \dontrun{
#' results <- format_results(file_path)
#'}
format_results <- function(file_paths){

  edd_results <- getCCALData(file_paths) %>%
    lapply(function(x) {
    x[[1]] %>% # process CCAL data with function from original package
      handle_duplicates() %>% # remove duplicates
      assign_flags() %>% # assign flags
      dplyr::left_join(limits %>% dplyr::select(-c(analysis)), # join data about detection limits
                       by = c("parameter" = "ccal_code")) %>%
      dplyr::rename("#Org_Code" = "project_code") %>%
      dplyr::mutate(Activity_ID = NA,
             Result_Detection_Condition = dplyr::case_when(flag == "NFNSU" ~ "Detected Not Quantified",
                                                    flag == "KRMDL" ~ "Not Detected",
                                                    flag == "J-R" ~ "Detected And Quantified",
                                                    TRUE ~ "Detected And Quantified"),
             Result_Type = if_else(flag == "J-R", "Estimated", "Actual", missing = "Actual"),
             Result_Text = dplyr::if_else(Result_Detection_Condition == "Detected And Quantified", value, NA),
             Reportable_Result = dplyr::if_else(flag %in% c("NFNSU", "KRMDL"), "N", "Y")) %>%
      dplyr::left_join(qualifiers, by = c("flag" = "lookup_code")) %>%
      dplyr::rename("Result_Qualifier" = "flag",
                    "Result_Comment" = "remark.y") %>%
      dplyr::mutate(Result_Status = "Pre-Cert") %>%
      dplyr::rename("Method_Detection_Limit" = "method_detection_limit",
                    "Lower_Quantification_Limit" = "min_level_quantification") %>%
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
             Source_Flags = paste(comment, flag_symbol, qa_description, sep = "... "),
             Source_Flags = str_replace_all(Source_Flags, "NA... ", ""),
             Source_Flags = str_replace_all(Source_Flags, "... NA", ""),
             Source_Flags = if_else(Source_Flags == "NA", NA, Source_Flags),
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

  # add in names() basename() thing

  return(edd_results)
}

#' Write the results table of the EQuIS EDD.
#'
#' @param files Path to .xlsx file delivered by CCAL. Use a character vector to specify multiple files.
#' @inheritParams openxlsx::write.xlsx
#' @param format File format to export machine readable data to - either "xlsx" or "csv"
#' @param destination_folder Folder to save the data in. Defaults to current working directory. Folder must already exist.
#'
#' @return Invisibly returns a list containing the data that were written to file.
#' @export
#'
#' @examples
#' \dontrun{
#' ccal_folder <- "ccal_results"
#' dest_folder <- "ccal_results/edd"
#' file_list <- list.files(ccal_folder, pattern = "*.xlsx$", full.names = TRUE)
#' write_results(file_list, format = "xlsx", destination_folder = dest_folder)
#' write_results(file_list, format = "csv", destination_folder = dest_folder)
#' }
write_results <- function(files, format = c("xlsx", "csv"), destination_folder = "./", overwrite = FALSE) {
  format <- match.arg(format)
  destination_folder <- normalizePath(destination_folder, winslash = .Platform$file.sep)

  all_data <- format_results(files)  # Read in data

  write_data(all_data, format, destination_folder, overwrite, suffix = "_edd_results", num_tables = 1)

  return(invisible(all_data))
}







