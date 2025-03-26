test_that("Lab duplicates are removed with remove_ccal_duplicates()", {
  # Create data without duplicates
  data <- suppressMessages(read_ccal(use_example_data("SPAC_081599.xlsx")))[[1]]$data |>
    remove_ccal_duplicates()

  # Check that the number of rows is as expected
  expect_equal(nrow(data), 420)

  # Check that every entry in repeat_measurement is NA
  expect_equal(data |>
                 dplyr::filter(!is.na(repeat_measurement)) |>
                 nrow(),
               0)
})

test_that("J-R flags are assigned correctly with assign_detection_flags()", {
  # Edit limits table
  detection_limits <- detection_limits |>
    dplyr::mutate(EndDate = dplyr::if_else(EndDate == "2024-12-31", lubridate::ymd("2099-12-31"), EndDate))

  # Create data without duplicates and with detection flags
  data <- suppressMessages(read_ccal(use_example_data("SPAC_081599.xlsx")))[[1]]$data |>
    remove_ccal_duplicates() |>
    assign_detection_flags(detection_limits)

  # Determine the row numbers of J-R flags
  flag_indices <- data |>
    dplyr::mutate(row_num = dplyr::row_number()) |>
    dplyr::filter(flag == "J-R") |>
    dplyr::pull(row_num)

  # Check that J-R flags occur where expected
  expect_equal(flag_indices,
               c(4, 20, 21, 27, 32, 46, 60, 62, 70, 75, 76, 88, 90, 92, 94,
                 102, 104, 107, 116, 118, 132, 134, 144, 146, 149, 150, 160,
                 164, 172, 174, 178, 186, 188, 200, 202, 206, 214, 216, 218,
                 230, 234, 242, 244, 245, 256, 258, 270, 272, 286, 288, 289,
                 300, 314, 317, 318, 328, 331, 332, 336, 342, 354, 356, 357,
                 382, 396, 410, 411, 412))
})

test_that("Results table is formatted correctly by format_equis_results()", {
  # Edit limits table
  detection_limits <- detection_limits |>
    dplyr::mutate(EndDate = dplyr::if_else(EndDate == "2024-12-31", lubridate::ymd("2099-12-31"), EndDate))

  # Create results table
  results <- suppressMessages(format_equis_results(file_paths = use_example_data("SPAC_081599.xlsx"),
                                                   limits = detection_limits))

  # Check that every row has a Characteristic_Name, Filtered_Fraction, Result_Detection_Condition, and Lab_Reported_Result
  expect_equal(results[[1]] |>
                 dplyr::filter(is.na(Characteristic_Name) | is.na(Filtered_Fraction) |
                                 is.na(Result_Detection_Condition) | is.na(Lab_Reported_Result)) |>
                 nrow(),
               0)

  # Check that all columns are included
  expect_equal(colnames(results[[1]]),
               c("#Org_Code", "Activity_ID", "Characteristic_Name", "Method_Speciation",
               "Filtered_Fraction", "Result_Detection_Condition", "Result_Text",
               "Result_Unit", "Result_Qualifier", "Result_Status", "Result_Type", "Result_Comment",
               "Method_Detection_Limit", "Lower_Quantification_Limit", "Upper_Quantification_Limit", "Limit_Comment",
               "Temperature_Basis", "Statistical_Basis", "Time_Basis", "Weight_Basis", "Particle_Size_Basis",
               "Precision", "Bias", "Confidence_Interval", "Upper_Confidence_Limit",
               "Lower_Confidence_Limit", "Result_Sampling_Point_Name", "Result_Depth_Height_Measure",
               "Result_Depth_Height_Measure_Unit", "Result_Depth_Altitude_Reference_Point",
               "Analytical_Method_ID", "Analytical_Remark", "Lab_ID", "Lab_Remark_Code", "Analysis_Start_Date",
               "Analysis_Start_Time", "Analysis_Start_Time_Zone", "Lab_Accreditation_Indicator",
               "Lab_Accreditation_Authority_Name", "Lab_Batch_ID", "Lab_Sample_Preparation_ID",
               "Lab_Sample_Preparation_Start_Date", "Lab_Sample_Preparation_Start_Time",
               "Lab_Sample_Preparation_Start_Time_Zone", "Dilution_Factor", "Num_of_Replicates",
               "Data_Logger_Line_Name", "Biological_Intent", "Biological_Individual_ID", "Subject_Taxon",
               "Unidentified_Species_ID", "Tissue_Anatomy", "Group_Summary_Count_or_Weight",
               "Group_Summary_Count_or_Weight_Unit", "Cell_Form", "Cell_Shape", "Habit_Name_1",
               "Habit_Name_2", "Habit_Name_3", "Voltinism", "Pollution_Tolerance", "Pollution_Tolerance_Scale",
               "Trophic_Level", "Functional_Feeding_Group_1", "Functional_Feeding_Group_2",
               "Functional_Feeding_Group_3", "File_Name_or_Resource_ID", "Resource_Date",
               "Resource_Title_Name", "Resource_Creator_Name", "Resource_Publisher_Name",
               "Resource_Publication_Year", "Resource_Volume_Pages", "Resource_Subject_Text",
               "Frequency_Class_Descriptor_1", "Frequency_Class_Bounds_Unit_1",
               "Frequency_Class_Lower_Bound_1", "Frequency_Class_Upper_Bound_1",
               "Frequency_Class_Descriptor_2", "Frequency_Class_Bounds_Unit_2",
               "Frequency_Class_Lower_Bound_2", "Frequency_Class_Upper_Bound_2",
               "Frequency_Class_Descriptor_3", "Frequency_Class_Bounds_Unit_3",
               "Frequency_Class_Lower_Bound_3", "Frequency_Class_Upper_Bound_3",
               "Taxonomist_Accreditation_Indicator", "Taxonomist_Accreditation_Authority_Name",
               "Result_File_Name", "Lab_Reported_Result", "Reportable_Result", "Source_Flags",
               "Logger_Standard_Difference", "Logger_Percent_Error", "Analytical_Method_ID_Context",
               "Predicted_Result"))
})
