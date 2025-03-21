test_files <- use_example_data(c("SPAC_080199.xlsx", "SPAC_081599.xlsx"))
# data <- read_ccal(test_files[1])

test_that("read_ccal throws a warning when there are duplicate questionable results", {
  expect_warning(read_ccal(test_files[1]), regexp = ".*Questionable results contain duplicate rows.*")
  expect_no_warning(read_ccal(test_files[2]))
})

test_that("read_ccal output is structured correctly", {
  data <- read_ccal(test_files[2])  # Use the one that doesn't throw a duplicates warning
  expect_type(data, "list")
  expect_equal(names(data), "SPAC_081599.xlsx")
  expect_equal(names(data$SPAC_081599.xlsx), c("data", "metadata", "samples", "questionable"))

  expect_equal(names(data$SPAC_081599.xlsx$data), c("sample_name", "project_code",
                                                    "lab_number", "site_id", "remark",
                                                    "delivery_date", "comment", "parameter",
                                                    "unit", "value", "date", "repeat_measurement",
                                                    "flag_symbol"))
  expect_equal(names(data$SPAC_081599.xlsx$metadata), c("input_file_name", "investigator",
                                                        "delivery_date", "sample_location",
                                                        "sample_numbers", "project_code",
                                                        "file_number", "analyses_requested",
                                                        "misc_charges", "other_charges",
                                                        "comments", "quest_results_comments"))
  expect_equal(names(data$SPAC_081599.xlsx$samples), c("lab_number", "sample_id",
                                                       "remarks", "number_ub", "number_fb"))
  expect_equal(names(data$SPAC_081599.xlsx$questionable), c("lab_number", "param_description",
                                                            "comparison", "assessment",
                                                            "within_precision_limits",
                                                            "orig_text", "parameter"))

  expect_true(tibble::is_tibble(data$SPAC_081599.xlsx$data))
  expect_true(tibble::is_tibble(data$SPAC_081599.xlsx$metadata))
  expect_true(tibble::is_tibble(data$SPAC_081599.xlsx$samples))
  expect_true(tibble::is_tibble(data$SPAC_081599.xlsx$questionable))

})


