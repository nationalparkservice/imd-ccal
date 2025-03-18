test_files <- use_example_data(c("SPAC_080199.xlsx", "SPAC_081599.xlsx"))
data <- read_ccal(test_files[1])

test_that("read_ccal output is structured correctly", {

  expect_type(data, "list")
  expect_equal(names(data), "SPAC_080199.xlsx")
  expect_equal(names(data$SPAC_080199.xlsx), c("data", "metadata", "samples", "questionable"))

  expect_equal(names(data$SPAC_080199.xlsx$data), c("sample_name", "project_code",
                                                    "lab_number", "site_id", "remark",
                                                    "delivery_date", "comment", "parameter",
                                                    "unit", "value", "date", "repeat_measurement",
                                                    "flag_symbol", "qa_within_precision_limits",
                                                    "qa_description"))
  expect_equal(names(data$SPAC_080199.xlsx$metadata), c("input_file_name", "investigator",
                                                        "delivery_date", "sample_location",
                                                        "sample_numbers", "project_code",
                                                        "file_number", "analyses_requested",
                                                        "misc_charges", "other_charges",
                                                        "comments", "quest_results_comments"))
  expect_equal(names(data$SPAC_080199.xlsx$samples), c("lab_number", "sample_id",
                                                       "remarks", "number_ub", "number_fb"))
  expect_equal(names(data$SPAC_080199.xlsx$questionable), c("lab_number", "param_description",
                                                            "comparison", "assessment",
                                                            "within_precision_limits",
                                                            "orig_text", "parameter"))

  expect_true(tibble::is_tibble(data$SPAC_080199.xlsx$data))
  expect_true(tibble::is_tibble(data$SPAC_080199.xlsx$metadata))
  expect_true(tibble::is_tibble(data$SPAC_080199.xlsx$samples))
  expect_true(tibble::is_tibble(data$SPAC_080199.xlsx$questionable))

})


