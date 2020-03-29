test_that("function check", {
  expect_equal(SR_search_variables(df = data.frame(CUSTOMER_ID = 1001:1010,
                                                   CUSTOMER_NAME = LETTERS[1:10],
                                                   ADRESSE_ID = 1:10),
                                   var = "id"),
               NULL)
})
