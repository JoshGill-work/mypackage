test_that("return_financial_year works for standard dates", {
  expect_equal(return_financial_year("2025-10-29"), 2025)  # After April, same year
  expect_equal(return_financial_year("2026-02-04"), 2025)  # Before April, previous year
})

test_that("return_financial_year works for boundary dates", {
  expect_equal(return_financial_year("2025-04-01"), 2025)  # First day of FY
  expect_equal(return_financial_year("2025-03-31"), 2024)  # Last day of previous FY
})

test_that("return_financial_year handles Date objects", {
  expect_equal(return_financial_year(as.Date("2025-10-29")), 2025)
  expect_equal(return_financial_year(as.Date("2026-02-04")), 2025)
})

test_that("return_financial_year works for January to March", {
  expect_equal(return_financial_year("2026-01-01"), 2025)
  expect_equal(return_financial_year("2026-03-31"), 2025)
})

test_that("return_financial_year works for April to December", {
  expect_equal(return_financial_year("2026-04-01"), 2026)
  expect_equal(return_financial_year("2026-12-31"), 2026)
})

test_that("return_financial_year handles leap years", {
  expect_equal(return_financial_year("2024-02-29"), 2023)  # Leap day in Feb
  expect_equal(return_financial_year("2024-04-01"), 2024)  # After leap day
})
