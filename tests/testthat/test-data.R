test_that("provided data sets plot correctly", {
  a <- plot_map(data = statepop, values = "pop_2023", include = "WB")
  b <- plot_map(data = wb_2011, values = "pop_2011")

  vdiffr::expect_doppelganger("statepop", a)
  vdiffr::expect_doppelganger("wb_2011", b)
})
