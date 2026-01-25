source(here("src/fast_apply.R"))

test_that("fast_apply est équivalent à apply pour mes besoins de grilles 2D", {
  grid <- combn(4, 2)
  expect_equal(
    fast_apply(grid, 2, `%in%`, x = 2),
    apply(grid, 2, `%in%`, x = 2)
  )
  expect_equal(
    fast_apply(grid, 2, `%in%`, x = 10),
    apply(grid, 2, `%in%`, x = 10)
  )
})
