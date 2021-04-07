test_that("multiplication works", {
  expect_equal(2 * 2, 4)
})

test_that('get_mpc()  function  recreates mpc_health_outlays()', {
  vec <- runif(9, 0, 10)
  expect_equal(mpc_ui(vec), mpc_ui_alt(vec) )
})



test_that('get_mpc()  function  recreates mpc_health_outlays()', {
  vec <- runif(9, 0, 10)
  expect_equal(mpc_health_outlays(vec), mpc_health_outlays_alt(vec) )
})

test_that('mpc subsidies works',
          {
          
            expect_equal(
              fim %>% mpc_subsidies_covid(),
              fim %>% calculate_mpc('subsidies')
            )
          })


