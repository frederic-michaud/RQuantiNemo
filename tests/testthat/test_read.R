test_that('Loading statistique', {
  my_sim = new("simulation",
               parameters = list("generations" = 1250, "patch_capacity" = 100,stat = "{adlt.nbInd}")
  )
  run(my_sim, verbose = F)
  stat <- loadStat(my_sim)
  expect_equal(stat$adlt.nbInd[1230],100)
})

