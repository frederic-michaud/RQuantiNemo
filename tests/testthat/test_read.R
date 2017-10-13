test_that('Loading statistique', {
  my_sim = new("simulation",
               parameters = list("generations" = 1250, "patch_capacity" = 100,stat = "{adlt.nbInd}")
  )
  run(my_sim, verbose = F)
  stat <- loadStat(my_sim)
  expect_equal(stat$adlt.nbInd[1230],100)
})


test_that('Loading statistique for replicate', {
my_sim <- new("simulation")
my_sim <- setParameter(my_sim,"stat","{adlt.nbInd}")
my_sim <- setParameter(my_sim,"patch_capacity",100)
my_sim <- setParameter(my_sim,"replicates",10)
run(my_sim,verbose = F)
stat.r <- loadStatRep(my_sim)
expect_equal(head(stat.r$adlt.nbInd[stat.r$replicate == 1],1),100)
})