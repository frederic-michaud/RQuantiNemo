context("read")

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


test_that('Loading statistique for patch', {
parameters = list("generations" = 5,
                  "patch_capacity" = 10,
                  "patch_number" = 11,
                  "stat" = "{adlt.nbInd_p}",
                  "patch_ini_size" = "{1 2 3 4 5 6 7 8 9 10}")
  my_sim = new("simulation", parameters = parameters)
  run(my_sim, verbose =FALSE)
  stat.patch <- loadStatPatch(my_sim, "adlt.nbInd_p")
  expect_equal(stat.patch[1, 1],1)
  expect_equal(stat.patch[1, 5],5)
})


test_that('Loading Phenotype', {
  parameters = list("generations" = 5,
                    "patch_capacity" = 10,
                    "quanti_loci" = 3,
                    "quanti_nb_trait" = 3,
                    "quanti_save_phenotype" = 1)
  my_sim = new("simulation", parameters = parameters)
  run(my_sim, verbose = F)
  pheno <- loadPheno(my_sim)
  expect_equal(pheno[1, 1], 1)
})

test_that('Loading Genotype', {
  parameters = list("generations" = 5,
                    "patch_capacity" = 10,
                    "quanti_loci" = 3,
                    "quanti_all" = 3,
                    "quanti_nb_trait" = 3,
                    "quanti_save_genotype" = 1)
  my_sim = new("simulation", parameters = parameters)
  run(my_sim, verbose = F)
  geno <- loadGeno(my_sim)
  expect_equal(geno[1, 1], "1")
  expect_true(geno[1, 2] %in% c("11","12","13","21","22","23","31","32","33"))

})

