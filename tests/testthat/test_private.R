
test_that('Writting the command to launch quantiNemo', {
  print(getwd())
  my_sim = new("simulation",sim.dir = "/home/fred/",sim.name = "new_test" ,exe.dir = "/bin" ,exe.name ="quanti")
  expect_equal(getCommand(my_sim),c("/bin/quanti"," /home/fred/new_test.ini"))
})

test_that('Writting files', {
  my_sim = new("simulation",
               parameters = list("generations" = 100, "patch_capacity" = 100),
               sim.dir = "/Users/frederic/Desktop/test_package/"
               
               )
  writeInput(my_sim)
  expect_true(file.exists(file.path("/Users/frederic/Desktop/test_package/my_simulation.ini")))
})

test_that('Running a minimal simulation', {
  my_sim = new("simulation",
               parameters = list("generations" = 100, "patch_capacity" = 100),
               sim.dir = "/Users/frederic/Desktop/test_package/",
               exe.dir = "/Users/frederic/Desktop/test_package/",
               exe.name = "quantiNemo2"
  )
  system2("touch","/Users/frederic/Desktop/test_package/quantiNemo2.log")
  system2("rm", " /Users/frederic/Desktop/test_package/quantiNemo2.log")
  res <- run(my_sim)
  expect_equal(0,res)
})

test_that('Getting the last part of a name for a particular generation and replicate', {
  my_sim = new("simulation",
               parameters = list("generations" = 1250, "replicates" = 10),
               sim.dir = "/Users/frederic/Desktop/test_package/",
               exe.dir = "/Users/frederic/Desktop/test_package/",
               exe.name = "quantiNemo2"
  )
  expect_equal(getPostInfo(my_sim,replicate = 5, generation = -5),"_g1246_r05")
  expect_equal(getPostInfo(my_sim,replicate = 0, generation = 100),"_g0100")
  expect_equal(getPostInfo(my_sim),"_g1250")
})

