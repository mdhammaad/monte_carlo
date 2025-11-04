library(tidyverse)
library(furrr)

runReplication = function(test, filename, seed) {
  
  set.seed(seed)
  
  if (filename %in% list.files("./sims-a"))
      return()
      
      A  = rnorm(100, mean = 600, sd = 75)
      B  = rnorm(100, mean = 700, sd = 75)
      
      if (test == "Student")
        hypothesis_test = t.test(A, B, var.equal = T)
      if (test == "Welch")
        hypothesis_test = t.test(A, B, var.equal = F)
      if (test == "MW")
        hypothesis_test = wilcox.test(A, B)
      
      result = list(
        testname = test,
        result = hypothesis_test[["p.value"]] < 0.05
      )
      
      saveRDS(result, file = filename)
      
}

sims = expand_grid(
  test = c("Student", "Welch", "MW"),
  rep = 1:1000000
) |> 
  mutate(
    filename = paste0("./sims-a/",test, "-", rep, ".rds"),
    seed = row_number()
  )

plan(multisession, workers = 2)
future_pwalk(list(sims[["test"]], 
                  sims[["filename"]],
                  sims[["seed"]]), 
             runReplication)


results = foreach(file = list.files("sims-a"), 
                  .combine = bind_rows) %do% {
  paste0("./sims-a/", file) |> readRDS()
}

results |> 
  group_by(testname) |> 
  summarize( power = mean(result) )
