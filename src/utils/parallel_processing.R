library(doParallel)
library(parallel)

start_time <- 0

start_parallel_processing <- function () {
  # time start
  start_time <<- Sys.time()

  num_cores <- detectCores() - 1
  cl <<- makePSOCKcluster(num_cores)
  registerDoParallel(cl)
}

end_parallel_processing <- function () {
  stopCluster(cl)
  # time end
  end_time <<- Sys.time()
  end_time - start_time
}
