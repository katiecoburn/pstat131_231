write_rds(tune_res_churn, file = "lab-5/churn-en-results.rds")
write_rds(tune_res_hitters, file = "lab-5/hitters-en-results.rds")


save(tune_res_churn, file = "lab-5/churn-en-results.rda")

tune_res_churn

load(file = "lab-5/churn-en-results.rda")

tune_res_churn <- read_rds(file = "lab-5/churn-en-results.rds")
tune_res_hitters <- read_rds(file = "lab-5/hitters-en-results.rds")

