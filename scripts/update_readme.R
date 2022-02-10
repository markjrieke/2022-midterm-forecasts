# update generic ballot
message("Updating Generic Ballot")
message("...")
tictoc::tic()
source("scripts/model_run/generic_ballot_update.R")
tictoc::toc()
message("Generic Ballot updated")
message()

# updated presidential approval
message("Updating Presidential Approval")
message("...")
tictoc::tic()
source("scripts/model_run/approval_update.R")
tictoc::toc()
message("Presidential Approval updated")
message()
