# update generic ballot
message("Updating Generic Ballot")
message("...")
tictoc::tic()
source("scripts/generic_ballot_weighting.R")
tictoc::toc()
message("Generic Ballot updated")
message()

# updated presidential approval
message("Updating Presidential Approval")
message("...")
tictoc::tic()
source("scripts/approval_trends_weighting.R")
tictoc::toc()
message("Presidential Approval updated")
message()
