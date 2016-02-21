# Library paths
libs <- .libPaths();
cat(".libPaths():\n");
cat(sprintf("%02d. %s\n", seq_along(libs), libs), sep="");

# Installed packages
dd <- installed.packages();
pkgs <- paste(dd[,"Package"], dd[,"Version"], sep="_");
paths <- dd[,"LibPath"];
cat(sprintf("%s: %s\n", pkgs, paths), sep="");

# Session information
cat("sessionInfo():\n");
info <- utils::sessionInfo();
print(info);

