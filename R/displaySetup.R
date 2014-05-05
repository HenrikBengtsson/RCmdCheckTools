displaySetup <- function(...) {
  cat("Session info:\n");
  print(utils::sessionInfo());
  
  cat("R system environments:\n");
  Renv <- Sys.getenv("R_CHECK_ENVIRON", NA);
  if (file.exists(Renv)) readRenviron(Renv);
  envs <- Sys.getenv();
  keep <- grep("^(R_|_R_)", names(envs));
  envs <- envs[keep];
  cat(paste(names(envs), "=", envs, sep=""), sep="\n");
  
  cat("Library paths:\n");
  print(.libPaths());
  
  cat("Installed packages:\n");
  t <- utils::installed.packages();
  t <- t[,c("Package","Version", "LibPath")];

  # Drop duplicates
  d <- duplicated(t[,"Package"]); t <- t[!d,];

  cat("Library paths (2nd take):\n");
  print(unique(t[,"LibPath"]));

  pkgs <- sprintf('%s v%s (%s)', t[,"Package"], t[,"Version"], t[,"LibPath"]);
  pkgs <- sort(pkgs);
  cat(pkgs, sep="\n");
} # displaySetup()
