setupAll <- function(...) {
  reposNames <- getReposNames();
  cat("Repositories:\n");
  print(sapply(reposNames, FUN=getRepos));

  cat("Library paths:\n");
  libs <- getLibPaths(c("test", reposNames, "default"));
  print(libs);

  libPaths <- useLibPaths();
  cat("Library paths for testing:\n");
  print(libPaths);
} # setupAll()


############################################################################
# HISTORY:
# 2012-08-19
# o Now Summary.txt is written to check,<rver>/ directory.
# 2012-07-12
# o Now downloadPackages() only returns the packages requested, regardless
#   of other packages in the download directory.
# 2012-07-10
# o Now generating a Summary.txt file.
# o Now generating one <pkg>,EmailToCRAN.txt per package.
# 2012-07-04
# o Added updatePackages().
# o BUG FIX: This tool forgot to install package dependencies that the
#   packages to be submitted need.
# o Added useLibPaths().
# 2012-07-03
# o CLEANUP: Generalized and modularized the script.
# o Now also testing Suggests packages. Proposed by Brian Ripley.
# 2012-06-2?
# o Created.
############################################################################
