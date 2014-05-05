testAll <- function(..., verbose=FALSE) {
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  # Identify package to be submitted
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  pkgsToSubmit <- getPackagesToSubmit();
  if (nrow(pkgsToSubmit) > 1L) {
    stop("Only one package can be submitted to CRAN at any time: ", paste(sQuote(pkgsToSubmit$pathname), collapse=", "));
  }


  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  # Identify dependencies
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  deps <- pkgDependenciesWithMaintainers(pkgsToSubmit$Package, reverse=TRUE, recursive=TRUE);
  pkgDeps <- deps$Package;
  # In case of circular dependencies, don't test packages
  # to be submitted.
  pkgDeps <- setdiff(pkgDeps, pkgsToSubmit$Package);


  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  # Download dependendencies to be tests
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  cat("Downloading reverse package dependencies:\n");
  pkgsToTest <- downloadPackages(pkgDeps);



  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  # Setup up test environment to test packages
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  cat("CRAN packages to test:\n");
  print(pkgsToTest);

  pkgsIgnore <- unique(c(pkgsToSkip("inst"), pkgsToSkip()));
  cat("CRAN packages to skip:\n");
  print(pkgsIgnore);

  missing <- getRequiredPackages(pkgsToTest, which="missing");
  #stopifnot(length(missing) == 0L);

  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  # Test the package to be submitted
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  # Display/log test results
  rCmdCheckSetup(verbose=verbose);
  local({
    outPath <- sprintf("checks,%s", getRversion());
    pathname <- file.path(outPath, "setup.log");
    rCmdCheckSetup(logfile=pathname, verbose=verbose);
    rCmdCheckSummary(outPath);
  });

  for (kk in seq(length=nrow(pkgsToSubmit))) {
    pkgToSubmit <- as.list(pkgsToSubmit[kk,]);
    pkg <- pkgToSubmit$fullname;
    pathname <- pkgToSubmit$pathname;
    cat(sprintf("Testing package to be submitted: %s\n", pkg));
    res <- rCmdCheck(pathname);
    cat(sprintf("Package %s: %s\n", pkg, paste(res, collapse=", ")));
  } # for (kk ...)


  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  # Test dependent packages
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  # Display/log test results
  rCmdCheckSetup(verbose=verbose);
  local({
    outPath <- sprintf("checks,%s", getRversion());
    pathname <- file.path(outPath, "setup.log");
    rCmdCheckSetup(logfile=pathname, verbose=verbose);
    rCmdCheckSummary(outPath);
  });

  for (kk in seq(length=nrow(pkgsToTest))) {
    pkg <- pkgsToTest$Package[kk];
    # Skip package?
    if (is.element(pkg, pkgsIgnore)) {
      cat(sprintf("Package %s: skipping\n", pkg));
      next;
    }
    pathname <- pkgsToTest$pathname[kk];
    res <- rCmdCheck(pathname);
    cat(sprintf("Package %s: %s\n", pkg, paste(res, collapse=", ")));
  } # for (pkg ...)

} # testAll()


############################################################################
# HISTORY:
# 2013-09-19
# o Now testAll() also tests the package(s) to be submitted.
# 2013-05-18
# o Packages to be submitted are never considered reverse dependent
#   of themselves, regardless of circular dependencies or not.
# 2012-12-01
# o Added support for PackagesToSkip.txt.
# 2012-10-21
# o Added rCmdCheckSummary() at the end.
# 2012-08-19
# o Now Summary.txt is written to check,<rver>/ directory.
# 2012-07-10
# o Now generating a Summary.txt file.
# o Now generating one <pkg>,EmailToCRAN.txt per package.
# 2012-07-03
# o CLEANUP: Generalized and modularized the script.
# o Now also testing Suggests packages. Proposed by Brian Ripley.
# 2012-06-2?
# o Created.
############################################################################
