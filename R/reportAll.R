reportAll <- function(recursive=TRUE, ...) {
  cat("- - - - - - - - - - - - - - - - - - - - - - - - - - - - - -\n");
  cat(" RESULTS \n");
  cat("- - - - - - - - - - - - - - - - - - - - - - - - - - - - - -\n");
  outPath <- sprintf("checks,%s", getRversion());
  rCmdCheckSummary(outPath);

  # Identify dependencies
  pkgsToSubmit <- getPackagesToSubmit();
  deps <- pkgDependenciesWithMaintainers(pkgsToSubmit$Package, reverse=TRUE, recursive=recursive);
  pkgDeps <- deps$Package;
  pkgsToTest <- downloadPackages(pkgDeps);
  pkgsIgnore <- unique(c(pkgsToSkip("inst"), pkgsToSkip()));

  filename <- "Summary.txt";
  pathname <- file.path(outPath, filename);
  for (kk in seq(length=nrow(pkgsToTest))) {
    pkg <- pkgsToTest$Package[kk];
    # Skip package?
    if (is.element(pkg, pkgsIgnore)) {
      cat(sprintf("Package %s: skipping\n", pkg));
      next;
    }
    pathnameKK <- pkgsToTest$pathname[kk];
    res <- rCmdCheck(pathnameKK, verbose=FALSE);
    msg <- sprintf("Package %s: %s\n", pkg, paste(res, collapse=", "));
    cat(msg);
    cat(msg, file=pathname, append=TRUE);
  } # for (pkg ...)

  if (require("R.rsp")) {
    pathname <- system.file("rsp/EmailToCRAN.txt.rsp", package="RCmdCheckTools", mustWork=TRUE);
    for (pp in 1:nrow(pkgsToSubmit)) {
      pathnameT <- R.rsp::rsp(pathname)
      filenameP <- sprintf("%s,EmailToCRAN.txt", pkgsToSubmit$Package[pp]);
      file.rename(pathnameT, filenameP);
    }
  }
} # reportAll()


############################################################################
# HISTORY:
# 2014-05-07
# o Added argument 'recursive' to reportAll().
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
