testPkgsToSubmit <- function(install=TRUE, test=TRUE, report=TRUE, offline=FALSE, ...) {
  # Install/link to certain packages to 'test' library, that are already
  # installed on this system, before updating the .libPaths() settings.
  pkgs <- c("RCmdCheckTools", Sys.getenv("RCmdCheckTools_xpkgs"));
  pkgs <- pkgs[nchar(pkgs) > 0L];
  pkgs <- unique(pkgs);
  pkgPaths <- lapply(pkgs, FUN=function(pkg) {
    pd <- packageDescription(pkg);
    dirname(dirname(attr(pd, "file")));
  });
  names(pkgPaths) <- pkgs;
  str(as.list(pkgPaths));
  installTestPkgs(pkgPaths);

  setupAll();

  if (install) installAll(offline=offline, ...);

  if (test) testAll();

  if (report) reportAll();
} # testPkgsToSubmit()


############################################################################
# HISTORY:
# 2013-09-22
# o Added arguments 'setup', 'test' and 'report' to testPkgsToSubmit().
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
