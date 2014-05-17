installTestPkgs <- function(pkgPaths=NULL, ...) {
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  # Validate arguments
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  # Argument 'pkgPaths':
  pkgs <- names(pkgPaths);
  if (length(pkgPaths) > 0L) {
    stopifnot(!is.null(pkgPaths));
    stopifnot(!is.null(pkgs));
  }

  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  # Install/link to request packages to 'test', if missing
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  if (length(pkgs) > 0L) {
    # Where to install/link to packages
    libPath <- getLibPath("test");
    stopifnot(file.exists(libPath));

    # Are package already installed?
    pkgDB <- installed.packages();
    nok <- !is.element(pkgs, pkgDB[,"Package"]);
    if (any(nok)) {
      stop("Cannot install/link to non-existing packages: ",
            paste(sQuote(pkgs[nok]), collapse=", "));
    }

    # Install/link to each package
    for (pkg in pkgs) {
      cat(sprintf("Installing/link to package '%s'...\n", pkg));
      pkgPath <- pkgPaths[[pkg]];
      cat("  Package path: ", pkgPath, "\n", sep="");
      stopifnot(file.exists(pkgPath));
      to <- file.path(libPath, basename(pkgPath));
      if (!file.exists(to)) {
        # First try to create a symbolic link
        file.symlink(from=pkgPath, to=to);
        if (file.exists(to) && file.info(to)$size == 0L) {
          file.remove(to);
        }
        # Otherwise, copy the package
        if (!file.exists(to)) {
          file.copy(from=pkgPath, to=libPath, recursive=TRUE);
        }
      }
      cat(sprintf("Installing/link to package '%s'...done\n", pkg));
    }

    # Assert
    pkgDB <- installed.packages(lib.loc=libPath);
    nok <- !is.element(pkgs, pkgDB[,"Package"]);
    if (any(nok)) {
      stop("Failed to install/link to package(s): ",
            paste(sQuote(pkgs[nok]), collapse=", "));
    }
  }

  invisible(pkgs);
} # installTestPkgs()


installAll <- function(..., pkgPaths=NULL, delta=1.0, offline=FALSE, recursive=TRUE) {
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  # Validate arguments
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  # Argument 'pkgPaths':
  pkgs <- names(pkgPaths);
  if (length(pkgPaths) > 0L) {
    stopifnot(!is.null(pkgPaths));
    stopifnot(!is.null(pkgs));
  }


  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  # Install/link to request packages to 'test', if missing
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  installTestPkgs(pkgPaths);


  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  # Record timestamp
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  # Time since last install?
  fi <- file.info('.timestamp,install');
  t0 <- fi$mtime;
  dt <- Sys.time() - t0;

  # Skip if recently installed
  isOld <- is.na(dt) || (dt > delta * 3600);

  cat(sprintf("Last update/install of packages: %s (%.2f %s ago) => %s\n", t0, dt, attr(dt, "units"), if (isOld) "install" else "skipping"));


  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  # Install packages to be submitted
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  pkgsToSubmit <- getPackagesToSubmit();
  cat("Packages to be submitted to CRAN:\n");
  print(pkgsToSubmit);

  # Update existing packages
  if (isOld && !offline) {
    updatePackages();
  }

  # Install packages to be submitted
  installPackagesToSubmit();

  # Install dependent packages
  if (isOld && !offline) {
    installPackageDependencies(recursive=recursive);
  }

  # Packages to install
  pkgsToInstall <- pkgsToSubmit$Package;
  rm(pkgsToSubmit);

  # For additional packages
  pkgsToInstall <- unique(c(pkgsToInstall, pkgsToInstall()));


  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  # Install CRAN versions of packages and their dependencies
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  if (isOld && !offline) {
    # Identify dependencies
    deps <- pkgDependenciesWithMaintainers(pkgsToInstall, reverse=TRUE, recursive=recursive, offline=offline);

    pkgDeps <- deps$Package;
    cat("Reverse package dependencies on CRAN:\n");
    print(pkgDeps);

    pkgsNonCRAN <- attr(deps, "unknown");
    if (length(pkgsNonCRAN) > 0L) {
      cat("Reverse package dependencies not on CRAN:\n");
      print(pkgsNonCRAN);
    }

    # Install CRAN versions of all packages and their dependencies
    cat("Updating/installing all needed CRAN packages:\n");
    pkgsToInstall <- c(pkgsToInstall, pkgDeps);
    if (isOld && !offline) {
      installPackages(pkgsToInstall, ignore=TRUE, recursive=recursive);
    }
  }

  if (isOld && !offline) {
    file.create('.timestamp,install');
  }
} # installAll()


############################################################################
# HISTORY:
# 2014-05-07
# o Added argument 'recursive' to installAll().
# 2012-09-25
# o BUG FIX: Forgot to install all dependencies of the package tested.
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
