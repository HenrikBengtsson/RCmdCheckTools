# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Local functions
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Source: http://developer.r-project.org/CRAN/Scripts/depends.R
pkgDependencies <- function(packages, which=c("Depends", "Imports", "Suggests", "LinkingTo"), recursive=FALSE, reverse=FALSE, repos=getRepos(c("CRAN", "BioCsoft")), offline=FALSE, force=FALSE) {
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  # Local function
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  loadDB <- function(repos, offline=FALSE, force=FALSE) {
    url <- sprintf("%s/src/contrib/PACKAGES", repos);
    # Memoized?
    key <- url;
    db <- getOption(key);
    if ((force && !offline) || is.null(db)) {
      con <- if(substring(url, 1L, 7L) == "file://") {
        file(url, open="r");
      } else {
        url(url, open="r");
      }
      on.exit(close(con));
      db <- read.dcf(con);
      args <- list(db); names(args) <- key;
      do.call("options", args=args);
    }
    db;
  } # loadDB()

  # Load all available package information
  db <- lapply(repos, FUN=loadDB, offline=offline, force=force);
  common <- lapply(db, FUN=colnames);
  common <- Reduce(intersect, common);
  db <- lapply(db, FUN=function(x) x[,common]);
  db <- Reduce(rbind, db);
  db <- unique(db);

  # Look for dependencies
  rdepends <- tools::package_dependencies(packages, db=db,
                                   which=which,
                                   recursive=recursive,
                                   reverse=reverse);
  rdepends <- sort(unique(unlist(rdepends)));

  pos <- match(rdepends, db[, "Package"], nomatch=0L);
  res <- db[pos, c("Package", "Version"), drop=FALSE];
  res <- as.data.frame(res, stringsAsFactors=FALSE);
  res$fullname <- sprintf("%s %s", res$Package, res$Version);

  # Any non-CRAN packages?
  if (any(pos == 0L)) {
    unknownPkgs <- rdepends[(pos == 0L)];
    attr(res, "unknown") <- unknownPkgs;
  }

  res;
} # pkgDependencies()


getRequiredPackages <- function(pkgsToTest, which=c("all", "missing"), repos=getRepos(c("CRAN", "BioCsoft"))) {
  which <- match.arg(which);

  if (length(repos) > 1L) {
    pkgs <- lapply(repos, FUN=function(repos) {
      getRequiredPackages(pkgsToTest, which=which, repos=repos);
    })
    pkgs <- sort(unlist(pkgs, use.names=FALSE));
    return(pkgs);
  }

  contriburl <- contrib.url(repos, "source");
  avail <- available.packages(contriburl=contriburl);
  pkgs <- tools::package_dependencies(pkgsToTest$Package, which=c("Depends", "Imports", "LinkingTo", "Suggests"), db=avail);
  pkgs <- unlist(pkgs, use.names=FALSE);
  pkgs <- unique(pkgs);
  if (which == "missing") {
    libs <- getLibPaths(c(getReposNames(), "default"));
    pkgs <- setdiff(pkgs, installed.packages(lib.loc=libs)[,"Package"]);
  }
  pkgs;
} # getRequiredPackages()


nonBasePackages <- function(pkgs, ...) {
  # Exclude 'base' packages
  lib <- .libPaths();
  lib <- lib[length(lib)];
  avail <- installed.packages(lib.loc=lib);
  avail <- as.data.frame(avail, stringsAsFactors=FALSE);
  Priority <- NULL; rm(Priority); # To please R CMD check
  pkgsAvail <- subset(avail, Priority == "base")$Package;
  pkgsLeft <- setdiff(pkgs, pkgsAvail);
  pkgsLeft;
} # nonBasePackages()


nonInstalledPackages <- function(pkgs, reposNames=getReposNames(), ...) {
  repositories <- sapply(reposNames, FUN=getRepos);
  libs <- sapply(reposNames, FUN=getLibPath);

  cat("Identifying already installed packages...\n");
  for (rr in seq_along(repositories)) {
    # Nothing todo?
    if (length(pkgs) == 0L) {
      return(pkgs);
    }
    repos <- repositories[rr];
    lib <- libs[rr];
    cat("  Repository: ", repos, "\n", sep="");
#    cat("  Library path: ", lib, "\n", sep="");

    # Identify missing packages
    avail <- installed.packages(lib.loc=lib, ...);
    availPkgs <- avail[,"Package"];
    pkgs <- setdiff(pkgs, availPkgs);

#    cat("  Remaining packages to install:\n");
#    print(pkgs);
  } # for (rr ...)
  cat("Identifying already installed packages...done\n");
  pkgs;
} # nonInstalledPackages()


updatePackages <- function(reposNames=getReposNames(), ...) {
  # Update all libraries automatically
  cat("Updating packages...\n");

  # Already updated once today?
  today <- Sys.Date();
  if (getOption("SubmitToCRAN/lastUpdated", today-1) >= today) {
    cat("Already done today\n");
    cat("Updating packages...done\n");
    return();
  }

  repositories <- sapply(reposNames, FUN=getRepos);
  libs <- sapply(reposNames, FUN=getLibPath);

  for (rr in seq_along(repositories)) {
    repos <- repositories[rr];
    lib <- libs[rr];
    cat("  Repository: ", repos, "\n", sep="");
#    cat("  Library path: ", lib, "\n", sep="");
    # Update existing packages
    update.packages(repos=repos, lib.loc=lib, ..., ask=FALSE);
  } # for (rr ...)

  options("SubmitToCRAN/lastUpdated"=today);

  cat("Updating packages...done\n");
} # updatePackages()


installPackages <- function(pkgs, dependencies=c("Depends", "Imports", "LinkingTo", "Suggests", "Enhances"), reposNames=getReposNames(), deps=TRUE, recursive=TRUE, update=FALSE, type=getOption("pkgType"), ..., ignore=FALSE) {
  # Sanity check
  stopifnot(all(regexpr("\\n", pkgs) == -1));

  # Exclude 'base' packages
  pkgs <- nonBasePackages(pkgs);

  # Sanity check
  stopifnot(all(regexpr("\\n", pkgs) == -1));

  # Nothing todo?
  if (length(pkgs) == 0L) {
    return();
  }

  pkgs <- unique(pkgs);
  cat("Packages of interest:\n");
  print(pkgs);

  if (ignore) {
    pkgsIgnore <- pkgsToSkip("inst");
#    cat("Packages to skip:\n");
#    print(pkgsIgnore);
    pkgs <- setdiff(pkgs, pkgsIgnore);

    # Nothing todo?
    if (length(pkgs) == 0L) {
      return();
    }
  }

  cat("Repositories:\n");
  print(reposNames);

  repositories <- sapply(reposNames, FUN=getRepos);
  libs <- sapply(reposNames, FUN=getLibPath);

  pkgsLeft <- pkgs;

  # (a) Identify all package dependencies?
  if (deps) {
    cat("Finding package dependencies...\n");
    cat("Packages:\n");
    print(pkgs);
    cat("Recursive: ", recursive, "\n", sep="");
    cat("What: ", paste(dependencies, collapse=", "), "\n", sep="");
    # Only install first generation of Suggested packages
    if (recursive && is.element("Suggests", dependencies)) {
      pkgDeps <- pkgDependencies(pkgs, which=dependencies, recursive=FALSE);
      cat("Packages they directly depend on:\n");
      print(pkgDeps);
      dependencies <- setdiff(dependencies, "Suggests");
      pkgDeps2 <- pkgDependencies(pkgs, which=dependencies, recursive=TRUE);
      keep <- !is.element(pkgDeps2$Package, pkgDeps$Package);
      pkgDeps2 <- pkgDeps2[keep,];
      cat("Packages they indirectly depend on:\n");
      print(pkgDeps2);
      pkgDeps <- rbind(pkgDeps, pkgDeps2);
    } else {
      pkgDeps <- pkgDependencies(pkgs, which=dependencies, recursive=recursive);
    }
    pkgDeps <- c(pkgDeps$Package, attr(pkgDeps, "unknown"));
    cat("Packages they depend on:\n");
    print(pkgDeps);

    if (ignore) {
      pkgDeps <- setdiff(pkgDeps, pkgsIgnore);
    }

    # Package to install
    pkgsLeft <- c(pkgsLeft, pkgDeps);
    pkgsLeft <- unique(pkgsLeft);

    # Exclude 'base' packages
    pkgsLeft <- nonBasePackages(pkgsLeft);

    cat("Finding package dependencies via CRAN...done\n");
  }

  pkgsLeft <- unique(pkgsLeft);

  # (b) Update all libraries automatically?
  if (update) {
    updatePackages(reposNames=reposNames);
  }


  # (c) Identifying already installed packages
  pkgsLeft <- nonInstalledPackages(pkgsLeft);


  # (d) Install remaining packages
  cat("Installing remaining packages...\n");
  cat("  Packages to be installed:\n");
  print(pkgsLeft);

  for (rr in seq_along(repositories)) {
    # Nothing to install?
    if (length(pkgsLeft) == 0L) break;

    repos <- repositories[rr];
    lib <- libs[rr];
    cat("  Repository: ", repos, "\n", sep="");
    cat("  Library path: ", lib, "\n", sep="");

    pkgsBefore <- installed.packages(lib.loc=lib, ...)[,"Package"];
    suppressWarnings({
      install.packages(pkgsLeft, dependencies=dependencies, repos=repos, lib=lib, type=type, ...);
    });
    pkgsAfter <- installed.packages(lib.loc=lib, ...)[,"Package"];
    pkgsInstalled <- setdiff(pkgsAfter, pkgsBefore);
    cat("  Packages installed:\n");
    print(pkgsInstalled);
    pkgsLeft <- setdiff(pkgsLeft, pkgsInstalled);
  } # for (rr ...)
  cat("Installing remaining packages...done\n");

  if (length(pkgsLeft) > 0L) {
    cat("Packages that were not installed:\n");
    print(pkgsLeft);
    warning(sprintf("The following packages were not installed: %s", paste(pkgsLeft, collapse=", ")));
  }
} # installPackages()


testPackages <- function(...) {
  path <- getDownloadPath();
  pattern <- "_[0-9.-]*[.]tar[.]gz$";
  pathnames <- list.files(path=path, pattern=pattern, full.names=TRUE);
} # testPackages()


listPackages <- function(path) {
  pattern <- "(.*)_([0-9.-]*)[.]tar[.]gz$";
  pathnames <- list.files(path, pattern=pattern, full.names=TRUE);
  filenames <- basename(pathnames);
  packages <- gsub(pattern, "\\1", filenames);
  versions <- gsub(pattern, "\\2", filenames);
  data.frame(
    Package = packages,
    Version = versions,
    fullname = sprintf("%s %s", packages, versions),
    pathname = pathnames,
    stringsAsFactors=FALSE
  );
} # listPackages()


getPackagesToSubmit <- function() {
  pkgs <- listPackages(".");
  if (length(pkgs) <= 1L) {
    return(pkgs);
  }

  # Order by relative dependencies
  depList <- lapply(pkgs$Package, FUN=tools::dependsOnPkgs, recursive=TRUE);
  names(depList) <- pkgs$Package;
  depList <- lapply(depList, FUN=intersect, pkgs$Package);

  # Order by size
  ns <- sapply(depList, FUN=length);
  o <- order(ns, decreasing=TRUE);
  depList <- depList[o];
  pkgs <- pkgs[o,];

##  # Order internally
##  while(TRUE) {
##    depList0 <- depList;
##    # Ready?
##    if (identical(depList, depList0)) break;
##  }

  # Return
  pkgs;
} # getPackagesToSubmit()


installPackagesToSubmit <- function(toSubmit=getPackagesToSubmit(), skip=TRUE) {
  missingPackages <- function(pkgs) {
    avail <- installed.packages(lib.loc=getLibPath("test"));
    avail <- avail[,c("Package", "Version"),drop=FALSE];
    avail <- as.data.frame(avail, stringsAsFactors=FALSE);
    pkgsF <- sprintf("%s_%s", pkgs$Package, pkgs$Version);
    isAvailF <- sprintf("%s_%s", avail$Package, avail$Version);
    missing <- !is.element(pkgsF, isAvailF);
    pkgs <- pkgs[missing,];
    pkgs;
  } # missingPackages()

  # Nothing to do?
  if (nrow(toSubmit) == 0L) {
    stop("No package to be submitted.");
  }

  # Skip already installed test packages?
  if (skip && nrow(toSubmit) > 0L) {
    toSubmit <- missingPackages(toSubmit);
  }

  # Any test packages to be installed?
  if (nrow(toSubmit) > 0L) {
    pathnames <- toSubmit$pathname;

    # Assert that files exists
    ok <- (file.access(pathnames, mode=0) == 0L)
    if (any(!ok)) {
      stop("Package file does not exists: ", paste(pathnames[!ok], collapse=", "));
    }

    # Assert read file permissions
    ok <- (file.access(pathnames, mode=4) == 0L)
    if (any(!ok)) {
      stop("Package file exists, but without read permissions: ", paste(pathnames[!ok], collapse=", "));
    }

    # Try to install
    install.packages(pathnames, type="source", repos=NULL, lib=getLibPath("test"));

    # Assert that the packages were really installed
    pkgsM <- missingPackages(toSubmit);
    if (nrow(pkgsM) > 0L) {
      msg <- sprintf("Failed to install packages to be submitted:\n%s\n",
                     paste(capture.output(print(pkgsM)), collapse="\n"))
      fi <- file.info(pkgsM$pathname)
      msg <- sprintf("%s\n%s\n", msg,
                     paste(capture.output(print(fi)), collapse="\n"))
      msg <- sprintf("%s\nCurrent working directory: %s\n", msg, getwd())
      stop(msg)
    }
  }

  # Return packages that were actually installed
  invisible(toSubmit);
} # installPackagesToSubmit()


installPackageDependencies <- function(toSubmit=getPackagesToSubmit(), recursive=TRUE) {
  # Nothing to do?
  if (nrow(toSubmit) == 0L) {
    return();
  }

  # Install dependencies
  deps <- pkgDependencies(toSubmit$Package, reverse=FALSE, recursive=recursive);
  # Nothing to do?
  if (nrow(deps) == 0L) {
    return();
  }

  cat("Package dependencies for packages to be submitted:\n");
  print(deps);

  depsAll <- NULL;
  pkgDeps <- deps$Package;
  for (pkg in pkgDeps) {
    pd <- packageDescription(pkg);
    # Installed?
    if (!is.list(pd) && is.na(pd)) {
      installPackages(pkg, deps=TRUE, recursive=recursive, update=FALSE, ignore=TRUE);
      pd <- packageDescription(pkg);
    }
    if (is.list(pd)){
      deps <- pd[c("Depends", "Imports", "Suggests")];
      deps <- unlist(deps, use.names=FALSE);
      deps <- gsub("\\([^)]*\\)", "", deps);
      deps <- gsub("[ \n]", "", deps);
      deps <- strsplit(deps, split=",");
      deps <- unlist(deps, use.names=FALSE);
      deps <- setdiff(deps, "R");
      deps <- unique(deps);
      depsAll <- c(depsAll, deps);
    }
  } # for (pkg ...)
  depsAll <- unique(depsAll);

  installPackages(depsAll, deps=TRUE, recursive=recursive, update=FALSE, ignore=TRUE);
} # installPackageDependencies()


readPkgsToFile <- function(name, which=NULL, ...) {
  fullname <- paste(c(name, which), collapse=",");
  filename <- sprintf("%s.txt", fullname);

  paths <- c("..", ".");
  pathnames <- file.path(paths, filename);
  pathnames <- pathnames[file.exists(pathnames)];

  # Nothing to do?
  if (length(pathnames) == 0L) {
    return();
  }

  res <- lapply(pathnames, FUN=function(pathname) {
    pkgs <- readLines(pathname);
    pkgs <- gsub("#.*", "", pkgs);
    pkgs <- gsub("^[ \t]*", "", pkgs);
    pkgs <- gsub("[ \t]*$", "", pkgs);
    pkgs <- pkgs[nchar(pkgs) > 0L];
    pkgs;
  });

  unlist(res, use.names=FALSE);
} # readPkgsToFile()


pkgsToSkip <- function(which=c("", "inst"), ..., path="~") {
  # Argument 'which':
  which <- match.arg(which);

  # WAS:
  ## readPkgsToFile("PackagesToSkip", which, ...);

  filename <- sprintf(".RCmdCheckTools-%signore", which);
  pathname <- file.path(path, filename);
  keep <- sapply(pathname, FUN=function(f) file_test("-f", f))
  pathname <- pathname[keep];
  pkgs <- character(0L);
  if (length(pathname) > 0L) {
    pkgs <- sapply(pathname, FUN=readLines, warn=FALSE);
    pkgs <- gsub("#.*", "", pkgs);
    pkgs <- gsub("^[ ]*", "", pkgs);
    pkgs <- gsub("[ ]*$", "", pkgs);
    pkgs <- pkgs[nzchar(pkgs)]
  }
  unique(pkgs);
} # pkgsToSkip()

pkgsToInstall <- function(..., path="~") {
  # WAS:
  ## readPkgsToFile("PackagesToInstall", ...);

  filename <- ".RCmdCheckTools-instalways";
  pathname <- file.path(path, filename);
  keep <- sapply(pathname, FUN=function(f) file_test("-f", f))
  pathname <- pathname[keep];
  pkgs <- character(0L);
  if (length(pathname) > 0L) {
    pkgs <- sapply(pathname, FUN=readLines, warn=FALSE);
    pkgs <- gsub("#.*", "", pkgs);
    pkgs <- gsub("^[ ]*", "", pkgs);
    pkgs <- gsub("[ ]*$", "", pkgs);
    pkgs <- pkgs[nzchar(pkgs)]
  }
  unique(pkgs);
}


R.bin <- function() {
  path <- R.home("bin");
  path <- file.path(path, "R");
  path <- normalizePath(path, mustWork=FALSE);
  path;
} # R.bin()


getRLIBSSep <- function() {
  path <- Sys.getenv("PATH");
  for (sep in c(";", ":")) {
    pattern <- sprintf("[%s]", sep);
    if (regexpr(pattern, path) != -1) return(sep);
  }
  stop("Failed to infer R_LIBS separator.");
} # getRLIBSSep()


############################################################################
# HISTORY:
# 2014-06-07
# o ROBUSTNESS: Now installPackagesToSubmit() asserts that the packages
#   to be submitted are really installed.
# 2014-06-05
# o Now pkgsToInstall() utilizes ~/.RCmdCheckTools-instalways.
# 2014-05-19
# o Now pkgDependencies() finds dependencies across repositories.
# o Now getRequiredPackages() also returns "Suggested" packages.
# 2014-05-18
# o Renamed pkgDependenciesWithMaintainers() to pkgDependencies().
# o Now pkgDependenciesWithMaintainers() works for multiple repositories
#   at onces and also other onces than CRAN.
# 2014-05-07
# o Added argument 'recursive' to installPackages().
# 2014-05-05
# o Updated pkgsToSkip() to use ~/.RCmdCheckTools-.*ignore instead.
# 2013-01-15
# o Added pkgsToInstall().
# 2013-01-14
# o Added getRLIBSSep().
# o Added R.bin().
# o Added argument 'ignore' to installPackages().
# o Added argument 'which' to pkgsToSkip().
# 2012-12-02
# o Added pkgsToSkip().
# 2012-11-18
# o Added argument skip=TRUE to installPackagesToSubmit().
# 2012-10-23
# o Added argument 'deps' and 'type' to installPackages().
# 2012-10-19
# o Removed sortCheckDirs().
# 2012-10-15
# o Added sortCheckDirs().
# 2012-08-19
# o Now Summary.txt is written to check,<rver>/ directory.
# 2012-07-04
# o Added updatePackages().
# o BUG FIX: This tool forgot to install package dependencies that the
#   packages to be submitted need.
# 2012-07-03
# o CLEANUP: Generalized and modularized the script.
# o Now also testing Suggests packages. Proposed by Brian Ripley.
# 2012-06-2?
# o Created.
############################################################################
