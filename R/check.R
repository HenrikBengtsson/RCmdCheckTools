rCmdCheckSetup <- function(..., logfile=NULL, verbose=TRUE) {
  # Create a ./Renviron file to be used for the checks
  pathname <- ".Renviron-check";
  envs <- c("_R_CHECK_XREFS_REPOSITORIES_"="\"invalidURL\"");
  envs <- c(envs, "_R_CHECK_FORCE_SUGGESTS_"="FALSE");
  envs <- c(envs, "_R_CHECK_REPLACING_IMPORTS_"="FALSE");
  envs <- c(envs, "## "="NB: The following are ignored with --as-cran");
  envs <- c(envs, "_R_CHECK_WARN_ON_NAMESPACE_"="FALSE");
  if (getRversion() < 3) {
    envs <- c(envs, "R_RD4PDF"="times,hyper");
  }
  envs <- c(envs, "_R_CHECK_WARN_ON_NAMESPACE_"="FALSE");
##  envs <- c(envs, "_R_CHECK_DOT_INTERNAL_"="TRUE");
##  envs <- c(envs, "_R_CHECK_USE_CODETOOLS_"="TRUE");

  ## Sizes
  envs <- c(envs, "_R_CHECK_PKG_SIZES_"="FALSE");
  envs <- c(envs, "_R_CHECK_DOC_SIZES_"="FALSE");
  envs <- c(envs, "_R_CHECK_DOC_SIZES2_"="FALSE");
  envs <- c(envs, "_R_CHECK_COMPACT_DATA_"="FALSE");

  ## Rd
  envs <- c(envs, "_R_CHECK_RD_LINE_WIDTHS_"="FALSE");
  envs <- c(envs, "_R_CHECK_RD_STYLE_"="FALSE");
  envs <- c(envs, "_R_CHECK_RD_XREFS_"="FALSE");

  ## Browsers, viewers etc.
  envs <- c(envs, "R_BROWSER"="false");  # Don't open URLs in browser
  envs <- c(envs, "R_PDFVIEWER"="false");  # Don't open PDFs

  envs <- paste(names(envs), "=", envs);
  cat(file=pathname, envs, sep="\n");
  Sys.setenv("R_CHECK_ENVIRON"=pathname);
  # Just in case, also set it here.
  readRenviron(pathname);

  libs <- c("test", getReposNames(), "default");
  libs <- getLibPaths(libs, collapse=TRUE);
  Sys.setenv("R_LIBS"=libs, "R_LIBS_USER"=getLibPaths("test"));

  # Sanity check
  path <- Sys.getenv("R_LIBS_USER");
  if (path == "") {
    stop("INTERNAL ERROR: Failed to set 'R_LIBS_USER'");
  } else if (!file.exists(path) || !file.info(path)$isdir) {
    stop("INTERNAL ERROR: 'R_LIBS_USER' is not an existing directory: ", path);
  }

  tmpfile <- tempfile(fileext=".R");
  on.exit({
    if (file.exists(tmpfile)) file.remove(tmpfile);
  });
  cat("RCmdCheckTools::displaySetup()", file=tmpfile);
  args <- c("--vanilla", "--slave", "-f", tmpfile);
  stdout <- "";
  if (!is.null(logfile)) {
    mkdirs(dirname(logfile));
    stdout <- logfile;
  }
#  if (TRUE) { cat("Command:\n"); print(cmd); }

  system2(R.bin(), args=args, stdout=stdout);
} # rCmdCheckSetup()


getRCmdCheckResults <- function(path, ...) {
  stopifnot(file.exists(path));
  print(path);
  parent <- dirname(path);
  if (identical(basename(parent), "SKIP")) return("SKIP");

  filenames <- c("00install.out", "00check.log");
  pathnames <- file.path(path, filenames);
  pathnames <- pathnames[sapply(pathnames, FUN=file.exists)];
  stopifnot(length(pathnames) > 0);

  res <- character(0);
  for (pathname in pathnames) {
    bfr <- suppressWarnings(readLines(pathname));
    for (what in c("NOTE", "WARNING", "ERROR")) {
      pattern <- sprintf("%s$", what);
      if (length(grep(pattern, bfr) > 0L)) {
        res <- c(res, what);
      }
    }

    # Add 'NOTE1' if 'NOTE: There was 1 note.' is reported
    if (any(regexpr("NOTE: There was 1 note.", bfr, fixed=TRUE) != -1L)) {
      res <- c(res, "NOTE1");
    }
  } # for (kk ...)

  if (length(res) == 0L) res <- "OK";

  res;
} # getRCmdCheckResults()


rCmdCheck <- function(pathname, ..., verbose=TRUE) {
##  rootPath <- sprintf("checks,%s", getRversion());
  rootPath <- "checks"
  mkdirs(rootPath);

  pattern <- "_[0-9.-]*[.]tar[.]gz$";
  pkg <- gsub(pattern, "", basename(pathname));

  # Already filed?
  results <- c("ERROR", "WARNING", "NOTE1", "NOTE", "OK", "SKIP");
  dirname <- sprintf("%s.Rcheck", pkg);
  paths <- file.path(rootPath, results, dirname);
  path <- paths[file.exists(paths)];
  stopifnot(length(path) <= 1L);
  if (length(path) == 1L) {
    done <- isTRUE(file.info(path)$isdir);
    if (done) {
      res <- getRCmdCheckResults(path);
      return(res);
    }
  }


  if (verbose) cat(sprintf("Testing package: %s\n", pkg));
  path0 <- file.path(rootPath, "tmp");
  path <- file.path(path0, dirname);
  isdir <- isTRUE(file.info(path)$isdir);
  if (!isdir) {
    logfile <- file.path(path0, "setup.log");
    rCmdCheckSetup(logfile=logfile, verbose=verbose);
    cmd <- sprintf("%s --no-init CMD check --as-cran -o %s %s", R.bin(), path0, pathname);
    if (verbose) cat("Command:\n");
    if (verbose) print(cmd);
    system(cmd);
  }
  res <- getRCmdCheckResults(path);

  # File according to result
  for (result in results) {
    if (is.element(result, res)) {
      pathD0 <- file.path(rootPath, result);
      if (!file.exists(pathD0)) dir.create(pathD0);
      pathD <- file.path(pathD0, dirname);
      file.rename(path, pathD);
      break;
    }
  }

  res;
} # rCmdCheck()


rCmdCheckSummary <- function(path, after=c(ERROR=100, WARNING=6, NOTE=6, NOTE1=6), ...) {
  results <- c("ERROR", "WARNING", "NOTE", "NOTE1");
  for (kk in seq(along=results)) {
    resultT <- results[kk];
    pathT <- file.path(path, resultT);
    if (!file.exists(pathT)) next;
    paths <- list.files(path=pathT, pattern="[.]Rcheck$", full.names=TRUE);
    filenameD <- sprintf("%s.log", resultT);
    pathnameD <- file.path(path, filenameD);
    pathnames <- file.path(paths, "00check.log");
    pathnames <- pathnames[file.exists(pathnames)];
    cat(file=pathnameD, append=FALSE);
    for (pp in seq(along=pathnames)) {
      pathname <- pathnames[pp];
      bfr <- readLines(pathname);

      indices <- c();
      for (rr in seq(along=results)) {
        result <- results[rr];
        idxs <- grep(result, bfr);
        if (length(idxs) == 0) next;
        idxs <- sapply(idxs, FUN=function(idx) {
          idx + seq(length=after[result])-1L;
        });
        idxs <- idxs[idxs <= length(bfr)];
        idxs <- sort(unique(idxs));
        n <- idxs[length(idxs)];
        if (bfr[n] == "for details.") {
          idxs <- idxs[1:(length(idxs)-3)];
        }
        indices <- c(indices, idxs);
        indices <- sort(unique(indices));
      } # for (rr ...)
      bfr <- bfr[indices];
      excl <- grep("... OK", bfr, fixed=TRUE);
      if (length(excl) > 0) bfr <- bfr[-excl];
      res <- sprintf("%s: %s", pathname, bfr);
      res <- c(res, "- - - - - - - - - - - - - - - - - -", "");
      res <- paste(res, collapse="\n");
      cat(res, file=pathnameD, append=TRUE);
    } # for (pp ...)
  } # for (kk ...)
} # rCmdCheckSummary()


############################################################################
# HISTORY:
# 2014-10-06
# o Now rCmdCheckSetup() no longer checks sizes.
# 2014-01-28
# o Now package with check status NOTE is separated into those with a
#   single NOTE and those with two or more NOTEs.
# 2014-01-05
# o Now URLs that packages wish to open are silently ignored.
# 2014-01-04
# o WORKAROUND: No longer using the 'inconsolata' font when building
#   the PDF manual on R (< 3.0.0).
# 2012-12-01
# o ROBUSTNESS: Added more validation to rCmdCheckSetup().
# 2012-10-23
# o Added argument 'deps' and 'type' to installPackages().
# 2012-10-19
# o Removed sortCheckDirs().
# 2012-10-15
# o Updated rCmdCheck() to also file by results.
# o Added sortCheckDirs().
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
