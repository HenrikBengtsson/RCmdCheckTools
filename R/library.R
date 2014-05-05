getLibUserPath <- function(...) {
  olibs <- as.list(Sys.getenv(c("R_LIBS_USER", "R_LIBS")));
  on.exit({
    do.call("Sys.setenv", args=olibs);
  });
  Sys.setenv("R_LIBS_USER"="", "R_LIBS"="");
  tmpfile <- tempfile(fileext=".R");
  on.exit({
    if (file.exists(tmpfile)) file.remove(tmpfile);
  });
  cat("cat(.libPaths()[1])", file=tmpfile);
  args <- c("--vanilla", "--slave", "-f", tmpfile);
  userPath <- system2(R.bin(), args=args, stdout=TRUE);

  # Drop: "WARNING: ignoring environment value of R_HOME"
  userPath <- grep("^WARNING: ", userPath, invert=TRUE, value=TRUE);

  userPath;
} # getLibUserPath()


getLibPath <- function(which, userPath=getLibUserPath(), ...) {
  defaultPath <- .Library;

  if (which == "user") {
    path <- userPath;
  } else if (which == "default") {
    path <- defaultPath;
  } else if (which == "test") {
    path <- sprintf("%s_%s", userPath, which);
    path <- file.path(getwd(), "R", basename(path));
  } else {
    path <- sprintf("%s_%s", userPath, which);
  }
  names(path) <- which;

  # Create, iff missing
  mkdirs(path);

  path;
} # getLibPath()


getLibPaths <- function(which, userPath=getLibUserPath(), ..., collapse=FALSE) {
  res <- sapply(which, FUN=getLibPath, userPath=userPath, ...);

  # Append library paths to stable version as a backup for non-available packages?
  if (regexpr("Under development", R.version$status) != -1L) {
    rVer <- basename(userPath);
    rVer <- as.numeric(rVer);
    rVerS <- sprintf("%.1f", rVer - 0.1);
    userPathS <- file.path(dirname(userPath), rVerS);
    whichS <- setdiff(which, "test");
    resS <- sapply(whichS, FUN=getLibPath, userPath=userPathS, ...);
    names(resS) <- sprintf("%s,stable", names(resS));
    res <- c(res, resS);
  }

  if (collapse) {
    sep <- getRLIBSSep();
    res <- paste(res, collapse=sep);
  }

  res;
} # getLibPaths()


useLibPaths <- function(which=c("test", getReposNames(), "default"), ...) {
  libs <- getLibPaths(which);
  .libPaths(libs);
  .libPaths();
} # useLibPaths()



############################################################################
# HISTORY:
# 2013-03-10
# o Now getLibPaths() will append library paths to the stable release
#   if current version of R is the developers version.
# o Added getLibUserPath().
# 2013-01-14
# o Now getLibPath() used underscore and not commas to generate paths.
# o Now getLibPath() ignores WARNING messages during R startup.
# 2012-07-04
# o Added useLibPaths().
############################################################################
