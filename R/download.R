getDownloadPath <- function() {
  # Get download path
  tags <- as.character(getRversion());
  path <- paste(c("downloads", tags), collapse=",");
  mkdirs(path);
  path;
} # getDownloadPath()


downloadPackages <- function(pkgs, repos=getRepos("CRAN"), ...) {
  cat("Downloading packages...\n");

  # Already downloaded once today?
  today <- Sys.Date();
  if (getOption("SubmitToCRAN/lastDownloaded", today-1) >= today) {
    cat("Already done today\n");
    path <- getDownloadPath();
    res <- listPackages(path);
    Package <- NULL; rm(Package); # To please R CMD check
    res <- subset(res, Package %in% pkgs);
    cat("Downloading packages...done\n");
    return(res);
  }

  # Available CRAN packages
  contriburl <- contrib.url(repos, "source");
  avail <- available.packages(contriburl=contriburl);
  avail <- avail[,c("Package", "Version")];
  avail <- as.data.frame(avail, stringsAsFactors=FALSE);
  avail <- subset(avail, Package %in% pkgs);

  # Identify packages to be downloaded
  filenames <- sprintf("%s_%s.tar.gz", avail$Package, avail$Version);
  path <- getDownloadPath();
  pathnames <- file.path(path, filenames);
  keep <- !file.exists(pathnames);
  missingPkgs <- avail$Package[keep];

  # Download missing packages
  if (length(missingPkgs) > 0) {
    download.packages(missingPkgs, repos=repos, type="source", destdir=path);
  }

  res <- listPackages(path);
  res <- subset(res, Package %in% pkgs);

  options("SubmitToCRAN/lastDownloaded"=today);

  cat("Downloading packages...done\n");

  res;
} # downloadPackages()


############################################################################
# HISTORY:
# 2012-07-12
# o Now downloadPackages() only returns the packages requested, regardless
#   of other packages in the download directory.
############################################################################
