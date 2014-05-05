getReposNames <- function() {
  repositories <- c(CRAN="CRAN", CRANextras="CRANextras", BioCsoft="BioCsoft", BioCann="BioCann", BioCexp="BioCexp", Omegahat="Omegahat", "R-Forge"="R-Forge");
  repositories;
} # getReposNames()


getRepos <- function(which, ...) {
  runningRDevel <- function(...) {
    (R.version$status == "Under development (unstable)");
  } # runningRDevel()

  repos <- getOption("repos");
  keys <- names(repos);
  if (is.element(which, keys)) {
    return(repos[which]);
  }

  repos <- NA;
  if (which == "CRAN") {
    repos <- "http://cran.r-project.org";
  } else if (which == "CRANextras") {
    repos <- "http://www.stats.ox.ac.uk/pub/RWin";
  } else if (which == "R-Forge") {
    repos <- "http://R-Forge.R-project.org";
  } else if (which == "Omegahat") {
    repos <- "http://www.omegahat.org/R";
  } else if (which == "BioCsoft") {
    if (runningRDevel()) {
      repos <- "http://www.bioconductor.org/packages/devel/bioc";
    } else {
      repos <- "http://www.bioconductor.org/packages/release/bioc";
    }
  } else if (which == "BioCann") {
    if (runningRDevel()) {
      repos <- "http://www.bioconductor.org/packages/devel/data/annotation";
    } else {
      repos <- "http://www.bioconductor.org/packages/release/data/annotation";
    }
  } else if (which == "BioCexp") {
    if (runningRDevel()) {
      repos <- "http://www.bioconductor.org/packages/devel/data/experiment";
    } else {
      repos <- "http://www.bioconductor.org/packages/release/data/experiment";
    }
  }

  repos;
} # getRepos()


############################################################################
# HISTORY:
# 2012-10-30
# o Now getRepos() no longer hard code what is R/BioC devel.
############################################################################
