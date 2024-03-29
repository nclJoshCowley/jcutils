% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/session-info.R
\name{session_info}
\alias{session_info}
\title{Output Session Information}
\usage{
session_info(git_repo, params = NULL)
}
\arguments{
\item{git_repo}{character. Path to be passed to \code{\link[git2r:repository_head]{git2r::repository_head()}}.}

\item{params}{print output of non-NULL object will be captured and appended.}
}
\description{
A \code{knitr}-friendly output of \code{\link[utils:sessionInfo]{utils::sessionInfo()}} with added git
information provided by \code{git2r}.
}
