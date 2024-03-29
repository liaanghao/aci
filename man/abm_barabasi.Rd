% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/abm_barabasi.R
\name{abm_barabasi}
\alias{abm_barabasi}
\title{Network simulation: tie formation based on a revised barabasi model.}
\usage{
abm_barabasi(
  mat.init,
  belief = 0,
  spatial.coord = NULL,
  target.outdegree = NULL,
  target.density = NULL,
  trial.limit = NULL,
  max.time = 10000,
  intercept = 1,
  homoph = 0,
  recip = 0,
  popul = 0,
  activ = 0,
  transiv = 0,
  common.target = 0,
  common.source = 0,
  proximity = 0,
  display.time = FALSE,
  seed = NULL,
  sparseM.output = FALSE
)
}
\arguments{
\item{mat.init}{Initial matrix.}

\item{belief}{Belief scores.}

\item{spatial.coord}{Spatial coordination.}

\item{target.outdegree}{The target outdegree.}

\item{target.density}{The target density.}

\item{trial.limit}{The upper limits of simulation.}

\item{max.time}{The maximum time for simulation.}

\item{intercept}{Intercept for calculating preference scores.}

\item{homoph}{Homophily beta.}

\item{recip}{Reciprocity beta.}

\item{popul}{Popularity beta.}

\item{activ}{Activity beta.}

\item{transiv}{Transitivity beta.}

\item{common.target}{Common target beta.}

\item{common.source}{Common source beta.}

\item{proximity}{Proximity beta.}

\item{display.time}{A logic for display time. Default is *FALSE*.}

\item{seed}{Set seed for simulation.}

\item{sparseM.output}{A logic for export sparse matrix. Default is *FALSE*.}
}
\description{
Network simulation: tie formation based on a revised barabasi model.
}
