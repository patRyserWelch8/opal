% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/opal.symbol.r
\name{opal.symbol_import}
\alias{opal.symbol_import}
\title{Import a tibble as a table in Opal}
\usage{
opal.symbol_import(opal, symbol, project, identifiers = NULL,
  policy = "required", id.name = "id", type = "Participant",
  wait = TRUE)
}
\arguments{
\item{opal}{Opal object.}

\item{symbol}{Name of the R symbol representing a tibble.}

\item{project}{Name of the project into which the data are to be imported.}

\item{identifiers}{Name of the identifiers mapping to use when assigning entities to Opal.}

\item{policy}{Identifiers policy: 'required' (each identifiers must be mapped prior importation (default)), 'ignore' (ignore unknown identifiers) and 'generate' (generate a system identifier for each unknown identifier).}

\item{id.name}{The name of the column representing the entity identifiers. Default is 'id'.}

\item{type}{Entity type (what the data are about). Default is 'Participant'.}

\item{wait}{Wait for import task completion. Default is TRUE.}
}
\description{
Import the tibble identified by the symbol as a table in Opal.
}
