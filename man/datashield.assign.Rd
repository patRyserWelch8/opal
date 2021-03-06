% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/datashield.r
\name{datashield.assign}
\alias{datashield.assign}
\alias{datashield.assign.opal}
\alias{datashield.assign.list}
\title{Data assignment}
\usage{
datashield.assign(opal, symbol, value, variables = NULL,
  missings = FALSE, identifiers = NULL, async = TRUE, wait = TRUE,
  tibble = FALSE)

\method{datashield.assign}{opal}(opal, symbol, value, variables = NULL,
  missings = FALSE, identifiers = NULL, async = TRUE, wait = TRUE,
  tibble = FALSE)

\method{datashield.assign}{list}(opal, symbol, value, variables = NULL,
  missings = FALSE, identifiers = NULL, async = TRUE, wait = TRUE,
  tibble = FALSE)
}
\arguments{
\item{opal}{Opal object or list of opal objects.}

\item{symbol}{Name of the R symbol.}

\item{value}{Fully qualified name of a variable or a table in Opal (must be the same in each Opal) or a R expression with allowed assign functions calls.}

\item{variables}{List of variable names or Javascript expression that selects the variables of a table (ignored if value does not refere to a table). See javascript documentation: http://wiki.obiba.org/display/OPALDOC/Variable+Methods}

\item{missings}{If TRUE, missing values will be pushed from Opal to R, default is FALSE. Ignored if value is an R expression.}

\item{identifiers}{Name of the identifiers mapping to use when assigning entities to R (from Opal 2.0).}

\item{async}{R script is executed asynchronously within the session (default is TRUE).}

\item{wait}{Wait for the R script asynchronously executed to complete (makes sense only with async=TRUE).}

\item{tibble}{Assign table to a tibble (from tidyverse) instead of a plain data.frame.}
}
\value{
The R command ID if the async flag is TRUE and if the wait flag is FALSE and if Opal version is at least 2.1, NULL otherwise.
}
\description{
Assign a Opal value to a R symbol in the current Datashield session.
}
\examples{
\dontrun{
# assign a list of variables from table HOP of opal object o
datashield.assign(o, symbol="D", value="demo.HOP", variables=list("GENDER","LAB_GLUC"))

# assign all the variables matching 'LAB' from table HOP of opal object o
datashield.assign(o, symbol="D", value="demo.HOP", variables="name().matches('LAB_')")
}
}
