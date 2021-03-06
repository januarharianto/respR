% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/import_file.R
\name{import_file}
\alias{import_file}
\title{Import respirometry system output files}
\usage{
import_file(path, export = FALSE)
}
\arguments{
\item{path}{string. Path to file.}

\item{export}{logical. If TRUE, exports the data to the same directory,
determined by the path parameter above.}
}
\value{
a data frame object of all columned data
}
\description{
Automatically read data from different respirometry hardware and software
systems. The aim is to work with most commercial DO sensors available in the
market with little input from the user. The function extracts data columns
from the file and generally cleans up column names to make the data easier to
work with.
}
\details{
Currently works for:
\itemize{
\item Firesting Logger
\item Pyro Oxygen Logger (another name for Firesting)
\item PreSens OXY10
\item PreSens OXY4
\item PreSens (generic, including multiplate systems)
\item PreSens/Loligo 24-Well Multiplate System (Excel files)
\item MiniDOT
\item Loligo AutoResp ('_raw' files output, not metadata files)
\item Loligo Witrox (same as AutoResp, without metadata)
\item Vernier (raw qmbl, csv, txt, (not yet gmbl))
\item Qbox Aqua (working on it)
}

We are always looking for sample files to improve the function. Please send
them to us via \href{mailto:nicholascarey@gmail.com}{email}, or via a \href{https://github.com/januarharianto/respR/issues}{Github issue}.

While the devices listed above are supported, the import functionality may
not be complete due to limited access to output files. This will improve over
time as users provide feedback. We are releasing this as it is, without any
warranty, so that some people can still benefit from the functionality as it
gets better. Users should be careful using this function, and be prepared to
to import data by themselves since it is a fundamental skill in R.
}
\examples{
NULL
}
