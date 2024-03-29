% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/download_cruncher.R
\encoding{UTF-8}
\name{configure_jwsacruncher}
\alias{configure_jwsacruncher}
\title{Configure the 'JWSACruncher' with a portable version of 'Java'}
\usage{
configure_jwsacruncher(jwsacruncher_path, java_path)
}
\arguments{
\item{jwsacruncher_path}{Path to the file \code{jwsacruncher.bat} of the 'JWSACruncher' (see details).}

\item{java_path}{Path to the file \code{java.exe} of the portable version of 'Java' (see details).}
}
\description{
Function configure the 'JWSACruncher' with a portable version of 'Java'.
}
\details{
Since the version 2.2.0, the 'JWSACruncher' needs 'Java' 8 or higher to run.
For versions 3.0.0 and higher, 'JWSACruncher' needs 'Java' 17 or higher.
In 'Windows' versions 3.0.0 and higher of 'JWSACruncher' includes a portable version of 'Java'.
For lower version of 'JWSACruncher', if you cannot install 'Java' (for example for security reasons) you can install a portable version of 'Java'
(that does not require administrator rights) and configure the 'JWSACruncher' to use this portable version.
To do it you have to:
\enumerate{
\item Unzip the downloaded file of the 'JWSACruncher';
\item Open, with a Text Editor, the file \code{jwsacruncher.bat} that is in the sub-folder \code{/bin/} of the 'JWSACruncher';
\item Edit the line 71 that contains \code{if "\%JAVACMD\%"=="" set JAVACMD=java} and replace \code{java}
by the path to the file \code{java.exe} of the portable version. For example, if the portable
version of 'Java' is installed under \code{D:/Software/Java}, the path to \code{java.exe} should be at
\code{D:/Software/Java/bin/java.exe} and the new line 71 would be
\code{if "\%JAVACMD\%"=="" set JAVACMD="D:\\Software\\Java\\bin\\java.exe"}.
}
The function \code{configure_jwsacruncher()} does the steps 2 and 3.
}
\seealso{
\code{\link[=download_cruncher]{download_cruncher()}}.
}
