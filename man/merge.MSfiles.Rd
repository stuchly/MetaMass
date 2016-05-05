\name{merge.MSfiles}
\alias{merge.MSfiles}
\title{merge.MSfiles}
\description{
 Merges data from several existing MSfiles, impute missing values by 1 and computes number of studies overlapped by the particular protein.
}
\usage{
merge.MSfiles(MSfiles,,by=1,all=TRUE,sep="\t")
}
\arguments{
  \item{MSfiles}{character vector; path(s) to the text files with MS data}
  \item{by}{positive integer. column index with protein identifier}
  \item{all}{logical; merge all proteins (if FALSE only proteins present in all files are merged}
  \item{sep}{character; cell separator in text files}

}

\value{
 invisible data.frame. One line per protein.

}
\details{one additional column is added - overlap - which says in how
  many studies the particular protein was detected. The protein is
  considered detected if if if the value in at least one fraction in the
study is greater than 1}

\examples{
files1<-system.file("extdata",c("Bileck.txt","Thakar.txt","Carvalho.txt","Andreyev.txt","Rodriguez.txt"),package="MetaMass")

res<-merge.MSfiles(files1,output="merged_files.txt")
}
