\name{pr_tables}
\alias{pr_tables}
\title{pr_tables}
\description{
 exports the  points to plot precision-recall curves and
 number_assigned_proteins agains precision in convenient form of 2 data.frames. see ?plot.prAM
}
\usage{
pr_tables(AM,output)
}
\arguments{
  \item{AM}{AnnoMass object. Result of function analyze.MSfile}
  \item{ouput}{character; saves the tables in 2 files - recall_output and assigned_proteins_output}
}
\details{}
\value{invisible list of data.frames}
\examples{
file1<-system.file("extdata","Data_Fig_1a.txt",package="MetaMass")
file1

res1<-analyze.MSfile(MSfile=file1,Metadata=c("Christoforou"),output="res1",markers=c(3:5))
pr_tables(res1,output="prtables.txt")

}
