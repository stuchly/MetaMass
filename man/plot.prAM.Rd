\name{plot.prAM}
\alias{plot.prAM}
\title{plot.prAM}
\description{
 creates ROC curve of annotated clusters with respect to all used annoations.
}
\usage{
plot.prAM(AM,abs=FALSE,legend.position="bottomleft")
}
\arguments{
  \item{AM}{AnnoMass object. Result of function analyze.MSfile}
  \item{abs}{logical; plot number of assigned proteins instead of
    recall}
  \item{legend.position}{character; see details in ?legend}
}
\details{This function evaluates clustering performance via
  precision-recall curves with respect to the
  annotation}

\examples{
file1<-system.file("extdata","Data_Fig_1a.txt",package="MetaMass")
file1

##proteins identified by genename -> annotation.ID=2 (see ?AnnotationAM)
##cluster with respect metadata only (group=0)
res1<-analyze.MSfile(MSfile=file1,Metadata=c("Christoforou"),output="res1",markers=c(3:5))
par(mfrow=c(5,3),mar=c(1, 4, 2.2, 1) + 0.1)
plot.prAM(res1) #roc curve with respect to the 1st annotation.fraction (3rd column in AnnotationFile)

}
