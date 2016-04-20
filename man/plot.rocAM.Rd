\name{plot.rocAM}
\alias{plot.rocAM}
\title{plot.rocAM}
\description{
 creates ROC curve of annotated clusters with respect to all used annoations.
}
\usage{
plot.rocAM(AM)
}
\arguments{
  \item{AM}{AnnoMass object. Result of function analyze.MSfile}

}
\details{This ROC evaluates clustering performance with respect to the
  annotation - the ration of all annotated proteins with respect to the
  cluster purity is plotted.}

\examples{
file1<-system.file("extdata","Data_Fig_1a.txt",package="MetaMass")
file1

##proteins identified by genename -> annotation.ID=2 (see ?AnnotationAM)
##cluster with respect metadata only (group=0)
res1<-analyze.MSfile(MSfile=file1,Metadata=c("Christoforou"),output="res1",markers=c(3:5))
par(mfrow=c(5,3),mar=c(1, 4, 2.2, 1) + 0.1)
plot.rocAM(res1) #roc curve with respect to the 1st annotation.fraction (3rd column in AnnotationFile)

}
