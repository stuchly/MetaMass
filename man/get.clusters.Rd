\name{get.clusters}
\alias{get.clusters}
\title{get.clusters}
\description{
 retrieves cluster data.frame from AnnoMass object
}
\usage{
get.clusters(AM,rID=1)
}
\arguments{
  \item{AM}{AnnoMass object. Result of function \code{analyze.MSfile}}
  \item{rID}{integer. Which annotation should be used to the comparison
    (see annotation.component argument in \code{analyze.MSfile})}
}

\value{
  data.frame. One line per each cluster with cluster info.

  main_component  - most abundant component in the cluster
  purity_main_component - ration of main_component in the cluster
  Nb_main_component - number of annotation suporting the most abundant component
  category - annotation of cluster with respect predefined scheme (see details)

  other columns - ratio of other components etc

}
\details{
  category scheme:
  Cytoplasm : (Cyt + CS (cytoskeleton)+proteasome ) >=51\%
  Subcategory: cytoskeleton if more than 30\% of markers in cytoplasm
  category is cytosleketon

  Ribosome (no subcategory) >=51\%

  Membrane: PM (Plasma membrane) + ER (endoplasmic
  reticulum)+Golgi+Mitochondrion  + lysosomes + ensodomes >=51\%
  Subcategory: Most dominant count.

  Nucleus: Nucleus + Nucleolus
  Subcategory: Nucleolus if more than 25\%  of nuclear markers are Nucleolus,
  }


\examples{
file1<-system.file("extdata","Bileck.txt",package="MetaMass")
file2<-system.file("extdata","Andreyev.txt",package="MetaMass")
file1
file2
res<-analyze.MSfile(MSfile=c(file1,file2),Metadata=c("Christoforou","Rodriguez"),markers=c(3,4,5,6,7))

head(get.clusters(res,rID=1)) #clusters annotation with respect to the 1st annotation.component (3rd column in AnnotationFile)
}
