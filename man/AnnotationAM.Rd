\name{AnnotationAM}
\docType{data}
\alias{AnnotationAM}
\title{Annotation of proteins by cell fraction}
\description{
  Annotation of proteins collected from Metadata.
}
\usage{
AnnotationAM}
\format{
  data.frame

  [,1] protein ID (uniprot ID)

  [,2] protein ID (genename)

  [,3] annotations - Christoforou_UniprotGO_overlap3	Final localizations mapped in Christoforou et al (DOI: 10.1038/ncomms9992), plus proteins listed with one and the same location in Uniprot and GO

  [,4] annotations - Uni_GO_overlap4	Proteins listed with one and the same location in Uniprot AND GO

  [,5] annotations - Uniprot_GO_SUM5	Proteins listed with a single location in Uniprot OR GO

  [,6] annotations - HPA_Single_supportive6	Proteins listed with a single location in HPA, annotation classified as supportive

  [,7] annotations - HPA_Single_uncertain7	Proteins listed with a single location in HPA, annotation classified as uncertain

  [,8] annotations - Christoforou_UniprotGO_overlap CMN 	Same as default markers, but cytoskeleton is classified as cytosol and proteins in cytoplasmic organelles and membranes are classified as membrane, ribosomes are excluded

}