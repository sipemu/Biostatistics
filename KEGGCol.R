#' Funktion: ColKEGGPathway
#' This function downloads the hsa (human) pathway from the website www.kepp.jp 
#' and color genes. The result is saved as a kgml-file. You can examine this
#' file e.g. with KEGGtranslator
#' 
#' @param Genes   A data.frame with columns:
#                   $names  : EntrezID of the Genes
#                   $reg    : values: "up" or "down"
#' @param KEGGID  String with KEGG ID of the pathway
#' @param pathout Directory for the result kgml-file
#' @param colup   html color for up-regulated genes
#' @param coldown html color for down-regulated genes
ColKEGGPathway <- function(Genes, KEGGID, pathout, colup="", coldown="") {
  
  library(XML)
  library(stringr)
  
  # Load KEGG-Pathway
  url <- paste("http://www.genome.jp/kegg-bin/download?entry=hsa", 
               KEGGID, 
               "&format=kgml", 
               sep="")
  temp <- tempfile()
  download.file(url, temp, method="internal")
  
  # Parse the pathway
  xmldata <- xmlTreeParse(temp)
  xmltop <- xmlRoot(xmldata)
  n <- sum(names(xmltop) == "entry")
  
  # colour the genes from our set
  ids <- list(NULL)
  k <- 1
  for (i in 1:n) {
    nodeName <- xmlAttrs (xmltop[[i]])[2]
    id <- unlist(sapply(Genes$names, function(x) which(str_detect(nodeName, x))))
    if (length(id) > 0) {
      ids[[k]] <- id
      k <- k + 1
      ifelse (Genes$reg == "up", 
              xmlAttrs (xmltop[[i]][[1]])[3] <- colup, 
              xmlAttrs (xmltop[[i]][[1]])[3] <- coldown)
    }
  }
  
  # save kgml-file
  saveXML(doc    = xmltop, 
          file   = paste(pathout, "hsa", KEGGID, ".xml", sep=""), 
          prefix = "<?xml version=\"1.0\"?>\n<!DOCTYPE pathway SYSTEM \"http://www.genome.jp/kegg/xml/KGML_v0.7.1_.dtd\">\n")
  
  return(ids)
}
