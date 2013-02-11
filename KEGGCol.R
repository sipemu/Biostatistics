ColKEGGPathway <- function(Genes, KEGGID, pathout, colup="", coldown="") {
  # ColKEGGPathway:
  # Die Funktion lädt von der Seite www.kepp.jp den hsa (human) Pathway
  # herunter und entsprechend der eigenen Gene eingefärbt und anschließend
  # lokal als kgml-Datei abgespeichert. Betrachten kann man die kgml-Dateien
  # beispielsweise mit KEGGtranslator
  #
  # Eingabe:
  #   Genes:    data.frame:
  #               $names  : die EntrezID der Gene
  #               $reg    : "up" oder "down"; hoch- oder runterexprimiert
  #   KEGGID:   String mit der KEGG ID des Pathways
  #   pathout:  Speicherort für die kgml-Datei
  #   colup, 
  #   coldown:  html Farbangaben
  #
  # Ausgabe:
  #   ids:  mehrere Gene können einem Knoten in dem Pathway zugeordnet werden,
  #         diese werden hier mit ausgegeben
  
  
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



