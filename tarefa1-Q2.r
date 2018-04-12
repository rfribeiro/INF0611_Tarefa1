#definindo o conteudo dos documentos
IDdocs <- c('D1', 'D2', 'D3', 'D4', 'D5')
conteudo <- c('A A B A C D E E D', 'C A E A F', 'D E A D', 'B A D B A E', 'D A D C A B')
Documentos <- data.frame(id = IDdocs, texto = conteudo)

Dict <- c('A', 'B', 'C', 'D', 'E', 'F')
Query <- c('C A B E')

#determinar a representacao vetorial
idf <- function(letter) {
  ndocs <- length(IDdocs)
  count <- 0
  
  for (doc in Documentos$texto) {
    count <- count + letter %in% strsplit(as.character(doc), " ")[[1]]
  }
  
  idft <- log(ndocs/count)
  
  idft
}


tf <- function(doc, letter) {
  documento <- strsplit(as.character(doc), " ")
  count <- sum(is.element(documento[[1]], letter))
  
  count
}


repVet <- function(doc, dict) {
  vect <- c()
  for (letter in dict) {
    calcTF <- tf(doc, letter) 
    calcIDF <- idf(letter)
    
    rep <- calcIDF * calcTF
    
    vect <- c(vect, rep)
  }
  vect
}

# O resultado da chamada abaixo me provê o resultado de cada representção vertical
# nas colunas
representacoes <- sapply(Documentos$texto, repVet, Dict)
colnames(representacoes) <- IDdocs
rownames(representacoes) <- Dict

queryRep <- repVet(Query, Dict)

# Devem ser passadas as representacoes vetoriais do documento e da query
similaridade <- function (doc, query) {
  if (length(doc) != length(query)) {
    return(0)
  }
  dividendo <- 0
  normaDoc <- 0
  normaQuery <- 0
  for (i in 1:length(doc)) {
    dividendo <- dividendo + doc[i]*query[i]
    
    normaDoc <- normaDoc + doc[i]^2
    normaQuery <- normaQuery + query[i]^2
  }
  
  normaDoc <- sqrt(normaDoc)
  normaQuery <- sqrt(normaQuery)
  
  return (dividendo/(normaDoc*normaQuery))
}

similaridades <- apply(representacoes, 2, similaridade, queryRep)

IDdocs[order(similaridades, decreasing = TRUE)[1]]
