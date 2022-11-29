gquad_overlap_base <- function(a){
  # replace spaces with nothing
  aSpace <- gsub("-", "", a)
  a <- aSpace
  if(nchar(a) < 11) return(data.frame("sequence_position" = "-", "sequence" = "-", "sequence_length" = "-", "likeliness" = "-"))


  # main
  a4 <- "(?=(GG\\w{7,50}GG))"
  a5 <- gregexpr(a4, a, ignore.case = TRUE, perl = TRUE)
  if(a5[[1]][1] == -1){
    resultClean5 <- data.frame("sequence_position" = "-", "sequence" = "-", "sequence_length" = "-", "likeliness" = "-")
    return(resultClean5)
  }else{
    sequence_position <- a5[[1]][1:length(a5[[1]])]
    sequence_length <- as.vector(attr(a5[[1]], "capture.length"))
    a_end = sequence_position + sequence_length - 1
    sequence <- substring(a, sequence_position, a_end)
    b4 <- "(?=(G{2,7}[ACGTURYSWKMBDHVN]{1,40}G{2,7}[ACGTURYSWKMBDHVN]{1,40}G{2,7}[ACGTURYSWKMBDHVN]{1,40}G{2,7}))"
    c4 <- "G{2,7}([ACGTURYSWKMBDHVN]{1,40}?)G{2,7}([ACGTURYSWKMBDHVN]{1,40}?)G{2,7}([ACGTURYSWKMBDHVN]{1,40}?)G{2,7}"
    sequenceCount <- 0
    sequenceAll <- ""
    sequenceAll2 <- ""
    sequence_positionAll <- ""
    sequence_positionAll2 <- ""
    for (x in sequence){
      sequenceCount <- sequenceCount + 1
      b5 <- gregexpr(b4, sequence[sequenceCount], ignore.case = TRUE, perl = TRUE)
      sequence_positionb <- b5[[1]][1:length(b5[[1]])]
      sequence_positionReal <- sequence_position[sequenceCount] + sequence_positionb - 1
      sequence_lengthb <- as.vector(attr(b5[[1]], "capture.length"))
      b_end = sequence_positionReal + sequence_lengthb - 1
      sequenceb <- substring(a, sequence_positionReal, b_end)
      sequenceAll <- c(sequenceAll, sequenceb)
      sequence_positionAll <- c(sequence_positionAll, sequence_positionReal)

      c5 <- gregexpr(c4, sequence[sequenceCount], ignore.case = TRUE, perl = TRUE)
      sequence_positionc <- c5[[1]][1:length(c5[[1]])]
      sequence_positionReal2 <- sequence_position[sequenceCount] + sequence_positionc - 1
      c6 <- regmatches(sequence[sequenceCount], c5)
      sequencec <- c6[[1]][1:length(c6[[1]])]
      sequence_lengthc <- nchar(sequencec)
      c_end = sequence_positionReal2 + sequence_lengthc - 1
      sequenceAll2 <- c(sequenceAll2, sequencec)
      sequence_positionAll2 <- c(sequence_positionAll2, sequence_positionReal2)

    }
    sequenceLengthAll <- nchar(sequenceAll)
    resultOnway <- unique(data.frame(sequence_positionAll, sequenceAll, sequenceLengthAll))
    resultClean1 <- subset(resultOnway,sequence_positionAll!="")
    resultClean2 <- subset(resultClean1,sequenceAll!="")

    sequenceLengthAll2 <- nchar(sequenceAll2)
    resultOnway2 <- unique(data.frame(sequence_positionAll2, sequenceAll2, sequenceLengthAll2))
    resultClean1b <- subset(resultOnway2,sequence_positionAll2!="")
    resultClean2b <- subset(resultClean1b,sequenceAll2!="")


    if(nrow(resultClean2) > 0){
      colnames(resultClean2) <- c("sequence_position", "sequence", "sequence_length")
    }else{
      resultClean2 <- data.frame("sequence_position" = "-", "sequence" = "-", "sequence_length" = "-")
    }
    if(nrow(resultClean2b) > 0){
      colnames(resultClean2b) <- c("sequence_position", "sequence", "sequence_length")
    }else{
      resultClean2b <- data.frame("sequence_position" = "-", "sequence" = "-", "sequence_length" = "-")
    }
    resultClean3 <- rbind(resultClean2, resultClean2b)
    resultClean3 <- unique(resultClean3)
    if(nrow(resultClean3) > 1){
      resultClean4 <- resultClean3[!(resultClean3$sequence == "-"),]
    }else{
      resultClean4 <- resultClean3
    }
    if(nrow(resultClean4[!(resultClean4$sequence == "-"),]) > 0){
      resultClean5 <- resultClean4[order(as.integer(as.character(resultClean4[,1])), -as.integer(as.character(resultClean4[,3]))),]
      rownames(resultClean5) <- 1:nrow(resultClean5)
    }else{
      resultClean5 <- data.frame("sequence_position" = "-", "sequence" = "-", "sequence_length" = "-")
    }

    ####### Likeliness

    likeliness <- ""
    for(sequ in resultClean5$sequence){
      if(sequ == "-"){
        sequlikely <- "-"
      }else{
        if((gregexpr("^G{3,7}\\w{1,7}G{3,7}\\w{1,7}G{3,7}\\w{1,7}G{3,7}$", sequ, ignore.case = TRUE))[[1]][1] == 1){
          sequlikely <- "**"
        }else{
          sequlikely <- "*"
        }
      }
      likeliness <- c(likeliness, sequlikely)
    }
    likeliness <- likeliness[2:length(likeliness)]
    resultClean5$likeliness <- likeliness

    return(resultClean5)
  }

}


gquadOverlap_main <- function(b){
  if(length(b) == 1){
    #remove newlines
    b <- gsub("[\r\n]", "", b)
    b <- gsub(" ", "", b)
    # exit if unacceptable characters exist
    if(grepl("[^acgturyswkmbdhvnACGTURYSWKMBDHVN-]", b) == "TRUE"){
      b1 <- data.frame("sequence_position" = "!", "sequence" = "Error: Non-nucleotide character(s) in input", "sequence_length" = "!", "likeliness" = "!")
      return(b1)
    }else{
      # else continue
      b1 <- gquad_overlap_base(b)
      return(b1)
    }

  }else{
    # exit if unacceptable characters exist
    input_pos = 0
    q <- data.frame("input_ID" = integer(0), "sequence_position" = character(0), "sequence" = character(0), "sequence_length" = character(0))
    for(i in b){
      #remove newlines
      b <- gsub("[\r\n]", "", i)
      b <- gsub(" ", "", i)
      # exit if unacceptable characters exist
      if(grepl("[^acgturyswkmbdhvnACGTURYSWKMBDHVN-]", i) == "TRUE"){
        b1 <- data.frame("sequence_position" = "!", "sequence" = "Error: Non-nucleotide character(s) in input", "sequence_length" = "!", "likeliness" = "!")
      }else{
        b1 <- gquad_overlap_base(i)
      }
      input_pos = input_pos + 1
      b2 <- cbind(input_ID = input_pos, b1)
      b2[,c(2,4)] <- sapply(b2[,c(2,4)],as.character)
      q <- rbind(q, b2)
    }
    return(q)
  }
}


#' Predicting G quadruplexes including overlaps
#'
#' This function predicts G quadruplexes
#' in 'x' (nucleotide sequence(s)) like the gquad function, but includes overlaps.
#' Nucleotide sequence can be provided in raw or fasta format or as GenBank accession number(s).
#' Internet is needed to connect to GenBank database, if accession number(s) is given as argument.
#'
#' @param x nucleotide sequence(s) in raw format or a fasta file or a GenBank accession number(s); from which G quadruplexes (including overlaps) will be predicted.
#'  If the fasta file name does not contain an absolute path, the file name is relative to the current working directory.
#' @param xformat a character string specifying the format of x : default (raw), fasta, GenBank (GenBank accession number(s)).
#' @return A dataframe of G quadruplexes' position, sequence, length and likeliness. If more than one nucleotide sequence is provided as argument, an input ID is returned for motif(s) predicted from each input sequence.
#' @author Hannah O. Ajoge
#' @details
#' This function predicts G quadruplexes in nucleic (both DNA and RNA) sequences, including overlaps and provide the position, sequence and length of the predicted motif(s). If any motif is predicted, the degree of likeliness for the motif to be formed is computed and scored as ** (more likely) or as * (less likely).
#' @export
#' @importFrom ape read.GenBank
#' @importFrom seqinr read.fasta
#' @importFrom seqinr getSequence
#' @references Paper on gquad and the web application (Non-B DNA Predictor) is under review, see draft in vignettes
#' @seealso gquad
#' @examples
#' ## Predicting G quadruplexes (including overlaps) from raw nucleotide sequences
#' E1 <- c("TCTTGGGCATCTGGAGGCCGGAAT", "taggtgctgggaggtagagacaggatatcct")
#' gquadO(E1)
#'
#' ## Predicting G quadruplexes (including overlaps) from nucleotide sequences in fasta file
#' ## Not run: gquadO(x="Example.fasta", xformat = "fasta")
#'
#' ## Predicting G quadruplexes (including overlaps) from nucleotide sequences,
#' ## using GenBank accession numbers.
#' ## Internet connectivity is needed for this to work.
#' ## Not run: gquadO(c("BH114913", "AY611035"), xformat = "GenBank")

gquadO <- function(x, xformat = "default"){
  if(xformat == "default"){
    x1 <- gquadOverlap_main(x)
    return(x1)
  }

  if(xformat == "GenBank"){
    x2 <- read.GenBank(x, as.character = TRUE)
    x3 <- sapply(x2, paste, collapse="")
    x4 <- gquadOverlap_main(x3)
    return(x4)
  }

  if(xformat == "fasta"){
    x5 <-read.fasta(x)
    x6 <- getSequence(x5, as.string = TRUE)
    x7 <- unlist(x6)
    x8 <- gquadOverlap_main(x7)
    return(x8)
  }else{
    stop("Unacceptable option for argument 'xformat'")
  }

}
