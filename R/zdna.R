zdna_base <- function(a){
  # replace spaces with nothing
  aSpace <- gsub("-", "", a)
  a <- aSpace
  if(nchar(a) < 12) return(data.frame("sequence_position" = "-", "sequence" = "-", "sequence_length" = "-", "likeliness" = "-"))


  # main strand
  a4 <- "((G[CTY]){6,})|(([AR][CTY]){6,})"
  a5 <- gregexpr(a4, a, ignore.case = TRUE, perl = T)
  sequence_position <- a5[[1]][1:length(a5[[1]])]
  if(a5[[1]][1] == -1){
    resultClean5 <- data.frame("sequence_position" = "-", "sequence" = "-", "sequence_length" = "-", "likeliness" = "-")
  }else{
    a8 <- regmatches(a, a5)
    sequence <- a8[[1]][1:length(a8[[1]])]
    sequence_length <- nchar(sequence)

    ####### Likeliness

    likeliness <- ""
    for(sequ in sequence){
      if((gregexpr("^(G[CTY]){6,}$", sequ, ignore.case = TRUE))[[1]][1] == 1){
        sequlikely <- "**"
      }else{
        sequlikely <- "*"
      }
      likeliness <- c(likeliness, sequlikely)
    }
    likeliness <- likeliness[2:length(likeliness)]
    a10 <- cbind(sequence_position, sequence, sequence_length, likeliness)
    resultClean5 <- data.frame(a10)
  }
  return(resultClean5)
}


zdna_main <- function(b){
  if(length(b) == 1){
    #remove newlines
    b <- gsub("[\r\n]", "", b)
    b <- gsub(" ", "", b)
    # exit if unacceptable characters exist
    if(grepl("[^acgtryswkmbdhvnACGTRYSWKMBDHVN-]", b) == "TRUE"){
      b1 <- data.frame("sequence_position" = "!", "sequence" = "Error: Non-DNA character(s) in input", "sequence_length" = "!", "likeliness" = "!")
      return(b1)
    }else{
      # else continue
      b1 <- zdna_base(b)
      return(b1)
    }

  }else{
    # exit if unacceptable characters exist
    input_pos = 0
    q <- data.frame("input_ID" = integer(0), "sequence_position" = character(0), "sequence" = character(0), "sequence_length" = character(0), "likeliness" = character(0))
    for(i in b){
      #remove newlines
      b <- gsub("[\r\n]", "", i)
      b <- gsub(" ", "", i)
      # exit if unacceptable characters exist
      if(grepl("[^acgtryswkmbdhvnACGTRYSWKMBDHVN-]", i) == "TRUE"){
        b1 <- data.frame("sequence_position" = "!", "sequence" = "Error: Non-DNA character(s) in input", "sequence_length" = "!", "likeliness" = "!")
      }else{
        b1 <- zdna_base(i)
      }
      input_pos = input_pos + 1
      b2 <- cbind(input_ID = input_pos, b1)
      b2[,c(2,4)] <- sapply(b2[,c(2,4)],as.character)
      q <- rbind(q, b2)
    }
    return(q)
  }
}


#' Predicting Z-DNA motif(s)
#'
#' This function predicts Z-DNA motif(s)
#' in 'x' in DNA. DNA sequence can be provided in raw or fasta format or as GenBank accession number(s).
#' Internet is needed to connect to GenBank database, if accession number(s) is given as argument.
#'
#' @param x DNA sequence(s) in raw format or a fasta file or a GenBank accession number(s); from which Z-DNA motif(s) will be predicted.
#'  If the fasta file name does not contain an absolute path, the file name is relative to the current working directory.
#' @param xformat a character string specifying the format of x : default (raw), fasta, GenBank (GenBank accession number(s)).
#' @return A dataframe of Z-DNA motif(s) position, sequence, length and likeliness. If more than one DNA sequence is provided as argument, an input ID is returned for motif(s) predicted from each input sequence.
#' @author Hannah O. Ajoge
#' @details
#' This function predicts Z-DNA motif(s) in DNA sequences and provide the position, sequence and length of the predicted motif(s). If any motif is predicted, the degree of likeliness for the motif to be formed is computed and scored as ** (more likely) or as * (less likely).
#' @export
#' @importFrom ape read.GenBank
#' @importFrom seqinr read.fasta
#' @importFrom seqinr getSequence
#' @references paper under review
#' @examples
#'  ## Predicting Z-DNA motif(s) from raw DNA sequences
#' E1 <- c("TCTTGCGCGCGCGCGCGCGCGCGCGCAAT", "taggtgctgggaggtagagacaggatatcct")
#' zdna(E1)
#'
#' ## Predicting Z-DNA motif(s) from DNA sequences in fasta file
#' ## Not run: zdna(x="Example.fasta", xformat = "fasta")
#'
#' ## Predicting Z-DNA motif(s) from DNA sequences,
#' ## using GenBank accession numbers.
#' ## Internet connectivity is needed for this to work.
#' ## Not run: zdna(c("BH114913", "AY611035"), xformat = "GenBank")



zdna <- function(x, xformat = "default"){
  if(xformat == "default"){
    x1 <- zdna_main(x)
    return(x1)
  }

  if(xformat == "GenBank"){
    x2 <- read.GenBank(x, as.character = TRUE)
    x3 <- sapply(x2, paste, collapse="")
    x4 <- zdna_main(x3)
    return(x4)
  }

  if(xformat == "fasta"){
    x5 <-read.fasta(x)
    x6 <- getSequence(x5, as.string = TRUE)
    x7 <- unlist(x6)
    x8 <- zdna_main(x7)
    return(x8)
  }else{
    stop("Unacceptable option for argument 'xformat'")
  }

}
