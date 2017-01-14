hdna_base <- function(a){
  # replace spaces with nothing
  aSpace <- gsub("-", "", a)
  a <- aSpace

  # main strand

  a25 <- "([AGR])([AGR])([AGR])([AGR])([AGR])([AGR])([AGR])([AGR])([AGR])([AGR])([AGR])([AGR])([AGR])([AGR])([AGR])([AGR])([AGR])([AGR])([AGR])([AGR])([AGR])([AGR])([AGR])([AGR])([AGR])\\w{0,500}\\25\\24\\23\\22\\21\\20\\19\\18\\17\\16\\15\\14\\13\\12\\11\\10\\9\\8\\7\\6\\5\\4\\3\\2\\1"
  a24 <- "([AGR])([AGR])([AGR])([AGR])([AGR])([AGR])([AGR])([AGR])([AGR])([AGR])([AGR])([AGR])([AGR])([AGR])([AGR])([AGR])([AGR])([AGR])([AGR])([AGR])([AGR])([AGR])([AGR])([AGR])\\w{0,500}\\24\\23\\22\\21\\20\\19\\18\\17\\16\\15\\14\\13\\12\\11\\10\\9\\8\\7\\6\\5\\4\\3\\2\\1"
  a23 <- "([AGR])([AGR])([AGR])([AGR])([AGR])([AGR])([AGR])([AGR])([AGR])([AGR])([AGR])([AGR])([AGR])([AGR])([AGR])([AGR])([AGR])([AGR])([AGR])([AGR])([AGR])([AGR])([AGR])\\w{0,500}\\23\\22\\21\\20\\19\\18\\17\\16\\15\\14\\13\\12\\11\\10\\9\\8\\7\\6\\5\\4\\3\\2\\1"
  a22 <- "([AGR])([AGR])([AGR])([AGR])([AGR])([AGR])([AGR])([AGR])([AGR])([AGR])([AGR])([AGR])([AGR])([AGR])([AGR])([AGR])([AGR])([AGR])([AGR])([AGR])([AGR])([AGR])\\w{0,500}\\22\\21\\20\\19\\18\\17\\16\\15\\14\\13\\12\\11\\10\\9\\8\\7\\6\\5\\4\\3\\2\\1"
  a21 <- "([AGR])([AGR])([AGR])([AGR])([AGR])([AGR])([AGR])([AGR])([AGR])([AGR])([AGR])([AGR])([AGR])([AGR])([AGR])([AGR])([AGR])([AGR])([AGR])([AGR])([AGR])\\w{0,500}\\21\\20\\19\\18\\17\\16\\15\\14\\13\\12\\11\\10\\9\\8\\7\\6\\5\\4\\3\\2\\1"
  a20 <- "([AGR])([AGR])([AGR])([AGR])([AGR])([AGR])([AGR])([AGR])([AGR])([AGR])([AGR])([AGR])([AGR])([AGR])([AGR])([AGR])([AGR])([AGR])([AGR])([AGR])\\w{0,500}\\20\\19\\18\\17\\16\\15\\14\\13\\12\\11\\10\\9\\8\\7\\6\\5\\4\\3\\2\\1"
  a19 <- "([AGR])([AGR])([AGR])([AGR])([AGR])([AGR])([AGR])([AGR])([AGR])([AGR])([AGR])([AGR])([AGR])([AGR])([AGR])([AGR])([AGR])([AGR])([AGR])\\w{0,500}\\19\\18\\17\\16\\15\\14\\13\\12\\11\\10\\9\\8\\7\\6\\5\\4\\3\\2\\1"
  a18 <- "([AGR])([AGR])([AGR])([AGR])([AGR])([AGR])([AGR])([AGR])([AGR])([AGR])([AGR])([AGR])([AGR])([AGR])([AGR])([AGR])([AGR])([AGR])\\w{0,500}\\18\\17\\16\\15\\14\\13\\12\\11\\10\\9\\8\\7\\6\\5\\4\\3\\2\\1"
  a17 <- "([AGR])([AGR])([AGR])([AGR])([AGR])([AGR])([AGR])([AGR])([AGR])([AGR])([AGR])([AGR])([AGR])([AGR])([AGR])([AGR])([AGR])\\w{0,500}\\17\\16\\15\\14\\13\\12\\11\\10\\9\\8\\7\\6\\5\\4\\3\\2\\1"
  a16 <- "([AGR])([AGR])([AGR])([AGR])([AGR])([AGR])([AGR])([AGR])([AGR])([AGR])([AGR])([AGR])([AGR])([AGR])([AGR])([AGR])\\w{0,500}\\16\\15\\14\\13\\12\\11\\10\\9\\8\\7\\6\\5\\4\\3\\2\\1"
  a15 <- "([AGR])([AGR])([AGR])([AGR])([AGR])([AGR])([AGR])([AGR])([AGR])([AGR])([AGR])([AGR])([AGR])([AGR])([AGR])\\w{0,500}\\15\\14\\13\\12\\11\\10\\9\\8\\7\\6\\5\\4\\3\\2\\1"
  a45 <- "([CTY])([CTY])([CTY])([CTY])([CTY])([CTY])([CTY])([CTY])([CTY])([CTY])([CTY])([CTY])([CTY])([CTY])([CTY])([CTY])([CTY])([CTY])([CTY])([CTY])([CTY])([CTY])([CTY])([CTY])([CTY])\\w{0,500}\\25\\24\\23\\22\\21\\20\\19\\18\\17\\16\\15\\14\\13\\12\\11\\10\\9\\8\\7\\6\\5\\4\\3\\2\\1"
  a44 <- "([CTY])([CTY])([CTY])([CTY])([CTY])([CTY])([CTY])([CTY])([CTY])([CTY])([CTY])([CTY])([CTY])([CTY])([CTY])([CTY])([CTY])([CTY])([CTY])([CTY])([CTY])([CTY])([CTY])([CTY])\\w{0,500}\\24\\23\\22\\21\\20\\19\\18\\17\\16\\15\\14\\13\\12\\11\\10\\9\\8\\7\\6\\5\\4\\3\\2\\1"
  a43 <- "([CTY])([CTY])([CTY])([CTY])([CTY])([CTY])([CTY])([CTY])([CTY])([CTY])([CTY])([CTY])([CTY])([CTY])([CTY])([CTY])([CTY])([CTY])([CTY])([CTY])([CTY])([CTY])([CTY])\\w{0,500}\\23\\22\\21\\20\\19\\18\\17\\16\\15\\14\\13\\12\\11\\10\\9\\8\\7\\6\\5\\4\\3\\2\\1"
  a42 <- "([CTY])([CTY])([CTY])([CTY])([CTY])([CTY])([CTY])([CTY])([CTY])([CTY])([CTY])([CTY])([CTY])([CTY])([CTY])([CTY])([CTY])([CTY])([CTY])([CTY])([CTY])([CTY])\\w{0,500}\\22\\21\\20\\19\\18\\17\\16\\15\\14\\13\\12\\11\\10\\9\\8\\7\\6\\5\\4\\3\\2\\1"
  a41 <- "([CTY])([CTY])([CTY])([CTY])([CTY])([CTY])([CTY])([CTY])([CTY])([CTY])([CTY])([CTY])([CTY])([CTY])([CTY])([CTY])([CTY])([CTY])([CTY])([CTY])([CTY])\\w{0,500}\\21\\20\\19\\18\\17\\16\\15\\14\\13\\12\\11\\10\\9\\8\\7\\6\\5\\4\\3\\2\\1"
  a40 <- "([CTY])([CTY])([CTY])([CTY])([CTY])([CTY])([CTY])([CTY])([CTY])([CTY])([CTY])([CTY])([CTY])([CTY])([CTY])([CTY])([CTY])([CTY])([CTY])([CTY])\\w{0,500}\\20\\19\\18\\17\\16\\15\\14\\13\\12\\11\\10\\9\\8\\7\\6\\5\\4\\3\\2\\1"
  a39 <- "([CTY])([CTY])([CTY])([CTY])([CTY])([CTY])([CTY])([CTY])([CTY])([CTY])([CTY])([CTY])([CTY])([CTY])([CTY])([CTY])([CTY])([CTY])([CTY])\\w{0,500}\\19\\18\\17\\16\\15\\14\\13\\12\\11\\10\\9\\8\\7\\6\\5\\4\\3\\2\\1"
  a38 <- "([CTY])([CTY])([CTY])([CTY])([CTY])([CTY])([CTY])([CTY])([CTY])([CTY])([CTY])([CTY])([CTY])([CTY])([CTY])([CTY])([CTY])([CTY])\\w{0,500}\\18\\17\\16\\15\\14\\13\\12\\11\\10\\9\\8\\7\\6\\5\\4\\3\\2\\1"
  a37 <- "([CTY])([CTY])([CTY])([CTY])([CTY])([CTY])([CTY])([CTY])([CTY])([CTY])([CTY])([CTY])([CTY])([CTY])([CTY])([CTY])([CTY])\\w{0,500}\\17\\16\\15\\14\\13\\12\\11\\10\\9\\8\\7\\6\\5\\4\\3\\2\\1"
  a36 <- "([CTY])([CTY])([CTY])([CTY])([CTY])([CTY])([CTY])([CTY])([CTY])([CTY])([CTY])([CTY])([CTY])([CTY])([CTY])([CTY])\\w{0,500}\\16\\15\\14\\13\\12\\11\\10\\9\\8\\7\\6\\5\\4\\3\\2\\1"
  a35 <- "([CTY])([CTY])([CTY])([CTY])([CTY])([CTY])([CTY])([CTY])([CTY])([CTY])([CTY])([CTY])([CTY])([CTY])([CTY])\\w{0,500}\\15\\14\\13\\12\\11\\10\\9\\8\\7\\6\\5\\4\\3\\2\\1"

  #a35
  a35b <- gregexpr(a35, a, ignore.case = TRUE, perl = T)
  sequence_position <- a35b[[1]][1:length(a35b[[1]])]
  if(a35b[[1]][1] == -1){
    resultClean35 <- data.frame("sequence_position" = "-", "sequence" = "-", "sequence_length" = "-")
  }else{
    a8_35 <- regmatches(a, a35b)
    sequence <- a8_35[[1]][1:length(a8_35[[1]])]
    sequence_length <- nchar(sequence)
    resultClean4_35 <- data.frame(cbind(sequence_position, sequence, sequence_length))
    if(length(resultClean4_35[,1]) == 0){
      resultClean35 <- data.frame("sequence_position" = "-", "sequence" = "-", "sequence_length" = "-")
    }else{
      resultClean35 <- resultClean4_35
    }
  }

  #a36
  a36b <- gregexpr(a36, a, ignore.case = TRUE, perl = T)
  sequence_position <- a36b[[1]][1:length(a36b[[1]])]
  if(a36b[[1]][1] == -1){
    resultClean36 <- data.frame("sequence_position" = "-", "sequence" = "-", "sequence_length" = "-")
  }else{
    a8_36 <- regmatches(a, a36b)
    sequence <- a8_36[[1]][1:length(a8_36[[1]])]
    sequence_length <- nchar(sequence)
    resultClean4_36 <- data.frame(cbind(sequence_position, sequence, sequence_length))
    if(length(resultClean4_36[,1]) == 0){
      resultClean36 <- data.frame("sequence_position" = "-", "sequence" = "-", "sequence_length" = "-")
    }else{
      resultClean36 <- resultClean4_36
    }
  }

  #a37
  a37b <- gregexpr(a37, a, ignore.case = TRUE, perl = T)
  sequence_position <- a37b[[1]][1:length(a37b[[1]])]
  if(a37b[[1]][1] == -1){
    resultClean37 <- data.frame("sequence_position" = "-", "sequence" = "-", "sequence_length" = "-")
  }else{
    a8_37 <- regmatches(a, a37b)
    sequence <- a8_37[[1]][1:length(a8_37[[1]])]
    sequence_length <- nchar(sequence)
    resultClean4_37 <- data.frame(cbind(sequence_position, sequence, sequence_length))
    if(length(resultClean4_37[,1]) == 0){
      resultClean37 <- data.frame("sequence_position" = "-", "sequence" = "-", "sequence_length" = "-")
    }else{
      resultClean37 <- resultClean4_37
    }
  }

  #a38
  a38b <- gregexpr(a38, a, ignore.case = TRUE, perl = T)
  sequence_position <- a38b[[1]][1:length(a38b[[1]])]
  if(a38b[[1]][1] == -1){
    resultClean38 <- data.frame("sequence_position" = "-", "sequence" = "-", "sequence_length" = "-")
  }else{
    a8_38 <- regmatches(a, a38b)
    sequence <- a8_38[[1]][1:length(a8_38[[1]])]
    sequence_length <- nchar(sequence)
    resultClean4_38 <- data.frame(cbind(sequence_position, sequence, sequence_length))
    if(length(resultClean4_38[,1]) == 0){
      resultClean38 <- data.frame("sequence_position" = "-", "sequence" = "-", "sequence_length" = "-")
    }else{
      resultClean38 <- resultClean4_38
    }
  }

  #a39
  a39b <- gregexpr(a39, a, ignore.case = TRUE, perl = T)
  sequence_position <- a39b[[1]][1:length(a39b[[1]])]
  if(a39b[[1]][1] == -1){
    resultClean39 <- data.frame("sequence_position" = "-", "sequence" = "-", "sequence_length" = "-")
  }else{
    a8_39 <- regmatches(a, a39b)
    sequence <- a8_39[[1]][1:length(a8_39[[1]])]
    sequence_length <- nchar(sequence)
    resultClean4_39 <- data.frame(cbind(sequence_position, sequence, sequence_length))
    if(length(resultClean4_39[,1]) == 0){
      resultClean39 <- data.frame("sequence_position" = "-", "sequence" = "-", "sequence_length" = "-")
    }else{
      resultClean39 <- resultClean4_39
    }
  }

  #a40
  a40b <- gregexpr(a40, a, ignore.case = TRUE, perl = T)
  sequence_position <- a40b[[1]][1:length(a40b[[1]])]
  if(a40b[[1]][1] == -1){
    resultClean40 <- data.frame("sequence_position" = "-", "sequence" = "-", "sequence_length" = "-")
  }else{
    a8_40 <- regmatches(a, a40b)
    sequence <- a8_40[[1]][1:length(a8_40[[1]])]
    sequence_length <- nchar(sequence)
    resultClean4_40 <- data.frame(cbind(sequence_position, sequence, sequence_length))
    if(length(resultClean4_40[,1]) == 0){
      resultClean40 <- data.frame("sequence_position" = "-", "sequence" = "-", "sequence_length" = "-")
    }else{
      resultClean40 <- resultClean4_40
    }
  }

  #a41
  a41b <- gregexpr(a41, a, ignore.case = TRUE, perl = T)
  sequence_position <- a41b[[1]][1:length(a41b[[1]])]
  if(a41b[[1]][1] == -1){
    resultClean41 <- data.frame("sequence_position" = "-", "sequence" = "-", "sequence_length" = "-")
  }else{
    a8_41 <- regmatches(a, a41b)
    sequence <- a8_41[[1]][1:length(a8_41[[1]])]
    sequence_length <- nchar(sequence)
    resultClean4_41 <- data.frame(cbind(sequence_position, sequence, sequence_length))
    if(length(resultClean4_41[,1]) == 0){
      resultClean41 <- data.frame("sequence_position" = "-", "sequence" = "-", "sequence_length" = "-")
    }else{
      resultClean41 <- resultClean4_41
    }
  }

  #a42
  a42b <- gregexpr(a42, a, ignore.case = TRUE, perl = T)
  sequence_position <- a42b[[1]][1:length(a42b[[1]])]
  if(a42b[[1]][1] == -1){
    resultClean42 <- data.frame("sequence_position" = "-", "sequence" = "-", "sequence_length" = "-")
  }else{
    a8_42 <- regmatches(a, a42b)
    sequence <- a8_42[[1]][1:length(a8_42[[1]])]
    sequence_length <- nchar(sequence)
    resultClean4_42 <- data.frame(cbind(sequence_position, sequence, sequence_length))
    if(length(resultClean4_42[,1]) == 0){
      resultClean42 <- data.frame("sequence_position" = "-", "sequence" = "-", "sequence_length" = "-")
    }else{
      resultClean42 <- resultClean4_42
    }
  }

  #a43
  a43b <- gregexpr(a43, a, ignore.case = TRUE, perl = T)
  sequence_position <- a43b[[1]][1:length(a43b[[1]])]
  if(a43b[[1]][1] == -1){
    resultClean43 <- data.frame("sequence_position" = "-", "sequence" = "-", "sequence_length" = "-")
  }else{
    a8_43 <- regmatches(a, a43b)
    sequence <- a8_43[[1]][1:length(a8_43[[1]])]
    sequence_length <- nchar(sequence)
    resultClean4_43 <- data.frame(cbind(sequence_position, sequence, sequence_length))
    if(length(resultClean4_43[,1]) == 0){
      resultClean43 <- data.frame("sequence_position" = "-", "sequence" = "-", "sequence_length" = "-")
    }else{
      resultClean43 <- resultClean4_43
    }
  }

  #a44
  a44b <- gregexpr(a44, a, ignore.case = TRUE, perl = T)
  sequence_position <- a44b[[1]][1:length(a44b[[1]])]
  if(a44b[[1]][1] == -1){
    resultClean44 <- data.frame("sequence_position" = "-", "sequence" = "-", "sequence_length" = "-")
  }else{
    a8_44 <- regmatches(a, a44b)
    sequence <- a8_44[[1]][1:length(a8_44[[1]])]
    sequence_length <- nchar(sequence)
    resultClean4_44 <- data.frame(cbind(sequence_position, sequence, sequence_length))
    if(length(resultClean4_44[,1]) == 0){
      resultClean44 <- data.frame("sequence_position" = "-", "sequence" = "-", "sequence_length" = "-")
    }else{
      resultClean44 <- resultClean4_44
    }
  }

  #a45
  a45b <- gregexpr(a45, a, ignore.case = TRUE, perl = T)
  sequence_position <- a45b[[1]][1:length(a45b[[1]])]
  if(a45b[[1]][1] == -1){
    resultClean45 <- data.frame("sequence_position" = "-", "sequence" = "-", "sequence_length" = "-")
  }else{
    a8_45 <- regmatches(a, a45b)
    sequence <- a8_45[[1]][1:length(a8_45[[1]])]
    sequence_length <- nchar(sequence)
    resultClean4_45 <- data.frame(cbind(sequence_position, sequence, sequence_length))
    if(length(resultClean4_45[,1]) == 0){
      resultClean45 <- data.frame("sequence_position" = "-", "sequence" = "-", "sequence_length" = "-")
    }else{
      resultClean45 <- resultClean4_45
    }
  }

  #a15
  a15b <- gregexpr(a15, a, ignore.case = TRUE, perl = T)
  sequence_position <- a15b[[1]][1:length(a15b[[1]])]
  if(a15b[[1]][1] == -1){
    resultClean15 <- data.frame("sequence_position" = "-", "sequence" = "-", "sequence_length" = "-")
  }else{
    a8_15 <- regmatches(a, a15b)
    sequence <- a8_15[[1]][1:length(a8_15[[1]])]
    sequence_length <- nchar(sequence)
    resultClean4_15 <- data.frame(cbind(sequence_position, sequence, sequence_length))
    if(length(resultClean4_15[,1]) == 0){
      resultClean15 <- data.frame("sequence_position" = "-", "sequence" = "-", "sequence_length" = "-")
    }else{
      resultClean15 <- resultClean4_15
    }
  }

  #a16
  a16b <- gregexpr(a16, a, ignore.case = TRUE, perl = T)
  sequence_position <- a16b[[1]][1:length(a16b[[1]])]
  if(a16b[[1]][1] == -1){
    resultClean16 <- data.frame("sequence_position" = "-", "sequence" = "-", "sequence_length" = "-")
  }else{
    a8_16 <- regmatches(a, a16b)
    sequence <- a8_16[[1]][1:length(a8_16[[1]])]
    sequence_length <- nchar(sequence)
    resultClean4_16 <- data.frame(cbind(sequence_position, sequence, sequence_length))
    if(length(resultClean4_16[,1]) == 0){
      resultClean16 <- data.frame("sequence_position" = "-", "sequence" = "-", "sequence_length" = "-")
    }else{
      resultClean16 <- resultClean4_16
    }
  }

  #a17
  a17b <- gregexpr(a17, a, ignore.case = TRUE, perl = T)
  sequence_position <- a17b[[1]][1:length(a17b[[1]])]
  if(a17b[[1]][1] == -1){
    resultClean17 <- data.frame("sequence_position" = "-", "sequence" = "-", "sequence_length" = "-")
  }else{
    a8_17 <- regmatches(a, a17b)
    sequence <- a8_17[[1]][1:length(a8_17[[1]])]
    sequence_length <- nchar(sequence)
    resultClean4_17 <- data.frame(cbind(sequence_position, sequence, sequence_length))
    if(length(resultClean4_17[,1]) == 0){
      resultClean17 <- data.frame("sequence_position" = "-", "sequence" = "-", "sequence_length" = "-")
    }else{
      resultClean17 <- resultClean4_17
    }
  }

  #a18
  a18b <- gregexpr(a18, a, ignore.case = TRUE, perl = T)
  sequence_position <- a18b[[1]][1:length(a18b[[1]])]
  if(a18b[[1]][1] == -1){
    resultClean18 <- data.frame("sequence_position" = "-", "sequence" = "-", "sequence_length" = "-")
  }else{
    a8_18 <- regmatches(a, a18b)
    sequence <- a8_18[[1]][1:length(a8_18[[1]])]
    sequence_length <- nchar(sequence)
    resultClean4_18 <- data.frame(cbind(sequence_position, sequence, sequence_length))
    if(length(resultClean4_18[,1]) == 0){
      resultClean18 <- data.frame("sequence_position" = "-", "sequence" = "-", "sequence_length" = "-")
    }else{
      resultClean18 <- resultClean4_18
    }
  }

  #a19
  a19b <- gregexpr(a19, a, ignore.case = TRUE, perl = T)
  sequence_position <- a19b[[1]][1:length(a19b[[1]])]
  if(a19b[[1]][1] == -1){
    resultClean19 <- data.frame("sequence_position" = "-", "sequence" = "-", "sequence_length" = "-")
  }else{
    a8_19 <- regmatches(a, a19b)
    sequence <- a8_19[[1]][1:length(a8_19[[1]])]
    sequence_length <- nchar(sequence)
    resultClean4_19 <- data.frame(cbind(sequence_position, sequence, sequence_length))
    if(length(resultClean4_19[,1]) == 0){
      resultClean19 <- data.frame("sequence_position" = "-", "sequence" = "-", "sequence_length" = "-")
    }else{
      resultClean19 <- resultClean4_19
    }
  }

  #a20
  a20b <- gregexpr(a20, a, ignore.case = TRUE, perl = T)
  sequence_position <- a20b[[1]][1:length(a20b[[1]])]
  if(a20b[[1]][1] == -1){
    resultClean20 <- data.frame("sequence_position" = "-", "sequence" = "-", "sequence_length" = "-")
  }else{
    a8_20 <- regmatches(a, a20b)
    sequence <- a8_20[[1]][1:length(a8_20[[1]])]
    sequence_length <- nchar(sequence)
    resultClean4_20 <- data.frame(cbind(sequence_position, sequence, sequence_length))
    if(length(resultClean4_20[,1]) == 0){
      resultClean20 <- data.frame("sequence_position" = "-", "sequence" = "-", "sequence_length" = "-")
    }else{
      resultClean20 <- resultClean4_20
    }
  }

  #a21
  a21b <- gregexpr(a21, a, ignore.case = TRUE, perl = T)
  sequence_position <- a21b[[1]][1:length(a21b[[1]])]
  if(a21b[[1]][1] == -1){
    resultClean21 <- data.frame("sequence_position" = "-", "sequence" = "-", "sequence_length" = "-")
  }else{
    a8_21 <- regmatches(a, a21b)
    sequence <- a8_21[[1]][1:length(a8_21[[1]])]
    sequence_length <- nchar(sequence)
    resultClean4_21 <- data.frame(cbind(sequence_position, sequence, sequence_length))
    if(length(resultClean4_21[,1]) == 0){
      resultClean21 <- data.frame("sequence_position" = "-", "sequence" = "-", "sequence_length" = "-")
    }else{
      resultClean21 <- resultClean4_21
    }
  }

  #a22
  a22b <- gregexpr(a22, a, ignore.case = TRUE, perl = T)
  sequence_position <- a22b[[1]][1:length(a22b[[1]])]
  if(a22b[[1]][1] == -1){
    resultClean22 <- data.frame("sequence_position" = "-", "sequence" = "-", "sequence_length" = "-")
  }else{
    a8_22 <- regmatches(a, a22b)
    sequence <- a8_22[[1]][1:length(a8_22[[1]])]
    sequence_length <- nchar(sequence)
    resultClean4_22 <- data.frame(cbind(sequence_position, sequence, sequence_length))
    if(length(resultClean4_22[,1]) == 0){
      resultClean22 <- data.frame("sequence_position" = "-", "sequence" = "-", "sequence_length" = "-")
    }else{
      resultClean22 <- resultClean4_22
    }
  }

  #a23
  a23b <- gregexpr(a23, a, ignore.case = TRUE, perl = T)
  sequence_position <- a23b[[1]][1:length(a23b[[1]])]
  if(a23b[[1]][1] == -1){
    resultClean23 <- data.frame("sequence_position" = "-", "sequence" = "-", "sequence_length" = "-")
  }else{
    a8_23 <- regmatches(a, a23b)
    sequence <- a8_23[[1]][1:length(a8_23[[1]])]
    sequence_length <- nchar(sequence)
    resultClean4_23 <- data.frame(cbind(sequence_position, sequence, sequence_length))
    if(length(resultClean4_23[,1]) == 0){
      resultClean23 <- data.frame("sequence_position" = "-", "sequence" = "-", "sequence_length" = "-")
    }else{
      resultClean23 <- resultClean4_23
    }
  }

  #a24
  a24b <- gregexpr(a24, a, ignore.case = TRUE, perl = T)
  sequence_position <- a24b[[1]][1:length(a24b[[1]])]
  if(a24b[[1]][1] == -1){
    resultClean24 <- data.frame("sequence_position" = "-", "sequence" = "-", "sequence_length" = "-")
  }else{
    a8_24 <- regmatches(a, a24b)
    sequence <- a8_24[[1]][1:length(a8_24[[1]])]
    sequence_length <- nchar(sequence)
    resultClean4_24 <- data.frame(cbind(sequence_position, sequence, sequence_length))
    if(length(resultClean4_24[,1]) == 0){
      resultClean24 <- data.frame("sequence_position" = "-", "sequence" = "-", "sequence_length" = "-")
    }else{
      resultClean24 <- resultClean4_24
    }
  }

  #a25
  a25b <- gregexpr(a25, a, ignore.case = TRUE, perl = T)
  sequence_position <- a25b[[1]][1:length(a25b[[1]])]
  if(a25b[[1]][1] == -1){
    resultClean25 <- data.frame("sequence_position" = "-", "sequence" = "-", "sequence_length" = "-")
  }else{
    a8_25 <- regmatches(a, a25b)
    sequence <- a8_25[[1]][1:length(a8_25[[1]])]
    sequence_length <- nchar(sequence)
    resultClean4_25 <- data.frame(cbind(sequence_position, sequence, sequence_length))
    if(length(resultClean4_25[,1]) == 0){
      resultClean25 <- data.frame("sequence_position" = "-", "sequence" = "-", "sequence_length" = "-")
    }else{
      resultClean25 <- resultClean4_25
    }
  }

  resultCleanR <- rbind(resultClean25, resultClean24, resultClean23, resultClean22, resultClean21, resultClean20, resultClean19, resultClean18, resultClean17, resultClean16, resultClean15, resultClean35, resultClean36, resultClean37, resultClean38, resultClean39, resultClean40, resultClean41, resultClean42, resultClean43, resultClean44, resultClean45)
  resultCleanU <- unique(resultCleanR)
  if(nrow(resultCleanU) > 1){
    resultClean4 <- resultCleanU[!(resultCleanU$sequence == "-"),]
  }else{
    resultClean4 <- resultCleanU
  }
  if(nrow(resultClean4[!(resultClean4$sequence == "-"),]) > 0){
    resultClean5 <- resultClean4[order(as.integer(as.character(resultClean4[,1])), -as.integer(as.character(resultClean4[,3]))),]
    rownames(resultClean5) <- 1:nrow(resultClean5)
    intresult <- resultClean5
    intresult$end <- as.integer(as.character(intresult[,1])) + as.integer(as.character(intresult[,3])) - 1
    nRowsintresult <- nrow(intresult)
    startQ <- 1
    q <- 1
    k <- 0
    for(i in 1:nRowsintresult){
      k <- k + 1
      if(as.integer(as.character(intresult[k,1])) > as.integer(as.character(intresult[q,4]))){
        q <- k
        startQ <- c(startQ,q)
      }
    }
    resultClean6 <- resultClean5[startQ, ]
    rownames(resultClean6) <- 1:nrow(resultClean6)
  }else{
    resultClean6 <- data.frame("sequence_position" = "-", "sequence" = "-", "sequence_length" = "-")
  }

  return(resultClean6)
}

hdna_main <- function(b){
  if(length(b) == 1){
    #remove newlines
    b <- gsub("[\r\n]", "", b)
    b <- gsub(" ", "", b)
    # exit if unacceptable characters exist
    if(grepl("[^acgtryswkmbdhvnACGTRYSWKMBDHVN-]", b) == "TRUE"){
      b1 <- data.frame("sequence_position" = "!", "sequence" = "Error: Non-DNA character(s) in input", "sequence_length" = "!")
      return(b1)
    }else{
      # else continue
      b1 <- hdna_base(b)
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
      if(grepl("[^acgtryswkmbdhvnACGTRYSWKMBDHVN-]", i) == "TRUE"){
        b1 <- data.frame("sequence_position" = "!", "sequence" = "Error: Non-DNA character(s) in input", "sequence_length" = "!")
      }else{
        b1 <- hdna_base(i)
      }
      input_pos = input_pos + 1
      b2 <- cbind(input_ID = input_pos, b1)
      b2[,c(2,4)] <- sapply(b2[,c(2,4)],as.character)
      q <- rbind(q, b2)
    }
    return(q)
  }
}



#' Predicting intramolecular triplexes (H-DNA)
#'
#' This function predicts H-DNA
#' in 'x' (DNA). DNA can be provided in raw or fasta format or as GenBank accession number(s).
#' Internet is needed to connect to GenBank database, if accession number(s) is given as argument.
#'
#' @param x DNA sequence(s) in raw format or a fasta file or a GenBank accession number(s); from which H-DNA will be predicted.
#'  If the fasta file name does not contain an absolute path, the file name is relative to the current working directory.
#' @param xformat a character string specifying the format of x : default (raw), fasta, GenBank (GenBank accession number(s)).
#' @return A dataframe of H-DNA' position, sequence and length. If more than one DNA sequence is provided as argument, an input ID is returned for motif(s) predicted from each input sequence.
#' @author Hannah O. Ajoge
#' @details
#' This function predicts H-DNA in DNA sequences and provide the position, sequence and length of the predicted motif(s), if any.
#' @export
#' @importFrom ape read.GenBank
#' @importFrom seqinr read.fasta
#' @importFrom seqinr getSequence
#' @references paper under review
#' @seealso hdnaO
#' @examples
#' ## Predicting H-DNA from raw DNA sequences
#' E1 <- c("TCTTCCCCCCTTTTTYYYYYGCTYYYYYTTTTTCCCCCCGAAT", "taggtgctgggaggtagagacaggatatcct")
#' hdna(E1)
#'
#' ## Predicting H-DNA from DNA sequences in fasta file
#' ## Not run: hdna(x="Example.fasta", xformat = "fasta")
#'
#' ## Predicting H-DNA from DNA sequences,
#' ## using GenBank accession numbers.
#' ## Internet connectivity is needed for this to work.
#' ## Not run: hdna(c("BH114913", "AY611035"), xformat = "GenBank")



hdna <- function(x, xformat = "default"){
  if(xformat == "default"){
    x1 <- hdna_main(x)
    return(x1)
  }

  if(xformat == "GenBank"){
    x2 <- read.GenBank(x, as.character = TRUE)
    x3 <- sapply(x2, paste, collapse="")
    x4 <- hdna_main(x3)
    return(x4)
  }

  if(xformat == "fasta"){
    x5 <-read.fasta(x)
    x6 <- getSequence(x5, as.string = TRUE)
    x7 <- unlist(x6)
    x8 <- hdna_main(x7)
    return(x8)
  }else{
    stop("Unacceptable option for argument 'xformat'")
  }

}
