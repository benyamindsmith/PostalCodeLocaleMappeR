get_postal_codes<-function(x){
  require(stringr)
  str_extract_all(x,
                  "[ABCEGHJKLMNPRSTVXY]\\d[ABCEGHJ-NPRSTV-Z][ ]?\\d[ABCEGHJ-NPRSTV-Z]\\d")
}
