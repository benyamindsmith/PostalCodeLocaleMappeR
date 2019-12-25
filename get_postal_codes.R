
require(stringr)

get_postal_codes<-function(x){
  str_extract_all(x,
                  "[ABCEGHJKLMNPRSTVXY]\\d[ABCEGHJ-NPRSTV-Z][ ]?\\d[ABCEGHJ-NPRSTV-Z]\\d")
}
