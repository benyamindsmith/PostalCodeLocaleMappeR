
Ontario_PC_MappeR<-function(addresses){
  
  require(readr)
  ##First lets attach the Ontario Postal Codes Data set##
  Ontario_ds<-read.csv(url("https://raw.githubusercontent.com/benyamindsmith/PostalCodeLocaleMappeR/master/Ontario%20Postal%20Code%20Dataset.csv?token=ALCCTHSDAGXRODXBPOQHSRC6AJDAS"))
  
  ##Now lets get our required functions
  get_postal_codes<-function(x){
    str_extract_all(x,
                    "[ABCEGHJKLMNPRSTVXY]\\d[ABCEGHJ-NPRSTV-Z][ ]?\\d[ABCEGHJ-NPRSTV-Z]\\d")
  }
  
  ##Lets extract the Postal Codes
  
  pc<-get_postal_codes(addresses)
  
  ##Get FSAs
  fsa<-str_extract_all(pc,"[A-Z][0-9][A-Z]")
  fsa<-unlist(fsa)
  
  ##Now match
  
  ind<- match(fsa,Ontario_ds$Area.Code)
  
  ##Get result
  
  locale<-sapply(ind,function(x) Ontario_ds$Locale[x])
  
  ##Print result
  
  locale
}
