#'Write function for getting locale from postal codes

library(rvest)
library(stringr)

pipeit<-function(url,code){
  read_html(url)%>%html_nodes(code)%>%html_text()
}

pipelink<-function(url,code){
  read_html(url)%>%html_nodes(code)%>%html_attr("href")
}


get_postal_codes<-function(x){
  str_extract_all(x,
                  "[ABCEGHJKLMNPRSTVXY]\\d[ABCEGHJ-NPRSTV-Z][ ]?\\d[ABCEGHJ-NPRSTV-Z]\\d")
}

main_links<-c("https://en.wikipedia.org/wiki/List_of_postal_codes_of_Canada:_A",
              "https://en.wikipedia.org/wiki/List_of_postal_codes_of_Canada:_B",
              "https://en.wikipedia.org/wiki/List_of_postal_codes_of_Canada:_C",
              "https://en.wikipedia.org/wiki/List_of_postal_codes_of_Canada:_E",
              "https://en.wikipedia.org/wiki/List_of_postal_codes_of_Canada:_G",
              "https://en.wikipedia.org/wiki/List_of_postal_codes_of_Canada:_H",
              "https://en.wikipedia.org/wiki/List_of_postal_codes_of_Canada:_J",
              "https://en.wikipedia.org/wiki/List_of_postal_codes_of_Canada:_K",
              "https://en.wikipedia.org/wiki/List_of_postal_codes_of_Canada:_L",
              "https://en.wikipedia.org/wiki/List_of_postal_codes_of_Canada:_M",
              "https://en.wikipedia.org/wiki/List_of_postal_codes_of_Canada:_N",
              "https://en.wikipedia.org/wiki/List_of_postal_codes_of_Canada:_P",
              "https://en.wikipedia.org/wiki/List_of_postal_codes_of_Canada:_R",
              "https://en.wikipedia.org/wiki/List_of_postal_codes_of_Canada:_S",
              "https://en.wikipedia.org/wiki/List_of_postal_codes_of_Canada:_T",
              "https://en.wikipedia.org/wiki/List_of_postal_codes_of_Canada:_V",
              "https://en.wikipedia.org/wiki/List_of_postal_codes_of_Canada:_X",
              "https://en.wikipedia.org/wiki/List_of_postal_codes_of_Canada:_Y")

pcd<-sapply(main_links[c(8,9,11,12)],function(x)pipeit(x,"td > b"))
pcdm<-pipeit(main_links[10],"td:nth-child(1)")[-c(288:292)]

areas<-sapply(main_links[c(8,9,11,12)],function(x) pipeit(x,"h3+ table td"))
areas<-lapply(areas,function(x) gsub("[A-Z]\\d[A-Z]","",x))
areas<-lapply(areas,function(x) gsub("\\n.*","",x))

areasm<-pipeit(main_links[10],"td:nth-child(2)")[-c(288:292)]

##Now lets build a data set

Ontario_ds<-data.frame(c(unlist(pcd),pcdm),c(unlist(areas),areasm))
