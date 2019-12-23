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

Ontario_ds<-data.frame("Area Code"=c(unlist(pcd),pcdm),
                       "Locale"=c(unlist(areas),areasm))
Ontario_ds<-Ontario_ds[order(Ontario_ds$Area.Code),]

####lets get a sample data set using the yellowpages webcraper
library(stringi)
library(tibble)
ypca_scrape<-function(url){
  ###############Functions needed#############################
  pipeit<-function(url,code){
    read_html(url)%>%html_nodes(code)%>%html_text()
  }
  
  pipelink<-function(url,code){
    read_html(url)%>%html_nodes(code)%>%html_attr("href")
  }
  ###################Scraper 
  scrape<-function(link){
    
    
    
    ##################################################
    
    
    #################GET LINKS###################
    
    
    ##main link###
    
    m<-"https://www.yellowpages.ca"
    
    ##extract sublinks##
    
    sb<-pipelink(link,".jsListingName")
    
    ##good sublinkslinks##
    gl<-paste0(m,sb)
    
    
    
    ###################SCRAPING###################
    
    ##name##
    nm<-sapply(gl,function(x){Sys.sleep(sample(10, 1) * 0.1) 
      pipeit(x,".merchant__title")})
    nm<-unname(nm)
    nm<-sapply(nm,function(x) x[1])
    nm<-nm[c(TRUE, FALSE)]
    
    ##Address##
    ad<-sapply(gl,function(x) { Sys.sleep(sample(10, 1) * 0.1)
      pipeit(x,".merchant__address__mobile")})
    ad<-unname(ad)
    ad<-gsub("\\n","",ad)
    
    ##Phone Number##
    pn<-sapply(gl,function(x){Sys.sleep(sample(10, 1) * 0.1)
      pipeit(x,".mlr__item--phone .mlr__submenu__item")})
    pn<-unname(pn)
    pn<-lapply(pn,function(x)unique(x))
    pn<-lapply(pn,function(x) paste(x))
    ##Website##
    wb<-sapply(gl,function(x){Sys.sleep(sample(10, 1) * 0.1)
      pipelink(x,".mlr__item__cta.hide-print")})
    wb<-unname(wb)
    wb<-suppressWarnings(str_extract(wb,"%3A%2F%2F(.*?)%2F"))
    wb<-gsub("%3A%2F%2F","",wb)
    wb<-gsub("%2F","",wb)
    
    ##Rating##
    rt<-sapply(gl,function(x){Sys.sleep(sample(10, 1) * 0.1)
      pipeit(x,".merchant__overall_rating")[1]})
    ##Data frame##
    dt<-tibble(nm,ad,pn,wb,rt)
    dt
    
  }
  
  
  ##number of pages##
  n<-pipeit(url,".pageCount span:nth-child(2)")
  n<-gsub("\\n","",n)
  n<-as.numeric(n)
  #Split and build url
  spliturl<-strsplit(url,"1")
  first<-spliturl[[1]][1]
  last<-spliturl[[1]][2]
  newurls<-c()
  for(i in 1:n){
    newurls[i]<-paste0(first,i,last)
  }
  
  ###Scrape and shape data###
  df<-sapply(newurls,function(x){Sys.sleep(sample(10, 1) * 0.1) 
    scrape(x)})
  ##Shape Data##
  dt<-sapply(df,unname)
  dt<-unname(df)
  dt<-rbind(df)
  nb<-lapply(dt[3,],function(x) paste(x))
  nb<-lapply(nb,function(x) sapply(x,function(y) gsub('c\\("',"",y)))
  nb<-lapply(nb,function(x) sapply(x,function(y) gsub('\"',"",y)))
  nb<-lapply(nb,function(x) sapply(x,function(y) gsub('\\)',"",y)))
  dt[3,]<-lapply(nb, unname)
  
  dt<-t(dt)
  rownames(dt)<-NULL
  
  dt 
}

###Music  in Ontario
dat<-ypca_scrape("https://www.yellowpages.ca/search/si/1/music+stores/Ontario+ON")

d1<-unlist(dat[,1])
d2<-unlist(dat[,2])
d3<-unlist(dat[,3])
d4<-unlist(dat[,4])
d5<-unlist(dat[,5])

dt<-tibble(d1,d2,d3,d4,d5)


##
library(plyr)
