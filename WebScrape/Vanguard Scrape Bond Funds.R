library(rvest)

bondfundlist <- c("TIP","FGMNX","VWITX","PTTDX")

for (j in bondfundlist) {
  row <- row + 1
  print(paste("Bond Fund ",row,"= ",j,sep=""))
  
  # start <- "http://portfolios.morningstar.com/fund/summary?t="
  # end <- "&region=usa&culture=en_US"
  # url <- paste(qt,start,j,end,qt,sep="")
  var <- j
  url <- paste("http://portfolios.morningstar.com/fund/summary?t=",var,"&region=usa&culture=en_US",sep="")
  # print(url)
  
  # print(paste("URL= ",url,sep=""))
  mStar <- html(url)
  
  alloc[row,1] <- mStar %>% 
    html_node("h1") %>%
    html_text() 
  
  alloc[row,2] <- mStar %>% 
    html_node(".r_title .gry") %>%
    html_text()
  
  alloc[row,3] <- mStar %>% 
    html_node("#asset_allocation_tab .row_lbl+ td") %>%
    html_text()
  alloc[row,4] <- mStar %>% 
    html_node("tr:nth-child(4) .row_lbl+ td") %>%
    html_text()
  alloc[row,5] <- mStar %>% 
    html_node("#asset_allocation_tab tr:nth-child(6) .row_lbl+ td") %>%
    html_text()
  alloc[row,6] <- mStar %>% 
    html_node("#asset_allocation_tab tr:nth-child(8) .row_lbl+ td") %>%
    html_text()
  
  #Avg Duration (bonds)
  
  alloc[row,19] <- mStar %>% 
     html_node(".col2~ .col2 tr:nth-child(2) td") %>% 
     html_text()
  #Avg Maturity (bonds)
  alloc[row,20] <- mStar %>% 
    html_node(".col2~ .col2 tr:nth-child(4) td") %>% 
    html_text()
  
  
}

for (n in 1:row) {
  print(alloc[n,])
}
