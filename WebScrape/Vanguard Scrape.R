library(rvest)

cat ("\014")
fundList <- c("VFINX","SPY","FLPSX","EFA","EEM","FDIVX","IJJ","IJR","IJT","IVV","SDY","VBR","VFIAX","VIGAX","VINEX","VTMGX","VTMSX","VWO")
row <- 0
funds <- data.frame (25,21)
cats <- c("fundName","SYMBOL","cash","USstock","NonUSstock","Bond","Other","Giant","Large","Medium","Small","Micro","NorthAmerica","LatinAmerica","Europe","Asia","DevelopedMarkets","EmergingMarkets","AvgDuration","AvgMaturity","Yield")
alloc <- matrix (ncol=length(cats),nrow=25)
  
  for (j in fundList) {
    row <- row + 1
    print(paste("Fund ",row,"= ",j,sep=""))

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

alloc[row,3] <- as.numeric(mStar %>% 
        html_node("#asset_allocation_tab .row_lbl+ td") %>%
        html_text() ) 
alloc[row,4] <- as.numeric(mStar %>% 
        html_node("tr:nth-child(4) .row_lbl+ td") %>%
        html_text() ) 
alloc[row,5] <- as.numeric(mStar %>% 
        html_node("#asset_allocation_tab tr:nth-child(6) .row_lbl+ td") %>%
        html_text() ) 
alloc[row,6] <- as.numeric(mStar %>% 
        html_node("#asset_allocation_tab tr:nth-child(8) .row_lbl+ td") %>%
        html_text() ) 
alloc[row,7] <- as.numeric(mStar %>% 
        html_node("#asset_allocation_tab tr:nth-child(10) .row_lbl+ td") %>%
        html_text() ) 

# Scrape Market Cap

alloc[row,8] <- as.numeric(mStar %>% 
  html_node("#equity_style_tab #equity_style_tab tr:nth-child(2) .row_lbl+ td") %>%
        html_text() ) 
alloc[row,9] <- as.numeric(mStar %>% 
        html_node("#equity_style_tab #equity_style_tab tr:nth-child(4) .row_lbl+ td") %>%
        html_text() ) 
alloc[row,10] <- as.numeric(mStar %>% 
        html_node("#equity_style_tab #equity_style_tab tr:nth-child(6) .row_lbl+ td") %>%
        html_text() ) 
alloc[row,11] <- as.numeric(mStar %>% 
        html_node("#equity_style_tab #equity_style_tab tr:nth-child(8) .row_lbl+ td") %>%
        html_text() ) 
alloc[row,12] <- as.numeric(mStar %>% 
        html_node("#equity_style_tab #equity_style_tab tr:nth-child(10) .row_lbl+ td") %>%
        html_text() ) 

# Scrape WORLD REGIONS  
#North America
alloc[row,13] <- as.numeric(mStar %>% 
        html_node("#world_regions_tab tr:nth-child(4) td:nth-child(2)") %>%
        html_text() ) 
#Latin America
alloc[row,14] <- as.numeric(mStar %>% 
        html_node("#world_regions_tab tr:nth-child(6) td:nth-child(2)") %>%
        html_text() ) 
#Europe
alloc[row,15] <- as.numeric(mStar %>% 
        html_node("#world_regions_tab tr:nth-child(8) td:nth-child(2)") %>%
        html_text() ) 
#Asia
alloc[row,16] <- as.numeric(mStar %>% 
        html_node("#world_regions_tab tr:nth-child(18) td:nth-child(2)") %>%
        html_text() ) 
#Developed Markets
alloc[row,17] <- as.numeric(mStar %>% 
        html_node("#world_regions_tab tr:nth-child(31) td:nth-child(2)") %>%
        html_text() ) 

#Emerging Markets
alloc[row,18] <- as.numeric(mStar %>% 
        html_node("#world_regions_tab tr:nth-child(33) td:nth-child(2)") %>%
        html_text() ) 
  }
#
# SCRAPE BOND FUNDS
#
library(rvest)

bondfundlist <- c("FGMNX","VWITX","PTTDX")

for (j in bondfundlist) {
  row <- row + 1
  print(paste("Bond Fund ",row,"= ",j,sep=""))
  
  # Manually set bond yields
  if (j == "FGMNX") alloc[row,21] <- .01
  if (j == "VWITX") alloc[row,21] <- .02
  if (j == "PTTDX") alloc[row,21] <- .03

  
  # start <- "http://portfolios.morningstar.com/fund/summary?t="
  # end <- "&region=usa&culture=en_US"
  # url <- paste(qt,start,j,end,qt,sep="")
  var <- j
  url <- paste("http://portfolios.morningstar.com/fund/summary?t=",var,"&region=usa&culture=en_US",sep="")
  # print(url)
  
  # print(paste("URL= ",url,sep=""))
  mStar <- html(url)
  
  # Get yields from MarketWatch
  url2 <- paste("http://www.marketwatch.com/investing/fund/",j,sep="")
  mWatch <- html(url2)
  
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
  
# # get yield from MarketWatch
#   alloc[row,21] <- mWatch %>% 
#     html_node(".bgQuote .lastsection .data") %>% 
#     html_text()
  
}

for (n in 1:row) {
  print(alloc[n,])
}

#
# Manually enter yields

cat("Please update Yields for ",bondfundlist,sep=" ")



