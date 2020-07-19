## ----include=FALSE, message=FALSE---------------------------------------------

rm(list=ls())

library(sf)
library(dplyr)
library(cartograflow)
library(cartography)

knitr::opts_chunk$set(fig.width=6, fig.height=6)


## ----flowdata_preprocess, warning=FALSE, echo=TRUE----------------------------

# Load Statistical information
tabflow<-read.csv2("./data/MOBPRO_ETP.csv", header=TRUE, sep=";",stringsAsFactors=FALSE,
                   encoding="UTF-8", dec=".",check.names=FALSE)


## ----var_typing, echo=FALSE, warning=FALSE------------------------------------
# Variable typing
tabflow$i<-as.character(tabflow$i)
tabflow$j<-as.character(tabflow$j)
tabflow$Fij<-as.numeric(tabflow$Fij)
tabflow$count<-as.numeric(tabflow$count)
str(tabflow)

## ----flowdata_reverse, echo=TRUE, message=FALSE, warning=FALSE----------------

# Selecting useful variables for changing format
tabflow<-tabflow %>% select(i,j,Fij)

# From list (L) to matrix (M) format
matflow <-flowtabmat(tabflow,matlist="M")
head(matflow[1:4,1:4])
dim(matflow)


## ----flowdata_reverseM, message=FALSE, warning=FALSE, include=FALSE-----------

# From matrix (M) to list (L) format
tabflow<-flowtabmat(tab=matflow,
                    matlist="L")
colnames(tabflow)<-c("i","j","Fij")
head(tabflow)


## ----data_preprocess, message=FALSE, warning=FALSE, include=FALSE-------------

# Load a list of geo codes
ID_CODE<-read.csv2("./data/COD_GEO_EPT.csv",
                   header=TRUE,sep=";",stringsAsFactors=FALSE,encoding="UTF-8", dec=".",   check.names=FALSE)
#head(ID_CODE)

CODE<-ID_CODE%>% dplyr::select(COD_GEO_EPT)

colnames(CODE)<-c("CODGEO")
#head(CODE)


## ----vara_typing2, message=FALSE, warning=FALSE, include=FALSE----------------
# Variable typing
tabflow$i<-as.character(tabflow$i)
tabflow$j<-as.character(tabflow$j)
tabflow$Fij<-as.numeric(tabflow$Fij)
as.data.frame(tabflow)

## ----data_computing, echo=TRUE, message=FALSE, warning=FALSE------------------

# Bilateral volum (gross) FSij:  
tabflow_vol<-flowtype(tabflow, format="L", origin="i", destination="j", fij="Fij",  x= "bivolum" )
# Matrix format (M= : matflow_vol<-flowtype(matflow, format="M", "bivolum")

# Bilateral balance (net ) FBij:  
tabflow_net<-flowtype(tabflow, format="L", origin="i", destination="j", fij="Fij", x="bibal")

# Bilateral maximum (maxFij): 
tabflow_max<-flowtype(tabflow, format="L", origin="i", destination="j", fij="Fij", x="bimax")

# Compute all types of bilateral flows, in one 11 columns
tabflow_all<-flowtype(tabflow,format="L", origin="i", destination="j", fij="Fij", x="alltypes")
head(tabflow_all) 


## ----maps_links, echo=TRUE, fig.show='hold', fig.width=6, message=FALSE, warning=FALSE, ECHO=FALSE----

library(sf)
map<-st_read("./data/MGP_TER.shp")

# Add and overlay spatial background 
par(bg = "NA")

# Graphic parameters
par(mar=c(0,0,1,0))
extent <- c(2800000, 1340000, 6400000, 4800000)
resolution<-150

plot(st_geometry(map), col = NA, border=NA, bg="#dfe6e1")
plot(st_geometry(map), col = "light grey", add=TRUE)

# Flowmapping of all links

flowmap(tab=tabflow,
        fij="Fij",
        origin.f = "i",
        destination.f = "j",
        bkg = map,
        code="EPT_NUM",
        nodes.X="X",
        nodes.Y = "Y",
        filter=FALSE,
        add=TRUE
        )

library(cartography)

# Map cosmetics
layoutLayer(title = "All origin-destination for commuting in Greater Paris, 2017",
           coltitle ="black",
           author = "Cartograflow, 2020",
           sources = "Data : INSEE, 2017 ; Basemap : APUR, RIATE, 2018.",
           scale = 2,
           tabtitle = FALSE,
           frame = TRUE,
           col = "grey"
            )
# North arrow
north("topright")


## ----maps_flowmean, echo=TRUE, fig.show='hold', fig.width=6, message=FALSE, warning=FALSE, ECHO=FALSE----

library(sf)
map<-st_read("./data/MGP_TER.shp")

# Add and overlay spatial background 
par(bg = "NA")

# Graphic parameters
par(mar=c(0,0,1,0))
extent <- c(2800000, 1340000, 6400000, 4800000)
resolution<-150

plot(st_geometry(map), col = NA, border=NA, bg="#dfe6e1")
plot(st_geometry(map), col = "light grey", add=TRUE)

# Flow mapping above-average flows
flowmap(tab=tabflow,
        fij="Fij",
        origin.f = "i",
        destination.f = "j",
        bkg = map,
        code="EPT_NUM",
        nodes.X="X",
        nodes.Y = "Y",
        filter=TRUE,
        threshold =(mean(tabflow$Fij)),  #mean value is the level of threshold
        taille=20,           
        a.head = 1,
        a.length = 0.11,
        a.angle = 30,
        a.col="#138913",
        add=TRUE)

# Map Legend
legendPropLines(pos="topleft",
                title.txt="Commuters > 13220 ",
                title.cex=0.8,   
                cex=0.5,
                values.cex= 0.7,  
                var=c(mean(tabflow$Fij),max(tabflow$Fij)), 
                lwd=5, 
                frame = FALSE,
                col="#138913",
                values.rnd = 0
                )

#Map cosmetic

layoutLayer(title = "Commuters up to above-average in Greater Paris",
           coltitle ="black",
           author = "Cartograflow, 2020",
           sources = "Data : INSEE, 2017 ; Basemap : APUR, RIATE, 2018.",
           scale = 2,
           tabtitle = FALSE,
           frame = TRUE,
           col = "grey"
            )

# North arrow
north("topright")


## ----maps_flownet, echo=TRUE, fig.show='hold', fig.width=6, message=FALSE, warning=FALSE, ECHO=FALSE----

#library(sf)
map<-st_read("./data/MGP_TER.shp")

# Net matrix reduction
tabflow_net <- tabflow_net %>% filter(.data$FBij>=0)

# Net matrix thresholding
Q80<-quantile(tabflow_net$FBij,0.95)


# Add and overlay spatial background 
par(bg = "NA")

# Graphic parameters
par(mar=c(0,0,1,0))
extent <- c(2800000, 1340000, 6400000, 4800000)
resolution<-150

plot(st_geometry(map), col = NA, border=NA, bg="#dfe6e1")
plot(st_geometry(map), col = "light grey", add=TRUE)

# Flow mapping above-average flows
flowmap(tab=tabflow_net,
        fij="FBij",
        origin.f = "i",
        destination.f = "j",
        bkg = map,
        code="EPT_NUM",
        nodes.X="X",
        nodes.Y = "Y",
        filter=TRUE,
        threshold = Q80,
        taille=12,           
        a.head = 1, 
        a.length = 0.11,
        a.angle = 30,
        a.col="#4e8ef5",
        add=TRUE)

# Map Legend
legendPropLines(pos="topleft",
                title.txt="Commuters > 5722 ",
                title.cex=0.8,   
                cex=0.5,
                values.cex= 0.7,  
                var=c(Q80,max(tabflow_net$FBij)), 
                lwd=12, 
                frame = FALSE,
                col="#4e8ef5",
                values.rnd = 0
                )

#Map cosmetic

layoutLayer(title = "Net commuters in Greater Paris (20% strongest)",
           coltitle ="black",
           author = "Cartograflow, 2020",
           sources = "Data : INSEE, 2017 ; Basemap : APUR, RIATE, 2018.",
           scale = 2,
           tabtitle = FALSE,
           frame = TRUE,
           col = "grey"
            )

# North arrow
north("topright")


