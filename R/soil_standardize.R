
soil_standardize <- function(soilDf, coreNumName = NA, depth0Name = NA, depth1Name = NA, newDepth0, newDepth1){

  if((is.na(coreNumName) | is.na(depth0Name) | is.na(depth1Name)) & ){

  } else if(is.na(coreNumName) | is.na(depth0Name) | is.na(depth1Name)){
    coreNum <- soilDf$coreNum
    depth0 <- soilDf$depth0
    depth1 <- soilDf$depth1
  }else {
    coreNum <- soilDf$coreNumName
    depth0 <- soilDf$depth0Name
    depth1 <- soilDf$depth1Name
  }


  soilDf <- soilDf %>%
    dplyr::mutate(coreNum = {{coreNum}},
                  depth0 = {{depth0}},
                  depth1 = {{depth1}})
  #check and give error if cols aren't present
  #rename option- make parameters with NA as default and then check that cols are there and named appropriately


  cores <- unique(soilCores$coreNum)

  #These are the new depths that you want to standardize to
  depth0_values <- c(0, 5, 15, 25, 35, 45, 55, 65, 75, 85, 95, 105)
  depth1_values <- c(5, 15, 25, 35, 45, 55, 65, 75, 85, 95, 105, 115)
  #To do: check that depth 0 and depth 1 share values
  #Do this for new and old depth0 and depth1 (to know that cores depths are continuous)
  #To do: make the newDepth0 and 1 customizeable

  soilDf$seg.length <- soilDf$depth1-soilDf$depth0
  soil_depth_std <- data.frame(data.frame(matrix(nrow = 0, ncol = ncol(soilDf)+1))) #creates a blank dataframe that will hold the all the cores after they've been depth standardized

  for(core in cores){
    coreDf <- subset(soilDf, coreNum == core) %>% #creates a dataframe that has one core in it, arranged by depth
      arrange(depth0)
    coreSegDf <- data.frame(matrix(nrow = 0, ncol = ncol(coreDf))) #creates a blank dataframe that will hold the segmented core
    colnames(coreSegDf) <- colnames(coreDf) #make the column names the same

    #split the core into 0.5 cm increments
    #to-do: documentation should indicate that the depths can't have greater than 0.5cm resolution
    for(i in 1:nrow(coreDf)){ #split the core into 0.5 cm increments

      #To do: might want to make this first if statement something that someone could change
      if (i == nrow(coreDf) & coreDf[i,]$seg.length != 10){ #is this the last segment in the core? and is it not a 10cm segment? Then make it a 10 cm increment
        segLength <- 2*(coreDf[i,]$seg.length)
        segMat <- data.frame(matrix(0, ncol = ncol(coreDf), nrow = segLength))
        colnames(segMat) <- colnames(coreSegDf)
        segMat[1:20, ] <- coreDf[i,]
        segMat$seg.length <- 0.5
        coreSegDf <- rbind(coreSegDf, segMat)
      } else {
        segLength <- 2*(coreDf[i,]$seg.length)
        segMat <- data.frame(matrix(0, ncol = ncol(coreDf), nrow = segLength))
        colnames(segMat) <- colnames(coreSegDf)
        segMat[1:segLength, ] <- coreDf[i,]
        segMat$seg.length <- 0.5
        coreSegDf <- rbind(coreSegDf, segMat)
      }
    }

    #modify the core segment df to get rid of plot and fence, etc, anything that can't be averaged
    #why can you get rid of depth 0 and depth1 here? Because no you will be using segment length
    coreSegAll <- coreSegDf %>%
      dplyr::select(-depth0, -depth1, -coreNum) %>%
      mutate(cu.seg.length = cumsum(seg.length))

    #Create empty df that will hold the standardized core depth values
    coreStd <-data.frame(matrix(nrow = 0, ncol = ncol(coreSegAll)))
    colnames(coreStd) <- colnames(coreSegAll)
    names(coreStd)[names(coreStd) == "cu.seg.length"] <- "depth1" #names should match the coreSegAllnames, but the cu.seg.length is depth 1 now

    #now calculate the standard depths from all the 0.5 cm pieces added up
    for(i in 1:nrow(coreSegAll)){
      if(coreSegAll[i,]$cu.seg.length == 5){
        df <- coreSegAll[(i-9):i, ]
        #sum the previous 10 values for data every column
        coreStdRow <- data.frame(lapply(df[], weighted.mean,  w = df$seg.length)) %>%
          mutate(depth1 = 5) %>%
          dplyr::select(-cu.seg.length)
        coreStd <- rbind(coreStd, coreStdRow)
      }
      else if(coreSegAll[i,]$cu.seg.length == 15){
        df <- coreSegAll[(i-19):i, ]
        #sum the previous 20 values for data every column
        coreStdRow <- data.frame(lapply(df[], weighted.mean,  w = df$seg.length)) %>%
          mutate(depth1 = 15)%>%
          dplyr::select(-cu.seg.length)
        coreStd <- rbind(coreStd, coreStdRow)
      }
      else if(coreSegAll[i,]$cu.seg.length == 25){
        df <- coreSegAll[(i-19):i, ]
        #sum the previous 20 values for data every column
        coreStdRow <- data.frame(lapply(df[], weighted.mean,  w = df$seg.length)) %>%
          mutate(depth1 = 25)%>%
          dplyr::select(-cu.seg.length)
        coreStd <- rbind(coreStd, coreStdRow)
      }
      else if(coreSegAll[i,]$cu.seg.length == 35){
        df <- coreSegAll[(i-19):i, ]
        #sum the previous 20 values for data every column
        coreStdRow <- data.frame(lapply(df[], weighted.mean,  w = df$seg.length)) %>%
          mutate(depth1 = 35)%>%
          dplyr::select(-cu.seg.length)
        coreStd <- rbind(coreStd, coreStdRow)
      }
      else if(coreSegAll[i,]$cu.seg.length == 45){
        df <- coreSegAll[(i-19):i, ]
        #sum the previous 20 values for data every column
        coreStdRow <- data.frame(lapply(df[], weighted.mean,  w = df$seg.length)) %>%
          mutate(depth1 = 45)%>%
          dplyr::select(-cu.seg.length)
        coreStd <- rbind(coreStd, coreStdRow)
      }
      else if(coreSegAll[i,]$cu.seg.length == 55){
        df <- coreSegAll[(i-19):i, ]
        #sum the previous 20 values for data every column
        coreStdRow <- data.frame(lapply(df[], weighted.mean,  w = df$seg.length)) %>%
          mutate(depth1 = 55)%>%
          dplyr::select(-cu.seg.length)
        coreStd <- rbind(coreStd, coreStdRow)
      }
      else if(coreSegAll[i,]$cu.seg.length == 65){
        df <- coreSegAll[(i-19):i, ]
        #sum the previous 20 values for data every column
        coreStdRow <- data.frame(lapply(df[], weighted.mean,  w = df$seg.length)) %>%
          mutate(depth1 = 65)%>%
          dplyr::select(-cu.seg.length)
        coreStd <- rbind(coreStd, coreStdRow)
      }
      else if(coreSegAll[i,]$cu.seg.length == 75){
        df <- coreSegAll[(i-19):i, ]
        #sum the previous 20 values for data every column
        coreStdRow <- data.frame(lapply(df[], weighted.mean,  w = df$seg.length)) %>%
          mutate(depth1 = 75)%>%
          dplyr::select(-cu.seg.length)
        coreStd <- rbind(coreStd, coreStdRow)
      }
      else if(coreSegAll[i,]$cu.seg.length == 85){
        df <- coreSegAll[(i-19):i, ]
        #sum the previous 20 values for data every column
        coreStdRow <- data.frame(lapply(df[], weighted.mean,  w = df$seg.length)) %>%
          mutate(depth1 = 85)%>%
          dplyr::select(-cu.seg.length)
        coreStd <- rbind(coreStd, coreStdRow)
      }
      else if(coreSegAll[i,]$cu.seg.length == 95){
        df <- coreSegAll[(i-19):i, ]
        #sum the previous 20 values for data every column
        coreStdRow <- data.frame(lapply(df[], weighted.mean,  w = df$seg.length)) %>%
          mutate(depth1 = 95)%>%
          dplyr::select(-cu.seg.length)
        coreStd <- rbind(coreStd, coreStdRow)
      }
      else if(coreSegAll[i,]$cu.seg.length == 105){
        df <- coreSegAll[(i-19):i, ]
        #sum the previous 20 values for data every column
        coreStdRow <- data.frame(lapply(df[], weighted.mean,  w = df$seg.length)) %>%
          mutate(depth1 = 105)%>%
          dplyr::select(-cu.seg.length)
        coreStd <- rbind(coreStd, coreStdRow)
      }
    }
    coreStd[nrow(coreStd)+(length(depth0_values)-nrow(coreStd)),] <- NA #this adds NAs to depths that didn't have any sample
    coreStd <- coreStd %>%
      dplyr::select(-seg.length) %>%
      mutate(depth0 = depth0_values,
             depth1 = depth1_values,
             coreNum = unique(coreDf$coreNum))

    #now add that to a df with all the core values
    colnames(soil_depth_std) <- colnames(coreStd)
    soil_depth_std <- rbind(soil_depth_std, coreStd)
  }
#to-do: clean up the soil_depth_standard order return
  return(soil_depth_std)
}


soil_standardize(soilDf = soilCores)
