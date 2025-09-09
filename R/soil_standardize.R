
#' Standardize soil cores by depth
#'
#' @param soilDf a dataframe that contains soil cores that need to be depth standardized
#' @param coreNumName unique identifier of each core (automatically assumed to be coreNum unless otherwise specified)
#' @param depth0Name top depths of each increment in soil core (automatically assumed to be depth0 unless otherwise specified)
#' @param depth1Name bottom depths of each increment in soil core (automatically assumed to be depth1 unless otherwise specified)
#'
#' @return A data frame with standardized depth values
#' @import dplyr
#' @export
#'
#' @examples NA

soilDf <- read.csv('/Volumes/Schuur Lab/2020 New_Shared_Files/DATA/CiPEHR & DryPEHR/Soil Cores/2022/Datasets/Soil2022NonStandardized2025-09-06.csv') %>%
  dplyr::group_by(fence, plot) %>%
  dplyr::mutate(coreNum = cur_group_id()) %>%
  dplyr::ungroup() %>%
  dplyr::select(-fence, -plot, -(X:depth.cat))
coreNumName <- "coreNum"
depth0Name <- "depth0"
depth1Name <- "depth1"
topDepthStd <- c(0, 5, 15, 25, 35, 45, 55, 65, 75, 85, 95, 105)
botDepthStd <- c(5, 15, 25, 35, 45, 55, 65, 75, 85, 95, 105, 115)
stdRes = 0.5

#takes in a data frame, and asks for the column name that shows the core identifier, and the top and bottom depth of the sample
soil_standardize <- function(soilDf, coreNumName = NA, depth0Name = NA, depth1Name = NA,
                             stdRes = 0.5, #I have 1 cm or 1 ft depths, and I want 0.5cm or 0.5ft depths
                             topDepthStd = c(0, 5, 15, 25, 35, 45, 55, 65, 75, 85, 95, 105),
                             botDepthStd = c(5, 15, 25, 35, 45, 55, 65, 75, 85, 95, 105, 115)){

  #to do: add check that standard depths are inclusive
if((is.na(coreNumName) & is.na(depth0Name) & is.na(depth1Name)) & (!("coreNum" %in% colnames(soilDf)) | !("depth0" %in% colnames(soilDf)) | !("depth1" %in% colnames(soilDf)))){
  print("Data frame must contain the following columns: 'coreNum', 'depth0', 'depth1'!")
} else if(!is.na(coreNumName) & !is.na(depth0Name) & !is.na(depth1Name)){
  coreNum <- soilDf$coreNum
  depth0 <- soilDf$depth0
  depth0 <- soilDf[[depth0Name]]
  depth1 <- soilDf$depth1
} else if(is.na(coreNumName) & is.na(depth0Name) & is.na(depth1Name)){
  coreNum <- soilDf$coreNum
  depth0 <- soilDf$depth0
  depth1 <- soilDf$depth1
} else if (!is.na(coreNumName) & is.na(depth0Name) & is.na(depth1Name)) {
  coreNum <- soilDf$coreNumName
  depth0 <- soilDf$depth0
  depth1 <- soilDf$depth1
} else if (is.na(coreNumName) & !is.na(depth0Name) & is.na(depth1Name)){
  coreNum <- soilDf$coreNum
  depth0 <- soilDf$depth0Name
  depth1 <- soilDf$depth1
} else if (is.na(coreNumName) & is.na(depth0Name) & !is.na(depth1Name)){
  coreNum <- soilDf$coreNum
  depth0 <- soilDf$depth0
  depth1 <- soilDf$depth1Name
} else if (!is.na(coreNumName) & !is.na(depth0Name) & is.na(depth1Name)){
  coreNum <- soilDf$coreNumName
  depth0 <- soilDf$depth0Name
  depth1 <- soilDf$depth1
} else if (!is.na(coreNumName) & is.na(depth0Name) & !is.na(depth1Name)){
  coreNum <- soilDf$coreNumName
  depth0 <- soilDf$depth0
  depth1 <- soilDf$depth1Name
} else if (is.na(coreNumName) & !is.na(depth0Name) & !is.na(depth1Name)){
  coreNum <- soilDf$coreNum
  depth0 <- soilDf$depth0Name
  depth1 <- soilDf$depth1Name
} else if (is.na(coreNumName) & !is.na(depth0Name) & !is.na(depth1Name)){
  coreNum <- soilDf$coreNumName
  depth0 <- soilDf$depth0Name
  depth1 <- soilDf$depth1Name
}

  cores <- unique(soilDf$coreNum)

  #These are the new depths that you want to standardize to
  depth0_values <- topDepthStd
  depth1_values <- botDepthStd
  #depth0_values <- c(0, 5, 15, 25, 35, 45, 55, 65, 75, 85, 95, 105)
  #depth1_values <- c(5, 15, 25, 35, 45, 55, 65, 75, 85, 95, 105, 115)

  soilDf$seg.length <- soilDf$depth1-soilDf$depth0
  soil_depth_std <- data.frame() #creates a blank dataframe that will hold the all the cores after they've been depth standardized

  for(core in cores){

    coreDf <- subset(soilDf, coreNum == core) %>%
      arrange(depth0) %>%
      mutate(cu.seg.length = NA)

    coreID <- unique(subset(soilDf, coreNum == core)$coreNum) #pull out coreID so that it can be added back into dataframe later

    coreSegDf <- data.frame(matrix(nrow = 0, ncol = ncol(coreDf))) #creates a blank dataframe that will hold the segmented core
    colnames(coreSegDf) <- colnames(coreDf) #make the column names the same

    #split the core into 0.5 cm increments
    for(i in 1:nrow(coreDf)){ #split the core into 0.5 cm increments
      segLength <- (1/stdRes)*(coreDf[i,]$seg.length) #*2 for 0.5 cm increments
      segMat <- data.frame(matrix(0, ncol = ncol(coreDf), nrow = segLength)) #another new dataframe to bind to the core one once each segment is filled.
      colnames(segMat) <- colnames(coreSegDf)
      segMat[1:segLength, ] <- coreDf[i,] #fill core df with copies of this segment, 2x as long as the segment (it's in 0.5cm increments)
      segMat$seg.length <- stdRes #for 0.5 cm increments
      coreSegDf <- rbind(coreSegDf, segMat)
    }

    coreSegAll <- coreSegDf %>%
      mutate(cu.seg.length = cumsum(seg.length)) # add up all the 0.5cm incrememnts

    #is this the last segment in the core? and is it not in the depth values? Figure out how many replicates are needed to get it to the next depth increment
    if (!(tail(coreSegAll, n=1)$cu.seg.length %in% depth1_values)){
      segLength <- (1/stdRes)*(depth1_values[which(depth1_values - tail(coreSegAll, n=1)$cu.seg.length >= 0)[1]] - tail(coreSegAll, n=1)$cu.seg.length) #how much depth needs to be added?
      segMat <- data.frame(matrix(0, ncol = ncol(coreDf), nrow = segLength))
      colnames(segMat) <- colnames(coreSegDf)
      segMat[1:segLength, ] <- coreDf[nrow(coreDf),]
      segMat$seg.length <- stdRes
      coreSegDf <- rbind(coreSegDf, segMat)
    }

    #modify the core segment df to get rid of plot and fence, etc, anything that can't be averaged
    #why can you get rid of depth 0 and depth1 here? Because no you will be using segment length
    coreSegAll <- coreSegDf %>%
      dplyr::select(-depth0, -depth1, -coreNum) %>%
      dplyr::mutate(cu.seg.length = cumsum(seg.length))

    #Create empty df that will hold the standardized core depth values
    coreStd <-data.frame(matrix(nrow = 0, ncol = ncol(coreSegAll)))
    colnames(coreStd) <- colnames(coreSegAll)
    names(coreStd)[names(coreStd) == "cu.seg.length"] <- "depth1" #names should match the coreSegAllnames, but the cu.seg.length is depth 1 now

    for (j in 1:length(depth1_values)){
      #since the model layer value isn't in integers
      xdepth_int <- round(depth1_values[j])
      thickness <- depth1_values[j] - depth0_values[j]

      for (i in 1:nrow(coreSegAll)){
        if (coreSegAll[i,]$cu.seg.length == xdepth_int){  #adjust window
          ## WARNING- don't know if this is right or what it is doing
          start_idx <- max(1, i - (thickness *(1/stdRes) -1)) #handles cases where there might be a negative number
          df <- coreSegAll[start_idx:i, ]      #extract a moving window
          #df <- coreSegAll[(i-(thickness*2)):i, ]

          # if(nrow(df) < length(depth1_values)){ #if there aren't enough standard depth increments sampled, fill the rest with NAs- this is the length of the depth1_values
          #   num_missing <- length(depth1_values) - nrow(df)
          #  missing_rows <- matrix(NA, nrow = num_missing, ncol = ncol(df))
          #   colnames(missing_rows) <- colnames(df)
          #   df <- rbind(df, missing_rows)
          # }

          #find the frequency of a value to find the weights
          #portions <- table(df$total_sand.wt) / sum(table(df$total_sand.wt))
          #weights <- portions[as.character(df$total_sand.wt)]

          #take the weighted average of the previous 10, .5cm values
          coreStdRow <- data.frame(lapply(df[], weighted.mean,  w = df$seg.length)) %>%
            #coreStdRow <- data.frame(lapply(df[], weighted.mean,  w = weights)) %>%
            mutate(depth0 = depth0_values[j], #add columns of info that isn't used in calc back into row
                   depth1 = depth1_values[j],
                   #zi = model_lyrs_df$zi[j],
                   coreNum = unique(coreDf$coreNum),
                   coreID = coreID) %>%
            dplyr::select(-cu.seg.length) #don't include this col in final dataset

          soil_depth_std <- rbind(soil_depth_std, coreStdRow) #add the row into the collection dataframe

        } #close if statement
      } #close rows loop

    } #close depth values loop
  }
  #to do: rename the columns to match the original naming
  return(soil_depth_std)
}

