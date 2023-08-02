#'
#' Correct the date for the growing season
#' 
#' The growing season is assumed to run from
#' June 1st to Aug 31st. The corrected date for
#' measurements taken before June 1st is the 
#' previous year. Measurements taken during the 
#' growth season yields a non round year. For 
#' instance, a measurement taken on July 1st 2012
#' yields a corrected date of 2011.33.
#' @param dataSet the nonoverlappingIntervals data.frame
#' @param dateFieldName the name of the field that contains the data (string)
#'  
getCorrectedYear <- function(dataSet, dateFieldName) {
  a <- as.Date(dataSet[,dateFieldName], format = "%Y/%m/%d")
  year <- as.integer(format(a, "%Y"))
  b <- as.Date(paste0(year,"/5/31"), format = "%Y/%m/%d")
  timeDiff <- a-b
  yearModifier <- rep(0,length(timeDiff))
  yearModifier[which(timeDiff <= 0)] <- -1
  yearModifier[which(timeDiff > 0 & timeDiff <= 92)] <- timeDiff[which(timeDiff > 0 & timeDiff <= 92)] / 92 - 1 
  return(year + yearModifier)
}



###############################
# Utility functions 
# Mathieu Fortin - March 2020
###############################

getFilepath <- function(thisFile) {
  return(paste(getDatabaseDirectoryPath(), thisFile, sep="/"))
}

tabulate <- function(dataFrame, nbDigits = NULL, prefix = NULL, suffix=NULL) {
  .tmp <- dataFrame
  for (field in colnames(.tmp)) {
    if ("numeric" %in% class(.tmp[,field])) {
      if (!is.null(nbDigits)) {
        .tmp[,field] <- format(round(.tmp[,field], digits = nbDigits), nsmall = nbDigits)
      }
      if (!is.null(suffix)) {
        .tmp[,field] <- paste(as.character(.tmp[,field]),suffix,sep="") 
      }
    }
    if ("factor" %in% class(.tmp[,field])) {
      .tmp[,field] <- as.character(.tmp[,field])
    }
  }
  output <- NULL
  for (i in 1:length(.tmp[,1])) {
    if (!is.null(prefix)) {
      entry <- paste(prefix, paste(paste(.tmp[i,], collapse = " & "), "\\"), sep="")
    } else {
      entry <- paste(paste(.tmp[i,], collapse = " & "), "\\")
    }
    output <- rbind(output, data.frame(entry))    
  }
  output[,1] <- format(output[,1], justfy = "left") 
  print(output, row.names = F)
}


removeAllExcept <- function(except="", includeFunctions = F) {
  allObjects <- ls(envir = globalenv())
  if (includeFunctions) {
    objectsToBeConsidered <- allObjects    
  } else {
    objectsToBeConsidered <- c()
    for (obj in allObjects) {
      if (!is.function(get(obj, envir = globalenv()))) {
        objectsToBeConsidered <- c(objectsToBeConsidered, obj)
      }
    }
  }
  rm(list = objectsToBeConsidered[which(!objectsToBeConsidered %in% except)], envir = globalenv())
}

replaceFieldName <- function(dataFrame, index, newName) {
  fieldNames <- colnames(dataFrame)
  fieldNames[index] <- newName
  colnames(dataFrame) <- fieldNames
  return(dataFrame)
}

# extractLastValue <- function(dataSet, fieldName, newFieldName) {
#   vector <- dataSet[, fieldName]
#   data <- dataSet[!is.na(vector), c(fieldName, "PlotID", "Measure")]
#   lastMeasure <- aggregate(Measure ~ PlotID, data = data, FUN="max")
#   data <- merge(data, lastMeasure, by=c("PlotID", "Measure"))
#   data <- replaceFieldName(data, length(data[1,]), newFieldName)
#   return(data)
# }

# setLastValueOf <- function(dataSet, fieldName, newFieldName) {
#   tmp <- extractLastValue(dataSet, fieldName, newFieldName)
#   indexPlots <<- merge(indexPlots, tmp[,c("PlotID", newFieldName)], all.x = T, by=c("PlotID"))
# }
# 


setLastValueOf <- function(dataSet, fieldName, newFieldName, plotIndexField, measureIndexField) {
  tmp <- extractLastValue(dataSet, fieldName, newFieldName, plotIndexField, measureIndexField)
  indexPlots <<- merge(indexPlots, tmp[,c(plotIndexField, newFieldName)], all.x = T, by=c(plotIndexField))
}

extractLastValue <- function(dataSet, fieldName, newFieldName, plotIndexField, measureIndexField) {
  vector <- dataSet[, fieldName]
#  data <- dataSet[!is.na(vector), c(fieldName, plotIndexField, measureIndexField)]  ### former version that did not account for field set to ""
  data <- dataSet[which(!is.na(vector) & vector != ""), c(fieldName, plotIndexField, measureIndexField)]   
  lastMeasure <- aggregate(formula(paste(measureIndexField, "~", plotIndexField)), data = data, FUN="max")
  data <- merge(data, lastMeasure, by=c(plotIndexField, measureIndexField))
  data <- replaceFieldName(data, length(data[1,]), newFieldName)
  return(data)
}



createDummyNonMissingValues <- function(dataSet, vectorFieldNames) {
  for (index in 1:length(vectorFieldNames)) {
    value_i <- as.numeric(!is.na(dataSet[,vectorFieldNames[index]]))    
    if (index == 1) {
      value <- value_i
    } else {
      value <- value * value_i
    }
  }
  dataSet$nonMissingValues <- as.logical(value)
  return(dataSet)
}

compareTwoDataFrame <- function(dataFrameNew, dataFrameRef) {
  index <- which(!colnames(dataFrameNew) %in% colnames(dataFrameRef))
  if (length(index) > 0) {
    newFields <- colnames(dataFrameNew)[index]
    print(paste("Warning: the new file contains additional fields : ", paste(newFields, collapse = ",")))
  }
  index <- which(!colnames(dataFrameRef) %in% colnames(dataFrameNew)) 
  if (length(index) > 0) {
    print(index)
    stop("Some fields of the previous file are missing in the new one!")
  } else {
    for (field in colnames(dataFrameRef)) {
      vec1 <- dataFrameNew[,field]
      vec2 <- dataFrameRef[,field]
      index <- which(is.na(vec1) != is.na(vec2))
      index <- c(index, which(vec1 != vec2))
      if (length(index) > 0) {
        print(field)
        print(index)
        stop("Differences detected in this field")
      }
    }
  }
  return("Ok! No difference")
}

removeTheseFields <- function(dataSet, fieldsToBeRemoved) {
  currentFields <- colnames(dataSet)
  fieldsToKeep <- currentFields[which(!currentFields %in% fieldsToBeRemoved)]
  output <- dataSet[, fieldsToKeep]
  return(output)
}


