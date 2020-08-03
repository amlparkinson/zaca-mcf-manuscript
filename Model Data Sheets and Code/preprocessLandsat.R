preprocessLandsat <- function(inputPath, 
                         allBands = TRUE, 
                         maskClouds = TRUE, 
                         writeFile = TRUE, 
                         toMemory = FALSE, 
                         fileFormat = "GTiff"){
  
  require(raster)
  
  setwd(inputPath)
  
  ### Iterate over every image subdirectory
  
  for (dir in list.dirs(path = ".", full.names = FALSE)[nchar(list.dirs(path = ".", full.names = FALSE)) > 0]){
    
    setwd(paste(inputPath, "/", dir, sep = ""))
    
    ### Create Raster Brick
    
    #List image files that are actually Landsat bands
    band_list <- list.files(path = ".", glob2rx("*band*.tif$"))
    
    #Drop aerosol band for Landsat 8
    if(substr(band_list[1], 1,4) == "LC08"){
      band_list <- band_list[-1]
      print("Landsat 8")
    } else {print("Not Landsat 8")}
    
    print(paste("Processing ", substr(band_list[1], 18, 25), sep = ""))
    
    print(band_list)
    
    #Merge bands into a brick
    image_brick <- stack(band_list)
    
    #Set NA value for output
    NAvalue(image_brick) <- -9999
    
    ### Overwrite Invalid Pixel Values
    
    #Set all bad retrievals to NA
    image_brick <- reclassify(image_brick, c(-Inf, 0, NA))
    image_brick <- reclassify(image_brick, c(10000, Inf, NA))
    
    #Set all bands to NA if any band is NA for a pixel
    if(allBands == TRUE){
      
      vals <- values(image_brick)
      vals[rowSums(is.na(vals)) > 0, ] <- NA
      image_brick <- setValues(image_brick, vals)
      
    }
    
    ### Mask Clouds
    
    if(maskClouds == TRUE){
      
      pixel_qa_band_name <- list.files(path = ".", glob2rx("*pixel_qa*.tif$"))
      
      pixel_qa_band <- raster(pixel_qa_band_name)
      
      #Mask medium and high confidence clouds
      image_brick[pixel_qa_band %in% c(130, 132, 136, 144, 160, 176, 224)] <- NA
      
    }
    
    ### Export Output
    
    setwd(inputPath)
    
    if(writeFile == TRUE){
      
      writeRaster(image_brick, 
                  substr(band_list[1], 1, nchar(band_list[1]) - 10), 
                  format = fileFormat, 
                  NAflag = -9999)
      
    }
    
    if(toMemory == TRUE){
      
      assign(substr(band_list[1], 1, nchar(band_list[1]) - 10), 
             image_brick,
             envir = .GlobalEnv)
      
    }
    
    print("Image completed")
    
  }
}