library(magick)
library(rgbif)
source(file = "Functions_W.R")
# 
max_filesize <- 900000 # in bytes (900 KB), limited to 1000000 bytes)
N_pp <- 50 # number of past posts image paths to remember
#parameters <- readLines("/home/nicolasmoiroux/taxobot_mail_param.txt") # report email parameter file

# Set image folder and exclusion file (last 'N_pp' posts)
subfolder <- paste0(getwd(),"/Planches") # where photos are stored
save_folder <- paste0(getwd(),"/Resized/") # where resized photos are stored
dir.create(save_folder) # create it if it does not exist
exclusion_file <- paste0(getwd(),"/past_posts.txt")


# Table of correspondence between genus names and genus abbreviations used in images filenames
lookup_table <- read.csv(file = "genus_lookup_table.csv", row.names = NULL)


jpg_files <- list.files(path = subfolder, pattern = "\\.jpg$", full.names = TRUE, recursive = TRUE, include.dirs = TRUE)


i <- 1

for (i in 1:length(jpg_files)){
  
  selected_image <- jpg_files[i]
  
  
  if (!is.null(selected_image)) {
    print(paste("Selected file:", selected_image))
    # load the image file
    img <- image_read(selected_image)
  } else {
    print("Can't select a file")
  }
  
  
  # Extract species and genus names
  ge_species <- extract_genus_and_species(selected_image)
  genus_abbreviation <- ge_species$genus_abbreviation
  species_name <- ge_species$species_name
  genus_name <- get_genus_name(genus_abbreviation, lookup_table)
  
  # Get gbif link to species & retrieve long species name 
  gbif_data <- name_backbone(name=paste(genus_name, species_name))
  gbif_link <- paste0("https://www.gbif.org/species/",gbif_data$usageKey[1])
  gbif_name <- gbif_data$scientificName[1]
  
  ## Resize image if necessary
  filename <- paste0(genus_name,"_",species_name,".jpg") # define file name of resized image
  
  if (file.exists(paste0(save_folder,filename))){ # check if the resized image already exists
    
    image_destroy(img)
    next
    
  } else {
    
    result_resize <- reduce_image_filesize(img, target_size = max_filesize) # resize the image
    
    img_post <- image_write(image = result_resize$image, # load and save resized image
                            path =  paste0(save_folder,filename), quality = result_resize$final_quality)
    image_destroy(img)
  }
}

  

