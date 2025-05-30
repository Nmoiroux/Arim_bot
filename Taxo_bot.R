library(atrrr)
library(magick)
library(rgbif)
source(file = "Functions_W.R")
# 
max_filesize <- 900000 # in bytes (900 KB), limited to 1000000 bytes)
N_pp <- 50 # number of past posts image paths to remember
parameters <- readLines("/home/nicolasmoiroux/taxobot_mail_param.txt") # report email parameter file

# Set image folder and exclusion file (last 'N_pp' posts)
save_folder <- paste0(getwd(),"/Resized/") # where resized photos are stored
dir.create(save_folder) # create it if it does not exist
jpg_files <- scan(paste0(getwd(),"/jpg_files.txt"), character(), quote = "")
exclusion_file <- paste0(getwd(),"/past_posts.txt")

# Table of correspondence between genus names and genus abbreviations used in images filenames
lookup_table <- read.csv(file = "genus_lookup_table.csv", row.names = NULL)

# select at random one file path among .jpg files in the image subfolder, excluding the paths stored in the last 'N_last' lines of the exclusion file (if it exists)
selected_image <- select_random_image(jpg_files = jpg_files, exclusion_file = exclusion_file, N_last = N_pp)

# Extract species and genus names
ge_species <- extract_genus_and_species(selected_image)
genus_abbreviation <- ge_species$genus_abbreviation
species_name <- ge_species$species_name
genus_name <- get_genus_name(genus_abbreviation, lookup_table)

# Get gbif link to species & retrieve long species name 
gbif_data <- name_backbone(name=paste(genus_name, species_name))
gbif_link <- paste0("https://www.gbif.org/species/",gbif_data$usageKey[1])
gbif_name <- gbif_data$scientificName[1]

filename <- paste0(genus_name,"_",species_name,".jpg") # define file name of resized image

if (file.exists(paste0(save_folder,filename))){ # check if the resized image already exists
  
  img_post <- paste0(save_folder,filename)
  
} else {
  print("Can't select a file")
}


## fill Bluesky post elements
#english
text <- paste0("🦟 This week, discover the unique ornaments of ",
               gbif_name, 
               " ⬇️!\nExplore details and distribution on GBIF🌱: ",
               gbif_link,
               " Fascinating diversity of #mosquitoes !\n\nPhotos by @nil-rahola.bsky.social \n\n#Entomology #Biodiversity 🧪🌐")

images_alt <- paste0("A photographic plate of ",gbif_name) 

#french
text_fr <- paste0("🦟 Cette semaine, découvrez les ornementations uniques de ",
               gbif_name, 
               " ⬇️!\nVoir les détails et la distribution sur le GBIF🌱: ",
               gbif_link,
               " .\nFascinante diversité des #moustiques !\n\nPhotos de @nil-rahola.bsky.social\n\n#Entomologie #Biodiversité")

images_alt_fr <- paste0("Une planche photographique de ",gbif_name) 


# Send the post
post_fr <- try(post_skeet(
  text = text_fr, 
  image = img_post, # selected_image 
  image_alt = images_alt_fr,
  verbose = TRUE
))

#send_me_mail(parameters, post_fr) # send mail with report of posting

Sys.sleep(3) # pause between post sending

post <- try(post_skeet(
  text = text, 
  image = img_post,
  image_alt = images_alt,
  verbose = TRUE
))

#send_me_mail(parameters, post) # send mail with report of posting

# after post is confirmed
if (class(post) != "try-error" & class(post_fr) != "try-error"){
  write(selected_image, exclusion_file, append = TRUE) # add posted file path to exclusion_file
}
