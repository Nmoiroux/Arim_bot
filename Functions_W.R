
# select_random_image() function ----

#' Select a Random Image from a Folder, Excluding Recently Used Files
#'
#' This function selects a random `.jpg` file from a specified folder, 
#' excluding files listed in an exclusion file. The exclusion file keeps track 
#' of recently selected files to avoid repetition.
#'
#' @param subfolder Character string. Path to the folder containing `.jpg` files.
#' @param exclusion_file Character string. Path to the file that stores paths of 
#'   recently used images. If the file does not exist, no files are excluded.
#' @param N_last Integer. Number of recent files to exclude based on the exclusion 
#'   file. Defaults to 10.
#'
#' @return Character string. The path of the randomly selected `.jpg` file. 
#'   Returns `NULL` if no eligible files are found.
#'
#' @details 
#' - The function first retrieves all `.jpg` files in the specified folder, 
#'   including those in subdirectories.
#' - It excludes files listed in the last `N_last` lines of the exclusion file, 
#'   if the file exists.
#' - A file is randomly selected from the remaining eligible files. If no eligible 
#'   files are found, the function returns `NULL` and prints a message.
#'
#' @examples
#' # Example folder and exclusion file
#' subfolder <- "path/to/images"
#' exclusion_file <- "path/to/exclusion.txt"
#'
#' # Select a random image, excluding the last 5 used
#' selected_image <- select_random_image(subfolder, exclusion_file, N_last = 5)
#'
#' @export
select_random_image <- function(jpg_files, exclusion_file, N_last = 10) {

  # Check if exclusion file exists
  excluded_files <- character(0)
  if (file.exists(exclusion_file)) {
    # Read the exclusion file and get the last N_last lines
    exclusions <- scan(exclusion_file, character(), quote = "")
    excluded_files <- tail(exclusions, N_last)
  }
  
  # Exclude files listed in the exclusion file
  eligible_files <- setdiff(jpg_files, excluded_files)
  
  # If no eligible files remain, return NULL
  if (length(eligible_files) == 0) {
    message("No eligible files to select from.")
    return(NULL)
  }
  
  # Select a random file from the eligible files
  selected_file <- sample(eligible_files, 1)
  
  # Add the selected file to the exclusion file
  #write(selected_file, exclusion_file, append = TRUE)
  
  return(selected_file)
}

# extract_genus_and_species() function ----

#' Extract Genus Abbreviation and Species Name from a File Path
#'
#' This function extracts the genus abbreviation (first two characters) and 
#' the species name (text between the first underscore and the file extension) 
#' from a given file path.
#'
#' @param file_path Character string. The path of the file from which to extract 
#'   genus abbreviation and species name.
#'
#' @return A list with two elements:
#'   \describe{
#'     \item{\code{genus_abbreviation}}{Character string. The first two characters of the file name.}
#'     \item{\code{species_name}}{Character string. The species name extracted from the file name.}
#'   }
#'
#' @details
#' - The genus abbreviation is taken as the first two characters of the file name 
#'   (excluding the directory path and extension).
#' - The species name is extracted as the substring between the first underscore 
#'   and the file extension.
#'
#' @examples
#' # Example file path
#' file_path <- "path/to/Planches/An_gambiae.jpg"
#'
#' # Extract genus abbreviation and species name
#' result <- extract_genus_and_species(file_path)
#' print(result$genus_abbreviation) # "An"
#' print(result$species_name)       # "gambiae"
#'
#' @export
extract_genus_and_species <- function(file_path) {
  # Extract the file name from the path
  file_name <- basename(file_path)
  
  # Remove the file extension
  file_base <- tools::file_path_sans_ext(file_name)
  
  # Extract the first two characters as genus abbreviation
  genus_abbreviation <- substr(file_base, 1, 2)
  
  # Extract the species name (string between the first underscore and file extension)
  species_name <- sub("^[^_]*_([^_]+).*", "\\1", file_base)
  
  # Return results as a list
  return(list(
    genus_abbreviation = genus_abbreviation,
    species_name = species_name
  ))
}

#### Retrieve Full Genus Name from an Abbreviation - function ----

#' Retrieve Full Genus Name from an Abbreviation
#'
#' This function retrieves the full genus name corresponding to a given genus abbreviation 
#' by looking it up in a provided table of abbreviations and genus names.
#'
#' @param genus_abbreviation Character string. The genus abbreviation to look up 
#'   (e.g., the first two characters of a file name).
#' @param lookup_table Data frame. A table containing two columns:
#'   \describe{
#'     \item{\code{genus_abbreviation}}{Character string. The abbreviation of the genus (e.g., "An").}
#'     \item{\code{genus_name}}{Character string. The full genus name (e.g., "Anopheles").}
#'   }
#'
#' @return Character string. The full genus name corresponding to the provided abbreviation.
#'
#' @details
#' - The function checks whether the provided abbreviation exists in the lookup table.
#' - If the abbreviation is not found, the function raises an error.
#' - The corresponding genus name is returned as a character string.
#'
#' @examples
#' # Example lookup table
#' lookup_table <- data.frame(
#'   genus_abbreviation = c("An", "Ae"),
#'   genus_name = c("Anopheles", "Aedes")
#' )
#'
#' # Retrieve the full genus name for an abbreviation
#' genus_name <- get_genus_name("An", lookup_table)
#' print(genus_name) # Outputs: "Anopheles"
#'
#' # Attempting with an unknown abbreviation
#' \dontrun{
#' get_genus_name("Cx", lookup_table) # Will raise an error
#' }
#'
#' @export
get_genus_name <- function(genus_abbreviation, lookup_table) {
  # Check if the genus_abbreviation exists in the lookup_table
  if (!genus_abbreviation %in% lookup_table$genus_abbreviation) {
    stop("Genus abbreviation not found in the lookup table.")
  }
  
  # Extract the corresponding genus_name
  genus_name <- lookup_table$genus_name[lookup_table$genus_abbreviation == genus_abbreviation]
  
  return(as.character(genus_name)) # Ensure the result is a character string
}


# reduce_image_filesize() function ----

#' Reduce Image File Size Below a Target Size
#'
#' This function reduces the file size of an image by iteratively adjusting its 
#' quality and scaling it down until the size falls below a specified target.
#'
#' @param image_in An object of class \code{magick-image}. The input image to be resized and compressed.
#' @param target_size Integer. The maximum allowable file size in bytes. Defaults to \code{900000} (900 KB).
#' @param quality_step Integer. The decrement step for image quality during compression. Defaults to \code{5}.
#' @param resize_factor Numeric. The scaling factor to reduce image dimensions 
#'   (as a percentage, e.g., \code{0.95} corresponds to 95%). Defaults to \code{0.95}.
#'
#' @return A list with the following elements:
#'   \describe{
#'     \item{\code{image}}{An object of class \code{magick-image}. The resized and compressed image.}
#'     \item{\code{final_filesize}}{Integer. The final file size of the processed image in bytes.}
#'     \item{\code{final_quality}}{Integer. The final quality setting used for the image.}
#'   }
#'
#' @details
#' - The function starts with the original image and reduces its quality iteratively.
#' - If the target size is not reached by reducing quality, the image is scaled down 
#'   by the specified \code{resize_factor}.
#' - The process repeats until the file size is below \code{target_size} or the 
#'   quality reaches the minimum allowable value (defined by \code{quality_step}).
#'
#' @examples
#' library(magick)
#'
#' # Read an image
#' img <- image_read("path/to/image.jpg")
#'
#' # Reduce file size to below 800 KB
#' result <- reduce_image_filesize(img, target_size = 800000, quality_step = 10, resize_factor = 0.9)
#'
#' # Save the processed image
#' image_write(result$image, "path/to/resized_image.jpg", quality = result$final_quality)
#'
#' @export
reduce_image_filesize <- function(image_in, target_size = 900000, quality_step = 5, resize_factor = 0.95) {
  # Read the image
  img <- image_in
  
  # Initialize variables
  current_quality <- 100
  current_img <- img
  current_filesize <- image_info(img)$filesize[1]
  
  # Iteratively reduce quality and size
  while (current_filesize > target_size && current_quality > quality_step) {
    # Adjust quality
    print(paste(current_quality,current_quality, current_filesize))
    current_quality <- max(current_quality - quality_step, quality_step)
    temp_img <- image_write(current_img, path = NULL, format = "jpeg", quality = current_quality)
    
    # Save the temporary image to a file to measure its size
    temp_path <- tempfile(fileext = ".jpg")
    writeBin(temp_img, temp_path)
    temp_filesize <- file.info(temp_path)$size
    
    # If still too large, resize the image
    if (temp_filesize > target_size) {
      current_img <- image_scale(current_img, paste0(resize_factor * 100, "%"))
      temp_img <- image_write(current_img, path = NULL, format = "jpeg", quality = current_quality)
      writeBin(temp_img, temp_path)
      temp_filesize <- file.info(temp_path)$size
    }
    
    # Update current values
    current_filesize <- temp_filesize
  }
  
  # Return final information
  return(list(
    image = current_img,
    final_filesize = current_filesize,
    final_quality = current_quality
  ))
}

#### send by email results of posting - function ----

#' Send an Email Notification
#'
#' This function sends an email report about the status of a post using SMTP. 
#' If the post is successful, the email includes the post content; otherwise, it reports an error.
#'
#' @param parameters Character vector. A vector containing email configuration parameters in the following order:
#'   \describe{
#'     \item{\code{[1]}}{Sender email address.}
#'     \item{\code{[2]}}{Recipient email address.}
#'     \item{\code{[3]}}{SMTP server address (e.g., "smtp.example.com").}
#'     \item{\code{[4]}}{Password for the sender email account.}
#'   }
#' @param post A character vector or \code{try-error} object. The result of a post operation, either a success message 
#'   or an error object.
#'
#' @return Sends an email and returns \code{NULL}. The function is used for its side effects.
#'
#' @details
#' - If the \code{post} object is a \code{try-error}, the subject of the email will indicate an error.
#' - If the post is successful, its content will be included in the email body.
#' - The function uses the \code{curl::send_mail()} function for sending emails via SMTP.
#'
#' @examples
#' # Example parameters
#' parameters <- c(
#'   "sender@example.com",
#'   "recipient@example.com",
#'   "smtp.example.com",
#'   "password123"
#' )
#'
#' # Example usage
#' post <- "Post content goes here."
#' send_me_mail(parameters, post)
#'
#'
#' @export
send_me_mail <- function(parameters, post){
  email_par <- parameters
  sender <- email_par[1]
  recipients <- email_par[2]
  server <- email_par[3]
  password <- email_par[4]
  username <- sender
  #subject <- "Post valid"
  if (class(post) == "try-error"){
    subject <- "Post error"
  } else {
    subject <- "Post valid"
    post <- "Post valid" #paste0(post, collapse = '\n')
  }

  message <- paste0("From: ",sender,"
To: ",recipients,"
Subject: Report of taxobot: ",subject,"

",post,"")
  
  curl::send_mail(
    sender,
    recipients,
    message,
    smtp_server = server,
    username = username,
    password = password
  )
  }


