image_func <- function(session, output, v, output_id, file_path){
  output[[output_id]] <- renderImage({
    # Formatting the string so that it corresponds to the lower_bound number:
    imagename <- sprintf(file_path, v$lower_bound)
    
    # Display the image
    list(src = imagename, contentType = 'image/png',
         height = "auto", width = "100%")
  }, deleteFile = FALSE)
}


