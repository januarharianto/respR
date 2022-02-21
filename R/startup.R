# Library load message
#' @importFrom utils packageDescription

.onAttach<- function (lib, pkg){
  if(interactive()){
    vers <- packageDescription("respR", fields = "Version")
    start_msg <- paste("
======================================================================
respR", vers, "
======================================================================

Please cite respR using the following:

Harianto, J., Carey, N., Byrne, M., 2019. respR - An R package for the
manipulation and analysis of respirometry data. Methods in Ecology and
Evolution, 10(6), 912-920. https://doi.org/10.1111/2041-210X.13162

Visit this site for help documentation and vignettes:

    http://bit.ly/respr_pkg

And follow respR on Twitter for latest news:

    http://twitter.com/respr_pkg

======================================================================")

    packageStartupMessage(start_msg)
  }
}
