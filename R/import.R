#' @importFrom utils install.packages installed.packages

.install.if.not.installed = function(this.package)
{
  new.packages = this.package[!(this.package %in% utils::installed.packages()[,"Package"])]

  if (length(new.packages))
  {
    utils::install.packages(new.packages)
  }

  # if (length(this.package))
  # {
  #   for (i in 1:length(this.package))
  #   {
  #     library(this.package, character.only = T)
  #   }
  # }
}
