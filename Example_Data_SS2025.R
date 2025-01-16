# R
## Add R repo
sudo apt-key adv --keyserver keyserver.ubuntu.com --recv-keys
sudo add-apt-repository "deb https://cloud.r-project.org/bin/linux/ubuntu $(lsb_release -cs)-cran40/"


# Update Packages
sudo apt updatelibrary(labelled)
library(tidyverse)
save(bev_data, file = "bev_data.RData")
load("bev_data.RData")
head(bev_data)

# Klarnamen der Merkmale:
val_labels(bev_data$P03MR)

# Codierung einzelner Merkmale
var_label(bev_data$P05)


# Falls wir die Merkmalsnamen/Ausprägungen durch die Label ersetzen wollen
apply_labels <- function (x, var.labels = T, val.labels = T) 
{
  if (var.labels) {
    var_labels <- labelled::var_label(x)
    idx <- which(is.null(var_labels))
    var_labels[idx] <- colnames(x)[idx]
    colnames(x) <- var_labels
  }
  if (val.labels) {
    x[] <- lapply(x, function(column) if (labelled::is.labelled(column)) 
      labelled::to_factor(column)
      else column)
  }
  x
}

apply_labels(bev_data)

git remote set-url origin git@github.com:<username>/<repository>.git

sudo apt upgrade