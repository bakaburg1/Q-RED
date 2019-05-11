pkg.require <- function(pkgs, load = T){
	installed <- utils::installed.packages()

	not.installed <- c()

	for (pkg in pkgs) {
		if (!(pkg %in% installed)) not.installed <- c(not.installed, pkg)
	}

	if (length(not.installed) > 0) utils::install.packages(not.installed)

	if (load) for (pkg in pkgs) library(pkg, character.only = TRUE)
}



.First.sys <- function () 
{
    for (pkg in getOption("defaultPackages")) {
        res <- require(pkg, quietly = TRUE, warn.conflicts = FALSE, 
            character.only = TRUE)
        if (!res) 
            warning(gettextf("package %s in options(\"defaultPackages\") was not found", 
                sQuote(pkg)), call. = FALSE, domain = NA)
    }

    pkg.require(c('shiny'))

    shiny::runApp(launch.browser = T)
}
