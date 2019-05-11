pkg.require(c('plyr'), load = F)
pkg.require(c('magrittr', 'tidyverse', 'lubridate', 'glue', 'broom', 'knitr', 'rmarkdown', 'readxl', 'scales', 'Hmisc', 'shiny'))

big.mark.opt <- ' '
mapvalues <- plyr::mapvalues
refvars <- read_excel('Labels.xlsx') %>% dplyr::select(1:2) %>% set_colnames(c('Var', 'Label')) %>% mutate_all(str_squish) %>% spread(Var, Label) %>% as.list

plots.theme = theme_minimal() +
	theme(
		axis.text = element_text(size = 12, colour = 'dimgray'),
		#axis.text.x = element_text(face = "bold", size = 15, colour = 'dimgray'),
		axis.title = element_text(face = "bold", size = 15, colour = 'dimgray'),
		axis.title.x = element_text(margin = ggplot2::margin(20, 0, 0, 0)),
		axis.title.y = element_text(margin = ggplot2::margin(0, 20, 0, 0)),
		#axis.title.y = element_blank(),
		axis.ticks.y = element_blank(),
		panel.grid.major.y = element_line(colour = "lightgray", size = .5),
		panel.grid.minor.y = element_line(colour = "lightgray", size = .25),
		panel.grid.major.x = element_blank(),
		panel.grid.minor.x = element_blank(),
		panel.border = element_blank(),
		plot.margin = unit(c(1,1.5,1,1), 'cm'),
		plot.title = element_text(face = "bold"),
		#legend.position = 'none',
		legend.key.width = unit(3,"line"),
		legend.title = element_text(size = 14, face = "bold")
	)

ggplot2::theme_set(plots.theme)

describe.discrete.var <- function(var, as.string = T, join = T, keep.order = F, exclude = NULL, asim = F, sep = ', ', final.sep = ' e ', invert = F, CIs = F, na.rm = F, ...) {

	if (na.rm) var <- na.exclude(var)

	var.length <- length(var)

	counts <- table(var)

	if ('logical' %in% class(var) & n_distinct(var) == 1) {
		counts[as.character(!var[1])] <- 0
		counts <- counts[c('FALSE', 'TRUE')]
	}
	if ('ordered' %nin% class(var) & keep.order == F) counts <- sort(counts, T)

	ratios <- lapply(counts, function(count) binom.test(count, var.length) %>% tidy) %>%
		do.call(what = 'rbind') %>%
		mutate_at(vars(estimate, conf.low, conf.high), percent)

	#ratios.est <- ratios$estimate %>% sapply(percent)

	if (CIs) {
		if (as.string) ratios.ci <- sprintf('; CI95%%: %s - %s', ratios$conf.low, ratios$conf.high)
		else ratios.ci <- sprintf('[%s - %s]', ratios$conf.low, ratios$conf.high)
	}
	else {
		ratios.ci <- ''
	}

	if (asim == F) {

		#var <- drop.levels(var[var %nin% exclude])

		if (as.string) {
			if (invert) cases <- sprintf("%s: %s (%s%s)", names(counts), format(counts, ...), ratios$estimate, ratios.ci)
			else cases <- sprintf("%s (%s%s) %s", format(counts, ...), ratios$estimate, ratios.ci, names(counts))

			if (join & length(cases) > 1) {
				cases <- paste(head(cases, -1), collapse = sep) %>% paste(tail(cases, 1), sep = final.sep)
			}

			cases <- cases %>% str_replace_all(' +', ' ')
		}
		else {
			cases <- data.frame(case = names(counts), count = unclass(counts), estimate = ratios$estimate, stringsAsFactors = F, row.names = NULL)
			if (CIs) cases$CIs <- ratios.ci
		}

		cases
	}
	else {
		sprintf('%s (%s%s)', format(tail(counts, 1), ...), tail(ratios$estimate, 1), tail(ratios.ci, 1)) %>% str_replace(fixed('NA (NA)'), '0 (0%)')
	}
}

describe.continuous.var <- function(var, as.string = T, CIs = F, sep.ext = ', ', sep.int = ' - ', scientific = F, big.mark = big.mark.opt, digits = 3, ...) {

	if (all(is.na(as.numeric(var)))) stop('Variable type not numeric of similar')

	#if (is.null(big.mark)) big.mark <- big.mark.opt

	var.sum <- summary(var)

	if (getOption('OutDec') == ',') sep.ext = '; '

	if (CIs) {
		mean.cis <- smean.cl.normal(var, na.rm = T)
		if (as.string) mean.cis <- sprintf('CI95%%: [%s%s%s]%s',
																			 mean.cis['Lower'] %>% format(digits = digits, scientific = F, big.mark = big.mark, ...),
																			 sep.int,
																			 mean.cis['Upper'] %>% format(digits = digits, scientific = F, big.mark = big.mark, ...),
																			 sep.ext)
		else mean.cis <- sprintf('[%s%s%s]',
														 mean.cis['Lower'] %>% format(digits = digits, scientific = F, big.mark = big.mark, ...),
														 sep.int,
														 mean.cis['Upper'] %>% format(digits = digits, scientific = F, big.mark = big.mark, ...))
	}
	else {
		mean.cis <- ''
	}

	if (as.string) {
		sprintf('media: %s ± %s%s%smediana: %s (IQR: %s%s%s)',
						var.sum['Mean'] %>% format(digits = digits, scientific = F, big.mark = big.mark, ...),
						sd(var, na.rm = T) %>% format(digits = digits, scientific = F, big.mark = big.mark, ...),
						sep.ext,
						mean.cis,
						var.sum['Median'] %>% format(digits = digits, scientific = F, big.mark = big.mark, ...),
						var.sum['1st Qu.'] %>% format(digits = digits, scientific = F, big.mark = big.mark, ...),
						sep.int,
						var.sum['3rd Qu.'] %>% format(digits = digits, scientific = F, big.mark = big.mark, ...))
	}
	else {
		out <- c(
			Mean = var.sum['Mean'] %>% format(digits = digits, scientific = F, big.mark = big.mark, ...),
			SD = sd(var, na.rm = T) %>% format(digits = digits, scientific = F, big.mark = big.mark, ...),
			Median = var.sum['Median'] %>% format(digits = digits, scientific = F, big.mark = big.mark, ...),
			IQR = sprintf('%s%s%s', var.sum['1st Qu.'] %>% format(digits = digits, scientific = F, big.mark = big.mark, ...), sep.int, var.sum['3rd Qu.'] %>% format(digits = digits, scientific = F, big.mark = big.mark, ...))
		) %>%
			set_names(c('Mean', 'SD', 'Median', 'IQR'))

		if (CIs) out <- c(out[1:2], c(CIs = mean.cis), out[3:4])

		out
	}

}

load.data <- function() {
	if ('Data' %in% ls()) rm(Data)

	#refvars <- read_excel('Labels.xlsx') %>% dplyr::select(1:2) %>% set_colnames(c('Var', 'Label')) %>% mutate_all(str_squish) %>% spread(Var, Label) %>% as.list

	tryCatch(read_excel("Report_data.xlsx"), error = function(e) file.choose()) %>%
		rename(!!c(
			Anno = refvars$year,
			Mese = refvars$month,
			Indagine = refvars$report,
			Reparto = refvars$context,
			Note = refvars$notes,
			Problemi = refvars$problems,
			Sforamenti = refvars$non.compliance,
			Protocollo = refvars$id
		)) %>%
		mutate(
			Data = ymd(paste(Anno, Mese, '1', sep = '-'))
		)
}

gen.fake.data <- function() { # To generate fake datasets
	pkg.require(c('WriteXLS'))

	add.notes <- function(type, p, n) {
		x <- sample(c(type, NA), size = n, prob = c(p, 1 - p), replace = T)

		x[!is.na(x)] <- paste(type, 1:sum(!is.na(x)))

		x
	}

	lapply(paste('Indagine', 1:5), function(Ind) {
		lapply(paste('Reparto', 1:6), function(Rep){
			months <- rlnorm(100, meanlog = log(6), sdlog = log(1.4)) %>% round %>% cumsum()
			dates <- paste(1, sample(1:12, 1), sample(2007:2010, 1)) %>% dmy %>% {. + months(months)} %>% purrr::keep(~ .x <= dmy('1/1/2019'))

			tibble(
				Anno = year(dates),
				Mese = month(dates),
				Indagine = Ind,
				Reparto = Rep,
				Note = add.notes('Note', .5, length(dates)),
				Problemi = add.notes('Problema', .05, length(dates)),
				`Non conformità` = add.notes('Non conformità', .1, length(dates)),
				Protocollo = paste0(LETTERS[which(Ind %in% paste('Indagine', 1:5))], 1:length(dates)),
			)
		}) %>% bind_rows()
	})  %>% bind_rows() %>%
		mutate_all(~ str_replace_all(.x, c('Indagine' = 'Report', 'Reparto' = 'Unit', 'Nota' = 'Note', 'Problema' = 'Problem', 'Non conformità' = 'Non-conformity'))) %>%
		set_colnames(c('Year', 'Month', 'Report', 'Unit', 'Notes', 'Problems', 'Non-conformity', 'Protocol ID')) %>%
		#set_colnames(c('Anno', 'Mese', 'Indagine', 'Reparto', 'Note', 'Problemi', 'Non conformità', 'ID Protocollo')) %>%
		WriteXLS('Fake data.xlsx')
}