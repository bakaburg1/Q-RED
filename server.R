#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

refvars <<- read_excel('Labels.xlsx') %>% dplyr::select(1:2) %>% set_colnames(c('Var', 'Label')) %>% spread(Var, Label) %>% as.list
load.data()

# Define server logic required to draw a histogram
shinyServer(function(input, output) {

	#Sys.setlocale("LC_TIME", "it_IT")

	Data <- Data %>%
		group_by(Reparto, Indagine) %>%
		mutate(Diff = c(NA, time_length(diff(Data), unit = 'months')) %>% round) %>%
		ungroup()


	output$Select_indagini <- renderUI({
		print('output$Select_indagini')

		selectInput('Indagine_corrente', NULL, req(Data$Indagine) %>% unique() %>% sort)
	})

	output$Select_reparti <- renderUI({
		print('output$Select_reparti')

		lista_reparti <- req(Data.filtered())$Reparto %>% unique() %>% sort
		selectInput('Reparto_corrente', NULL, lista_reparti, selected = if (!isTruthy(input$Reparto_corrente)) lista_reparti[1] else input$Reparto_corrente)
	})

	output$Slider_data.range <- renderUI({
		print('output$Slider_data.range')

		sliderInput("DateRange",
								label = NULL,
								step = 1,
								sep = '',
								min = min(Data$Anno), max = max(Data$Anno), value = c(max(min(Data$Anno), max(Data$Anno) - 5), max(Data$Anno)))
	})

	loading <- reactive(if (is.null(input$DateRange)) T else F)

	Data.filtered <- reactive({
		print('Data.filtered')

		if (loading()) {
			Data
		} else {
			Data %>%
				filter(
					Indagine == input$Indagine_corrente,
					between(Anno, input$DateRange[1], input$DateRange[2]))
		}
	})

	output$Indagine_current <- renderText(input$Indagine_corrente)
	output$Reparto_current <- renderText(input$Reparto_corrente)

	Data.Reparto <- reactive({
		print('Data.Reparto')

		Data.filtered() %>% filter(Reparto == req(input$Reparto_corrente))
	})

	intervallo_anni <- reactive(input$DateRange[2] - input$DateRange[1] + 1)

	output$Produzione_report <- renderUI({
		print('output$Produzione_report')

		Data.Reparto <- req(Data.Reparto())

		n_rep = nrow(Data.Reparto)
		anni = intervallo_anni()
		ratio = signif(nrow(Data.Reparto) / intervallo_anni(), 3)

		glue(if (length(Data.Reparto$Diff) == 1) refvars$report.prod.par.single else refvars$report.prod.par.plural,
				 ' ', refvars$report.prod.par.num, ' ',
				 if (length(Data.Reparto$Diff) > 2) {
				 	out <- describe.continuous.var(Data.Reparto$Diff, as.string = F) %>% as.list()
				 	refvars$report.prod.par.timing
				 }
				 else '.') %>%
			HTML
	})

	output$TrendPlot <- renderPlot({
		print('output$TrendPlot')
		try({
			Data %>%
				filter(Indagine == input$Indagine_corrente) %>%
				mutate(Ok = is.na(Note) & is.na(Problemi) & is.na(Sforamenti), Ok = replace(Ok, Ok == F, NA)) %>%
				transmute(Data, Reparto, Indagine, Protocollo, Note, Problemi, Sforamenti, Ok) %>%
				gather(Tipo, Appunti, -Data, -Protocollo, -Reparto, -Indagine,) %>%
				mutate(
					Tipo = replace(Tipo, year(Data) < input$DateRange[1] | year(Data) > input$DateRange[2], NA) %>% replace(is.na(Appunti), NA),
					Tipo = factor(
						Tipo,
						levels = c('Ok', 'Note', 'Problemi', 'Sforamenti'),
						labels = c('Ok', refvars$notes, refvars$problems, refvars$non.compliance)
					)) %>%
				filter(!is.na(Tipo)) %>%
				group_by(Indagine, Reparto, Protocollo) %>%
				mutate(
					Label = format(Data, '%m/%y ') %>% str_c('\n', mapvalues(unique(Tipo), c('Ok', refvars$notes, refvars$problems, refvars$non.compliance), c('Ok', refvars$notes.tag, refvars$problems.tag, refvars$non.compliance.tag), warn_missing = F) %>% paste(collapse = '-')),
					Label = replace(Label, (1:n()) != 1 | Reparto != input$Reparto_corrente, NA)
				) %>%
				group_by(Indagine, Reparto) %>%
				arrange(Protocollo) %>%
				mutate(Pos = replace(Label, !is.na(Label), rep_len(c(3, -1), sum(!is.na(Label)))) %>% as.numeric()) %>%
				ungroup() %>%
				arrange(Data) %>%
				ggplot(aes(Data, Reparto %>% factor(levels = sort(Reparto, T) %>% unique()))) +
				geom_line(aes(size = Reparto == input$Reparto_corrente, alpha = Reparto == input$Reparto_corrente), color = 'steelblue') +
				geom_point(aes(shape = Tipo, color = Tipo, alpha = Reparto == input$Reparto_corrente), size = 5) +
				geom_label(aes(label = Label, vjust = Pos * .5), size = 4) +
				guides(shape = 'none', size = 'none', alpha = 'none') +
				coord_cartesian(xlim = c(dmy(paste0('1/1/', input$DateRange[1])), dmy(paste0('1/1/', input$DateRange[2])))) +
				scale_y_discrete(expand = expand_scale(add = 1.7)) +
				scale_x_date(date_breaks = if (intervallo_anni() > 3) '6 months' else if (intervallo_anni() == 4) '2 months' else '1 month', date_labels = '%b %y') +
				scale_color_manual(name = NULL, values = c('limegreen', 'darkturquoise', 'orange', 'red') %>% set_names(c('Ok', refvars$notes, refvars$problems, refvars$non.compliance))) +
				scale_shape_manual(name = NULL, values = c(16, 17, 18, 15) %>% set_names(c('Ok', refvars$notes, refvars$problems, refvars$non.compliance))) +
				scale_size_discrete(range = c(1,2)) +
				scale_alpha_discrete(range = c(0.4, .9)) +
				theme(
					legend.position = 'bottom',
					panel.grid.major.x = element_line(colour = 'grey92'),
					panel.grid.minor = element_line(colour = 'grey92'),
					axis.text.x = element_text(angle = 45, hjust = 1),
					axis.text.y = element_text(face = ifelse(unique(Data.filtered()$Reparto) %>% sort(T) == input$Reparto_corrente, 'bold', 'plain'))
				) +
				labs(y = NULL, x = NULL, shape = NULL)
		}, silent = T) %>% req
	})

	output$Is_plot_freq <- reactive({length(req(Data.Reparto())$Diff) > 2})

	output$FreqPlot <- renderPlot({
		print('output$FreqPlot')
		try({
			Data.Reparto() %>%
				filter(!is.na(Diff)) %>%
				ggplot(aes(Diff)) +
				geom_bar(fill = 'steelblue', width = .25) + labs(x = refvars$plot.lab.mothdiff, y = refvars$plot.lab.freq) +
				coord_cartesian(
					xlim = c(max(0, min(Data.Reparto()$Diff, na.rm = T) - 2), max(Data.Reparto()$Diff + 1, na.rm = T)),
					ylim = c(0, max(table(Data.Reparto()$Diff)) + 1)
				) +
				scale_x_continuous(breaks = function(x) 0:max(x)) +
				scale_y_continuous(breaks = function(x) 0:max(x))
		}, silent = T) %>% req
	})

	Is_Appunti <- reactive({
		print('Is_Appunti')
		print(str(Data.Reparto()))
		df <- req(Data.Reparto()) %>%
			transmute(Data = format(Data, '%b %Y'), Protocollo, Note, Problemi, Sforamenti) %>%
			gather(Tipo, Appunti, -Data, -Protocollo) %>%
			mutate(Tipo = replace(Tipo, Tipo == 'Sforamenti', 'Non conformità') %>% factor(levels = c('Note', 'Problemi', 'Non conformità'))) %>%
			filter(!is.na(Appunti))

		nrow(df) > 0
	})

	output$Is_Appunti <- req(Is_Appunti)

	Is_Appunti <- reactive({
		print('Is_Appunti')
		df <- req(Data.Reparto()) %>%
			transmute(Data = format(Data, '%b %Y'), Protocollo, Note, Problemi, Sforamenti) %>%
			gather(Tipo, Appunti, -Data, -Protocollo) %>%
			mutate(Tipo = factor(
				Tipo,
				levels = c('Note', 'Problemi', 'Sforamenti'),
				labels = c(refvars$notes, refvars$problems, refvars$non.compliance)
			)) %>%
			filter(!is.na(Appunti))

		nrow(df) > 0
	})

	output$Appunti_report <- renderText({
		print('output$Appunti_report')
		req(Data.Appunti()) %>% pull(Tipo) %>% droplevels() %>% describe.discrete.var(keep.order = T, invert = T) %>% str_remove_all(' ?\\([^\\(\\)]*\\)') %>% str_to_lower() %>% str_squish() %>% str_to_lower() %>% str_c('.')
	})

	Data.Appunti <- reactive({
		print('Data.Appunti')

		req({try({Data.Reparto() %>%
				transmute(Data = format(Data, '%b %Y'), Protocollo, Note, Problemi, Sforamenti) %>%
				gather(Tipo, Appunti, -Data, -Protocollo) %>%
				mutate(Tipo = factor(
					Tipo,
					levels = c('Note', 'Problemi', 'Sforamenti'),
					labels = c(refvars$notes, refvars$problems, refvars$non.compliance)
				)) %>%
				filter(!is.na(Appunti))}, silent = F)})
	})

	create_tabella_appunti <- function(which) {
		print('create appunti table')
		renderTable({
			req(Data.Appunti()) %>%
				filter(Tipo %in% which) %>%
				dplyr::select(-Tipo) %>%
				rename(!!(c('Data', 'Protocollo', 'Appunti') %>%
						set_names(refvars[c('note.table.data', 'note.table.id', 'note.table.note_type')]))
				)
		})
	}


	output$Have_Note <- reactive(refvars$notes %in% Data.Appunti()$Tipo)
	output$Tabella_Note <- create_tabella_appunti(refvars$notes)

	output$Have_Problemi <- reactive(refvars$problems %in% Data.Appunti()$Tipo)
	output$Tabella_Problemi <- create_tabella_appunti(refvars$problems)

	output$Have_Sforamenti <- reactive(refvars$non.compliance %in% Data.Appunti()$Tipo)
	output$Tabella_Sforamenti <- create_tabella_appunti(refvars$non.compliance)

	outputOptions(output, "Is_plot_freq", suspendWhenHidden = F)
	outputOptions(output, "Is_Appunti", suspendWhenHidden = F)
	outputOptions(output, "Have_Note", suspendWhenHidden = F)
	outputOptions(output, "Have_Problemi", suspendWhenHidden = F)
	outputOptions(output, "Have_Sforamenti", suspendWhenHidden = F)

})