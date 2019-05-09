#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

refvars <<- read_excel('Labels.xlsx') %>% dplyr::select(1:2) %>% set_colnames(c('Var', 'Label')) %>% spread(Var, Label) %>% as.list

# Define UI for application that draws a histogram
shinyUI(fluidPage(
	includeCSS("styles.css"),

	titlePanel(refvars$app.title),

	sidebarLayout(
		sidebarPanel(
			class = 'sidebar d-print-none hidden-print',
			helpText(refvars$report.sel.text),
			uiOutput('Select_indagini'),
			br(),
			helpText(refvars$daterage.sel.text),
			uiOutput('Slider_data.range'),
			uiOutput('Select_reparti', class = 'fixed-top'),
			actionButton('makeReport', refvars$print, onClick = "window.print()"),
			width = 3
		),

		mainPanel(
			class = 'mainpanel',
			h2(textOutput("Indagine_current")),
			h3(textOutput("Reparto_current")),
			plotOutput('TrendPlot'),
			br(),
			br(),
			h4(refvars$report.prod.title),
			uiOutput('Produzione_report', container = p),
			conditionalPanel('output.Is_plot_freq == true', plotOutput('FreqPlot')),
			br(),
			h4(refvars$report.notes.title, class = 'page_break_before'),
			conditionalPanel(
				'output.Is_Appunti == false',
				p(refvars$report.notes.none)
			),
			conditionalPanel(
				'output.Is_Appunti == true',
				tagList(
					p(
						refvars$report.notes.have.notes, ' ', textOutput('Appunti_report', container = span)
						),
					uiOutput('Select_appunti'),
					conditionalPanel(
						'output.Have_Note == true',
						tagList(
							h5(refvars$notes),
							uiOutput('Tabella_Note'),
							br()
						)
					),
					conditionalPanel(
						'output.Have_Problemi == true',
						tagList(
							h5(refvars$problems),
							uiOutput('Tabella_Problemi'),
							br()
						)
					),
					conditionalPanel(
						'output.Have_Sforamenti == true',
						tagList(
							h5(refvars$non.compliance),
							uiOutput('Tabella_Sforamenti'),
							br()
						)
					)
				)
			),
			br(),
			br()
		)
	)
))