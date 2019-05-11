#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)


# Define UI for application that draws a histogram
shinyUI(fluidPage(
	# includeCSS("styles.css"),
	# tags$script(type = "text/javascript", "
	# 	$(function() { // Run when DOM ready
	# 		$(window).bind('beforeunload', function(e) {
	# 			Shiny.onInputChange('quit', true); // sets input$quit to true
	# 			});
	# 			});
	# 			")

	titlePanel(refvars$app.title),

	sidebarLayout(
		sidebarPanel(
			class = 'sidebar d-print-none hidden-print',
			uiOutput('UI.report.sel.text'),
			uiOutput('Select_indagini'),
			br(),
			uiOutput('UI.daterange.sel.text'),
			uiOutput('Slider_date.range'),
			uiOutput('Select_reparti', class = 'fixed-top'),
			uiOutput('Print.report'),
			width = 3
		),

		mainPanel(
			class = 'mainpanel',
			h2(textOutput("Indagine_current")),
			h3(textOutput("Reparto_current")),
			plotOutput('TrendPlot'),
			br(),
			br(),
			uiOutput('UI.report.prod.title'),
			uiOutput('Produzione_report', container = p),
			conditionalPanel('output.Is_plot_freq == true', plotOutput('FreqPlot')),
			br(),
			uiOutput('UI.report.notes.title'),
			conditionalPanel(
				'output.Is_Appunti == false',
				uiOutput('UI.report.notes.none')
			),
			conditionalPanel(
				'output.Is_Appunti == true',
				tagList(
					p(
						textOutput('UI.report.notes.have.notes', container = span), ' ', textOutput('Appunti_report', container = span)
						),
					uiOutput('Select_appunti'),
					conditionalPanel(
						'output.Have_Note == true',
						tagList(
							uiOutput('UI.notes'),
							uiOutput('Tabella_Note'),
							br()
						)
					),
					conditionalPanel(
						'output.Have_Problemi == true',
						tagList(
							uiOutput('UI.problems'),
							uiOutput('Tabella_Problemi'),
							br()
						)
					),
					conditionalPanel(
						'output.Have_Sforamenti == true',
						tagList(
							uiOutput('UI.non.compliance'),
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