library(shiny)
library(shinydashboard)
library(shinyWidgets)

library(ggplot2)
library(grid)

source("lioapi.R")
source("plots.R")
set_lytics_theme()

#
#  UI
#

ui <- dashboardPage(
	dashboardHeader(title = tags$span(
		tag("svg", list(class = "logo", xmlns = "http://www.w3.org/svg", viewBox = "80 -24 150 140", "enable-background" = "new 0 0 183.7 104.3", tag("path", list(fill = "#FFFFFF", d = "M120.6 32.1h.3c4-1.1 6.5-2.9 6.2-8.3-.1-2-.8-3-1.9-3-2 0-3.6 3.9-3.8 4.3l-.3.6c-1 2.2-1.9 4.3-1.4 5.7 0 .5.5.7.9.7zM19.8 9.9c-1.5-.7-3.2.5-3.1 2.1l.1.5c.1 1 .6 1.9 1.3 2.6 3.8 3.8 15 12.4 21.7 12.4h.6c.2 0 .4.2.4.4-1.3 9.7-3.7 18.6-5.6 22.5-.8 1.6-1.7 3.3-2.7 3.3-.2 0-.5-.1-.7-.2-.9-.4-2.1-1.2-3.3-1.9-1.7-1-3.4-2.1-4.8-2.6-4.6-1.9-7.6-3.1-10.8-3.5-5.3-.5-11.7 1.7-12.7 7.2-.9 4.5 1.7 10.3 5.5 12.7 5.3 3.3 10.1 4.9 14.5 4.9 3.7 0 7.2-1.2 10.5-3.5.3-.2.7-.6 1.3-1.1.7-.6 2.3-2 3.2-2.6.1-.1.3-.1.4 0 .5.3.9.8 1.4 1.2.6.5 1.2 1 1.7 1.4 3.5 2.5 7.4 3.6 9.9 3.8.6 0 1.1.1 1.6.1 2.7 0 5.1-.6 7.3-1.9.1-.1.3-.1.4.1 1.5 1.5 3.1 1.9 4.5 1.9 4.9 0 10.5-5.2 14.1-9.3 0 .1 0 .2-.1.3-.5 2.8-1.1 5.4-2.3 7.6-.8 1.3-1.9 2.6-3 3.8-.5.5-.9 1-1.3 1.5-1.2 1.4-5.2 6.2-8.5 11.5-2.5 4-4.7 8.3-3.7 12.3.6 2.3 2.1 4.1 4.6 5.6 1.6.9 3.2 1.4 4.7 1.4 5.8 0 9.1-6.8 10.5-10.9 1.5-4.4 1.9-7.6 2.3-11.4.2-1.7.4-3.4.7-5.5.1-.5.2-1.1.2-1.7.3-2.5.6-4.8 2.2-6.3 1.8-1.6 3.6-3.3 5.3-4.9l.3-.3c.7-.6 1.4-1.3 2-1.9l.8-.7c.2-.2.5-.1.6.2.4 1.8 1.1 3.3 2 4.6 1.8 2.4 4.5 3.8 7.6 3.8 2.8 0 5.6-1.2 7.7-3.2.3-.3.8-1 1.5-2 .1-.2.4-.2.6 0 .4.5.8 1 1.3 1.4 2.3 1.8 4.1 2.6 5.7 2.6 2.4 0 4.3-1.7 6.9-4.3.2-.2.5-.6.8-1.1.1-.2.5-.2.6 0l.6 1.1c2 3 5.1 5 8.7 5.5 1.5.2 2.8.3 4.1.3 5 0 8.8-1.7 12.5-5.9.1-.2.4-.2.5 0 .7 1 1.5 1.9 2.3 2.6 2.3 2 6 3.4 9.3 3.4 4.3 0 7.4-2.3 8.5-6.3.7-2.6.7-2.6 3.4-3.6 1.9-.7 6.3-2.7 6.8-6.1.1-.8 0-1.4-.4-1.9-.2-.3-.7-.6-1.5-.6-1.5 0-3.7 1.2-5.8 2.3-.9.5-2.1 1.1-2.7 1.3-.1.1-.3 0-.4-.1-.2-.3-.4-.9-.7-1.9-.1-.4-.2-.8-.3-1.1-1.4-3.4-3.3-6.9-4.9-9.8-.2-.4-.5-1-.7-1.6-.4-1.1-.9-2.3-1.7-3.1-.7-.7-2.9-1.3-4.3-1.3-1.5 0-2 .6-2.2 1.2-.4 1 0 1.9.2 2.6.1.3.2.5.2.7.1.4 0 1.2-.2 2.1-.3 1.6-1.2 3-1.9 4.1-.1.1-.2.3-.4.5-.8 1.1-1.5 1.8-1.9 2.5-.3-.1-.5-.2-.8-.2-.4 0-.9.1-1.4.7-.9 1-1.2 3.1-1.3 4.7 0 .1 0 .1-.1.2-.7.9-1.4 1.9-2.1 3.2-.4.7-.9 1.4-1.2 1.8-1.1 1.4-3.7 2.9-6.9 2.9-2.4 0-4.8-.9-6.8-2.7-1-.9-1.5-2.2-1.6-3.8-.1-3.9 2.7-9.3 5.4-11.9 1.4-1.4 2.9-2.2 4-2.2.3 0 .6.1.9.2 1.1.6 1.1.9.2 2.6-.6 1.1-1.2 2.3-.8 3.6.1.4.3.7.7.8.8.4 1.3.7 2 1.1.6.3 1.3.2 1.8-.3 2.7-3 3.6-6.6 2.3-9.5-1.3-3-4.8-4.5-9.1-3.8-6.2 1-11.9 6.9-13.7 14.2-.1.4-.2.8-.3 1.3v.1c-.3.6-.7 1.2-1.1 1.9-1.4 2.6-3.2 5.8-5.5 6.7-1.1.4-1.6.5-1.9.5h-.2c-.2-.4.1-2 .3-3l.2-1.1c.5-2.5 1.3-4.9 2.2-7.2 1.1-3 2.3-6.1 2.6-9.6.1-1.1-.3-2.3-1.1-3.1-.7-.7-1.5-1.2-2.4-1.2-.9 0-1.8.5-2.3 1.4-.1.1-5 9.7-6 17.7 0 .1 0 .1-.1.2l-.8 1c-1.9 2.5-4.3 5.7-6.6 6.6-1.5.6-2.7.3-3.4-.7-.8-1.1-1.1-3.1-.6-4.8 1-3.5 1.9-7.1 2.7-10.6v-.1c1.4-3.2 2.4-6.6 3.4-10 0-.1.1-.2.3-.2 2.4-.4 4.8-.8 7.1-1.2.8-.1 1.6-.2 2.4-.2l1.8-.1c.5-.1 1-.4 1.2-.9.3-.8.3-1.7-.2-2.5-.8-1.3-2.6-2.1-3.9-2.1h-.2c-1.7 0-3.6.1-5.6.2-.2-.2-.2-.2-.4-.5 1-2.9 1.6-5.3 1.6-7.6 0-1.8-.2-3.1-.9-4.2-.9-1.5-2.7-2.4-4.2-1.9-2 .6-2.8 2.9-3.2 4.2-.5 1.4-.8 2.9-1.2 4.5v.1c-.7 1.8-1.6 3.8-2.4 6l-.8.6-.6.1c-.4 0-1 .1-1.7.1-2.2.2-5.6.4-7.3.9-1.3.4-2.2 1.1-2.5 2.2-.1.4-.2 1.1.2 1.9.8 1.4 2.7 2.5 3.8 2.8.4.1.8.2 1.4.2 1.4 0 3-.3 4.5-.7l.4-.1-1 3.6c-.6 2.3-1 4.5-1.3 6.5v.1c-.8 1.5-1.7 2.9-2.6 4.2-1.4 2-3.3 3.7-5.1 5.4l-.8.7c.4-5.6 1-9.2 2.1-15.1.1-.5-.1-1-.5-1.3l-1.9-1.7c-.3-.2-.6-.3-.9-.3h-.7c-.4 0-.7.2-.9.5-1.4 2.6-2.5 4.8-3.5 7-1.6 3.4-3.2 6.7-5.8 10.4-1.1 1.6-3.7 2.9-5.7 2.9-.9 0-1.5-.2-2-.6-.1-.1-.1-.1-.1-.2l-.3-1.2c-.1-.3-.3-.5-.4-.7v-.3c-.2-4.7 2.4-14.2 4.3-18.9.6-1.4 1.3-3.4.4-4.7-.5-.7-1.3-1.1-2.5-1.1h-.7c-3.3.3-6.4 7.6-6.4 7.7-1.8 4.3-3.6 12.7-2.9 18.7 0 .1 0 .3-.2.3-1.8 1.1-3.9 1.8-5.7 1.6-2.1-.2-4.8-.7-9.7-4.9-.1-.1-.2-.3-.1-.4 5.7-11.7 7.3-20.2 8.1-27.1.1-.8.2-2.2.8-2.5.5-.2 1.8-.3 2.4-.3h.4c2.9-.2 5.4-.5 7.8-1 3.6-.8 7.4-2.5 11-4.9 3.2-2.2 7.2-5.8 7.6-10.6.2-2.6-.6-5.1-2.4-7.1-2.1-2.4-5.4-3.7-8.6-3.4-5.8.5-11.2 2.9-14.8 6.5-2.4 2.4-5.2 6.2-6.9 8.9-.4.6-.7 1.3-1 2-.4 1.4-1.3 3.2-2.3 3.3-5.9.3-14.7-6.2-17.9-8.9-.7-.6-1.4-1-2.2-1.5l-1.1-.6zm45-3.3c.3-.1.6-.2 1-.3 2.5-.6 4.7-.3 5.4.7.3.4.4 1 .3 1.7-.4 2.3-2 4.5-4.6 6.5-4.5 3.4-10.5 4.7-15.7 5.3-.3 0-.5-.3-.3-.5 2.6-5 7-10.7 13.9-13.4zm-48.4 56.5c-3.7-.2-8.1-3-9.5-6.1-.6-1.4-.6-2.7.3-3.8.8-.8 2-1.2 3.6-1.2 5.6 0 14.2 4.8 17.9 7 .2.1.2.3.1.5-2 2.8-6 3.9-12.4 3.6zm57.1 18.2c-.5 2.7-1 5.5-1.5 7.6-.9 4-2.3 6.7-6.2 7.9h-.1c-.4.1-.7.1-1 .1-.9 0-1.1-.3-1.1-.4-.3-.5-.7-2.7 4.8-10.9l.2-.3c1.4-2.1 3.1-4.4 4.6-6.3.2-.3.7-.1.6.3-.1.6-.2 1.3-.3 2zm88.6-32.6h.6c2 2.5 3.3 4.8 4.3 7.5.1.2-.1.5-.3.5-2 0-4.1-.4-5.8-1.3-.7-.3-1.1-.7-1.2-1.2-.3-1.5 1.2-3.7 2.4-5.5zm5.2 14.1c-.1.7-.5 1.7-2.5 1.7-1.8 0-3.8-.8-4.6-1.4-.8-.6-1.4-1.8-1.8-3-.1-.3.2-.6.5-.4 2 1.1 4.6 2.4 6.8 2.6h1.7c0 .1-.1.3-.1.5z"))))
	)),
	dashboardSidebar(
		textInput("lioAid", "Lytics Account ID", value = Sys.getenv("LIOAID"), placeholder = "(e.g. 1234)"),
		textInput("lioKey", "Lytics API Key", value = Sys.getenv("LIOKEY"), placeholder = "(e.g. aibBQUvU8cz39jmqo0j6tQxx)"),
		div(class = "form-group shiny-input-container", style = "height: 60px;",
			div(style = "float: left;",
				actionButton("credentials", "Update!", class = "btn-primary", style = "margin: 0; color: #fff;")
			),
			div(style = "float: right;",
				actionButton("refresh", "Reload", class = "btn-primary", icon = icon("redo"), style = "margin: 0; color: #fff;")
			)
		),
		br(),
		div(class = "form-group shiny-input-container",
			tags$label("Views")
		),
		#sidebarSearchForm(textId = "aid", buttonId = "updateAid", label = "Update AID"),
		sidebarMenu(
			menuItem("Model Summary", tabName = "summary", icon = icon("line-chart")),
			menuItem("Model Creation", tabName = "build", icon = icon("flask"))
		)
	),
	dashboardBody(
		tags$head(tags$style(HTML('body, .main-header .logo, .h1, .h2, .h3, .h4, .h5, .h6, h1, h2, h3, h4, h5, h6 {font-family: "Helvetica Neue", sans-serif !important;} .h1, .h2, .h3, .h4, .h5, .h6, h1, h2, h3, h4, h5, h6 { font-weight: 800; } .skin-blue .left-side, .skin-blue .main-sidebar, .skin-blue .wrapper { background-color: #262d37; }'))),
		tags$head(tags$style(HTML('.wrapper { overflow: visible !important; }'))),
		tags$head(tags$style(HTML('#shiny-notification-panel { width: 650px; } tfoot { display: none; } #modelTitle { font-weight: 800; font-size: 24px; line-height: 48px; padding-left: 0.5em;} .btn-dropdown-input { float: left; } '))),
		tags$head(tags$style(HTML('.denominator { font-size: 0.5em; bottom: 0.25em; position: relative; opacity: 0.7; }'))),
		# title header and filters
		fluidRow(
			style = "margin-top: -36px",
			box(width = NULL,
				# description
				fluidRow(
					column(12,
						tags$h1("SegmentML Dashboard"),
						tags$p(tags$strong("Now viewing:", tags$span(style = paste0("color: ", lytics_colors["brightgreen"]), textOutput("showAid", inline = TRUE))))
					)
				)
			)
		),
		tabItems(
			tabItem(tabName = "build", {
				tags$div(
					fluidRow(
						# plots and output
						box(width = 12,
							tags$div(
								tags$h2("Model Creation"),

								# custom model name
								textInput("model_name", "Custom Model Name", value = "", placeholder = "(optional) custom model names may not include the following characters: / \\ :"),

								tags$p("Select an (optional) source segment and a target segment, and click ", tags$strong("Go!"), " to view correlation for those segments"),

								# input for source segment
								uiOutput("sourceInputs"),

								# input for target segment
								uiOutput("targetInputs"),

								# input for target field
								uiOutput("targetFieldInput"),

								# input for aspect category
								selectInput("collections", "Select Aspect Collections", c(
									"Email" = "email",
									"Web" = "web",
									"Support" = "support",
									"Mobile" = "mobile",
									"Commerce" = "commerce",
									"Behavioral" = "behaviors",
									"Content" = "content"
								), multiple = TRUE),

								# input for custom fields
								uiOutput("fieldsInput"),

								# should the model use scores?
								checkboxInput("use_scores", "Use Scores?", value = TRUE),

								# should the model use content?
								checkboxInput("use_content", "Use Content?", value = TRUE),

								# should we just build the model?
								checkboxInput("build_only", "Build Only?", value = TRUE),

								# should we save the segments?
								checkboxInput("save_segment", "Save Segments", value = FALSE),

								# should we tune the model?
								checkboxInput("tune_model", "Tune Model", value = FALSE),

								# should we recreate the model weekly?
								checkboxInput("re_run", "Rebuild the Model Weekly?", value = FALSE),

								# should the model use autotune?
								checkboxInput("auto_tune", "Auto-Tune?", value = FALSE),

								# tags for insights
								textInput("tags", "Tags: ", value = ""),

								# additional fields not supported by fieldsInput
								textInput("additionalFields", "Additional Custom Fields: ", value = ""),

								# input for sample size
								numericInput("samplesize", "Sample Size", value = 5000, min = 500, max = 50000),

								# algorithm specification
								selectInput("model_type", "Model Type", c(
									"Random Forest" = "rf",
									"Gradient Boosting Machine" = "gbm"
								)),

								actionButton("buildModel", "Go!")
							)
						)
					),
					fluidRow(
						# output for model stuff
						box(width = 12,
							tags$div(
								tags$h3("Output"),
								uiOutput("buildOutput")
							)
						)
					)
				)
			})
		),
		tabItems(
			tabItem(tabName = "summary", {
				tags$div(
					fluidRow(
						# plots and output
						box(width = 12, height = "1300px",
							tags$div(
								# existing models
								fluidRow(
									column(12,
										dataTableOutput("modelsTable")
									)
								),
								fluidRow(
									column(12,
										# input for the existing models
										uiOutput("modelInputs"),

										# big model title
										div(style = "padding-bottom: 1em;",
											uiOutput("modelTitle", inline = TRUE),
											dropdownButton(icon = icon("gear"), label = "Advanced Options", tooltip = TRUE,
												# button to generate insight report from model
												actionButton("buildReport", "Generate Insight Report", icon = icon("chart-pie")),
												uiOutput("reportOutput"),

												# button to evaluate model to entities
												actionButton("evaluateModel", "Score Users Once", icon = icon("users")),
												uiOutput("evaluateOutput"),

												# show model configs
												actionButton("showModel", "Toggle Model Config", icon = icon("search-plus")),
												uiOutput("showModel")
											)
										),

										# infobox for accuracy metrics
										fluidRow(
											column(12,
												infoBoxOutput("modelError", 4),
												infoBoxOutput("modelSpecificity", 4),
												infoBoxOutput("modelSensitivity", 4)
											)
										),
										fluidRow(
											column(12,
												infoBoxOutput("modelAccuracy", 4),
												infoBoxOutput("modelReach", 4),
												infoBoxOutput("modelHealth", 4)
											)
										),

										tabBox(title = "", width = "100%;",
										#tabsetPanel(type = "tabs",
											tabPanel("Plot", plotOutput("importancePlot")),
											tabPanel("Table", dataTableOutput("importanceTable"))
										)
									)
								)
							)
						)
					),
					fluidRow(
						box(width = 12,
							fluidRow(
								column(6,
									plotOutput("fakeRocPlot")
								),
								column(6,
									plotOutput("densityPlot")
								)
							)
						)
					)
				)
			})
		)
	)
)


#
#  SERVER
#

server <- function(input, output) {
	# lytics api (lioapi) client to use for all API requests
	api <- lioapi$new()

	notificationIds <- list()

	account.data <- eventReactive(input$credentials, {
		return(api$set.credentials(input$lioAid, input$lioKey))
	})

	account.refresh <- eventReactive(input$refresh, {
		return(account.data())
	})

	segment.data <- reactive({
		data <- account.data()
		segs <- api$get.segments()
		return(list.to.idlist(segs))
	})

	existing.models <- reactive({
		data <- account.data()
		model.list <- api$get.segment.predictions()
 		# convert the array to a list/map
		models <- list()
		for (model in model.list) {
			models[[model$name]] = model
		}
		return(models)
	})

	input.model <- reactive({
		shiny::validate(shiny::need(!is.null(input$model), label = "Model name"))

		model <- input$model
		if(is.null(model)) {
			models <- existing.models()
			model <- names(models)[1]
		}
		return(model)
	})

	field.data <- function(valid.options = NULL) {
		if(is.null(valid.options)) {
			valid.options <- c("string", "[]string", "int", "number", "map[string]intsum", "map[string]num", "map[string]time", "map[string]int")
		}

		return(reactive({
			data <- account.data()
			schema <- api$get.schema()

			fields <- list()
			for(field in schema$columns) {
				if(field$type %in% valid.options) {
					name <- paste0(ifelse(field$shortdesc != "", field$shortdesc, field$as), " (", field$type, ")")
					fields[[name]] <- field$as
				}
			}
			fields <- unlist(fields)
			return(fields[order(names(fields))])
		}))
	}

	build.model <- eventReactive(input$buildModel, {
		additional <- input$additionalFields
		fields <- input$fields
		tags <- character(0)

		has.target <- !is.empty(input, "target")
		has.targetfield <- !is.empty(input, "target_field")

		if (has.target && has.targetfield) {
			stop("Cannot specify BOTH target segment AND target field")
		}
		
		if (has.target && input$source == input$target) {
			stop("The source and target segments cannot be the same")
		}

		if (nchar(additional) != 0) {
			 vals <- additional %>% strsplit(",") %>% unlist %>% trimws
			 vals <- vals[ nchar(vals) > 1 ]
			 if (length(vals) >= 1)  fields = c(fields, vals)
		}

		if (nchar(input$tags) != 0) {
			vals <- input$tags %>% strsplit(",") %>% unlist %>% trimws
			vals <- vals[ nchar(vals) > 1 ]
			if (length(vals) >= 1) tags = vals
		}

		if (grepl(":|/|\\\\", input$model_name)) {
			stop("Custom model name contains invalid character(s)")
		}

		if (!input$auto_tune && !input$use_scores && !input$use_content && length(fields) == 0 && length(input$collections) == 0) {
			stop("Cannot build a model with no features")
		}

		return(api$post.segment.predictions(
			target = input$target,
			source = input$source,
			aspects = input$collections,
			fields = fields,
			use_scores = input$use_scores,
			use_content = input$use_content,
			build_only = input$build_only,
			auto_tune = input$auto_tune,
			model_name = input$model_name,
			save_segment = input$save_segment,
			tune_model = input$tune_model,
			size = input$samplesize,
			tags = tags,
			re_run = input$re_run
		))
	})

	show.model <- eventReactive(input$showModel, {
		models <- existing.models()
		model <- models[[input.model()]]
		if (is.null(model$conf)) {
			stop(sprintf("model %s has no saved config", input.model()))
		}
		return (model$conf)
	})

	evaluate.model <- eventReactive(input$evaluateModel, {
		models <- existing.models()
		model <- models[[input.model()]]

		if (is.null(model$conf)) {
			stop(sprintf("model %s has no saved config", input.model()))
		}

		# in case there is no custom model name, we pass in the source/target 
		# to look up the model in the DB by the default name - <source_id>::<target_id>
		return(api$post.segment.predictions(
			model_name = model$conf$model_name,
			target = model$conf$target$id,
			source = model$conf$source$id,
			build_only = FALSE,
			eval_only = TRUE,
			tags = character(0)
			))
	})

	build.report <- eventReactive(input$buildReport, {
		tokenResponse <- api$create.token(
			scopes = c("admin", "report_manager"),
			name = "audience insight report creation token",
			label = "audience insight report creation token",
			expiry = "1h"
		)

		key <- tokenResponse$config[[1]]$value
		if (is.null(key)) {
			stop("could not get report_manager key")
		}

		models <- existing.models()
		model <- models[[input$model]]
		if (is.null(model$conf)) {
			stop(sprintf("model %s has no saved config", input$model))
		}

		return(api$post.audience.report(
			key= key,
			target = model$conf$target$id,
			source = model$conf$source$id,
			modelId = input$model,
			label = sprintf("From %s to %s", model$conf$source$name, model$conf$target$name),
			description = sprintf(
				"This report provides insights into how you can identify %s users who are likely to transition to %s",
				model$conf$source$slug_name,
				model$conf$target$slug_name
			)
		))
	})

	model.predictions <- reactive({
		info <- api$get.fieldinfo(c(sprintf("segment_prediction.%s", input.model())))
		return(info$fields[[1]])
	})

	model.fieldinfo <- reactive({
		models <- existing.models()
		model <- models[[input.model()]]
		return(api$get.fieldinfo)
	})

	output$showAid <- renderText({
		nothing <- account.data()
		paste0(api$account$name, " (", api$account$aid, ")")
	})

	model.table <- reactive({
		models <- existing.models()
		return(data.frame(
			Name = map_chr(models, ~ .x$name),
			Health = map_chr(models, ~ .x$summary$model_health),
			Accuracy = map_dbl(models, ~ .x$summary$accuracy),
			Reach = map_dbl(models, ~ .x$summary$reach)
		))
	})

	observeEvent(input$model, {
		models <- existing.models()
		model <- models[[input.model()]]

		if(length(notificationIds) > 0) {
			lapply(notificationIds, removeNotification)
		}
		notificationIds <<- lapply(model$summary$msgs, function(msg){
			type <- "warning"
			if(tolower(msg$severity) == "warn") {
				type <- "error"
			}
			showNotification(msg$text, duration = NULL, closeButton = TRUE, type = type)
		})
	})

	output$modelsTable <- renderDataTable(model.table(), options = list(pageLength = 5, dom = "tp"))

	model.fieldinfo <- reactive({
		models <- existing.models()
		model <- models[[input.model()]]

		info <- api$get.fieldinfo()
	})

	output$sourceInputs <- renderUI({
		segments <- segment.data()
		selectInput("source", "Source Segment", c(c("- Choose Source Segment -" = ""), segments))
	})

	output$targetInputs <- renderUI({
		segments <- segment.data()
		selectInput("target", "Target Segment", c(c("- Choose Target Segment -" = ""), segments))
	})

	output$targetFieldInput <- renderUI({
		fields <- field.data(c("int", "string", "number"))()
		selectInput("target_field", "(Optional) Target Field", fields, multiple = TRUE)
	})

	output$fieldsInput <- renderUI({
		fields <- field.data()()
		selectInput("fields", "Custom Fields", fields, multiple = TRUE)
	})

	output$modelInputs <- renderUI({
		models <- existing.models()
		model.slugs <- names(models)
		print(model.slugs)
		selectInput("model", "Choose an existing model", choices = model.slugs, selected = model.slugs[1])
	})

	output$modelError <- renderInfoBox({
		models <- existing.models()
		model.name <- input.model()
		valueBox(
			paste0(round(1 - accuracy(models[[model.name]]), 4) * 100, "%"), "Overall Error Rate", icon = icon("times"), color = "blue" #, fill = TRUE
		)
	})

	output$modelSensitivity <- renderInfoBox({
		models <- existing.models()
		model.name <- input.model()
		valueBox(
			paste0(round(1 - sensitivity(models[[model.name]]), 4) * 100, "%"), "False Negative", icon = icon("user-minus"), color = "purple" #, fill = TRUE
		)
	})

	output$modelSpecificity <- renderInfoBox({
		models <- existing.models()
		model.name <- input.model()
		valueBox(
			paste0(round(1 - specificity(models[[model.name]]), 4) * 100, "%"), "False Positive", icon = icon("user-plus"), color = "green" #, fill = TRUE
		)
	})

	output$modelHealth <- renderInfoBox({
		# TODO: color/text dynamic
		models <- existing.models()
		model.name <- input.model()

		text <- list(label = "Healthy", color = "green")
		if(models[[model.name]]$summary$model_health == "bad") {
			text <- list(label = "Unhealthy", color = "red")
		}
		valueBox(
			text$label, "Health", icon = icon("heartbeat"), color = text$color #, fill = TRUE
		)
	})

	output$modelReach <- renderInfoBox({
		models <- existing.models()
		model.name <- input.model()
		valueBox(
			span(models[[model.name]]$summary$reach, span(" / 10", class = "denominator")), "Reach", icon = icon("users"), color = "blue" #, fill = TRUE
		)
	})

	output$modelAccuracy <- renderInfoBox({
		models <- existing.models()
		model.name <- input.model()
		valueBox(
			span(models[[model.name]]$summary$accuracy, span(" / 10", class = "denominator")), "Accuracy", icon = icon("dashboard"), color = "purple" #, fill = TRUE
		)
	})

	output$fakeRocPlot <- renderPlot({
		p <- plot.thresholds(existing.models(), input.model())
		print(p)
	})

	output$densityPlot <- renderPlot({
		p <- plot.densities(existing.models(), input.model())
		print(p)
	})

	output$importancePlot <- renderPlot({
		p <- plot.importance(existing.models(), input.model())
		print(p)
	}, height = 700, res = 108)

	output$importanceTable <- renderDataTable({
		importance.table(existing.models(), input$model)
	}, options = list(pageLength = 10))

	output$buildOutput <- renderUI({
		model <- build.model()
		pre(RJSONIO::toJSON(model, pretty = TRUE))
	})

	output$evaluateOutput <- renderUI({
		model <- evaluate.model()
		pre(RJSONIO::toJSON(model, pretty = TRUE))
	})

	output$reportOutput <- renderUI({
		report <- build.report()
		pre(RJSONIO::toJSON(report, pretty = TRUE))
	})

	output$modelTitle <- renderUI({
		models <- existing.models()
		print(list(
			model.names = names(models),
			model.name = input.model()
		))

		model <- models[[input.model()]]
		tags$span(paste0(model$conf$source$name, " → ", model$conf$target$name))
	})

	showing.model <- FALSE
	output$showModel<- renderUI({
		conf <- show.model()
		print(paste0("showing (?", showing.model, ")"))
		if(showing.model) {
			showing.model <<- FALSE
			return(tags$div())
		}

		showing.model <<- TRUE
		pre(RJSONIO::toJSON(conf, pretty=TRUE))
	})
}

port <- as.numeric(Sys.getenv("SHINYPORT"))
if(is.na(port)) {
	port <- 1235
}
runApp(shinyApp(ui, server), port = port, launch.browser = FALSE, host = "0.0.0.0")
