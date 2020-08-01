## app.R ##
library(shiny)
library(shinythemes)
library(shinyWidgets)
library(shinyjs)
library(shinypop)
library(data.table)
library(dplyr)
library(stringr)
#library(fuzzyjoin)
library(kableExtra)

#### 
# setup global variables
####

indexkitslist <- list(
	"Illumina" = list(
		"IDT for Illumina DNA/RNA UD Indexes, Tagmentation" = "idt-udp"),
	"NEB" = list(
		"NEBNext Multiplex Oligos for Illumina (96 Unique Dual Index Primer Pairs)" = "neb"),
	"Zymo" = list(
		"Zymo-Seq UDI Primer Set" = "zymo")
)

machines <- list(
	"forward strand workflow" = list(
		"MiSeq" = "miseq", "NovaSeq" = "miseq", "HiSeq 2500" = "miseq", "HiSeq 2000" = "miseq"),
	"reverse complement workflow"= list(
		"iSeq" = "nextseq", "MiniSeq" = "nextseq", "NextSeq" = "nextseq", "HiSeq X" = "nextseq")
)
# load data and make it available for all sessions
 indexcsv <- fread("indexdata/indexcsv.csv")


####
ui <- fluidPage(
	
	useShinyjs(),
	use_notiflix_notify(position = "right-bottom", width = "380px"),
	use_notiflix_report(cssAnimationDuration = 100, width = "400px"),
	
	theme = shinytheme("cosmo"),
	titlePanel("Illumina Samplesheet Generator", 
						 windowTitle = "Illumina Samplesheet Generator"),
	tags$caption("NCCT | Microbiology"),
	tags$hr(),
	navlistPanel(
		widths = c(2,10),
		tabPanel("1. Paste sample-well",
						 h4("Paste sample-well-set mapping, e.g. sample01; A01; A and press the Read button"), 
						 fluidRow(
						 	column(6,
						 textAreaInput("csv", 
						 							label = "", 
						 							#value = "sample01; A01; setA", 
						 							#width = '400px', 
						 							height = '400px')),
						 column(6,
						 tableOutput("csvread")
						 )),
						 actionButton("read", "Read")
						 ),
		tabPanel("2. Select index kit and machine",
						 fluidRow(column(
						 	6,
						 	pickerInput(
						 		"indexkit",
						 		choices = indexkitslist,
						 		multiple = FALSE,
						 		width = "100%",
						 		label = "Select Index Kit"
						 	)
						 ), column(
						 	2,
						 	pickerInput(
						 		"set",
						 		choices = c("A", "B", "C", "D"),
						 		selected = "A",
						 		multiple = TRUE,
						 		width = "100%", 
						 		label = "Select Set(s)"
						 	)
						 ), column(
						 4,
						 pickerInput("machine", 
						 						choices = machines, 
						 						multiple = FALSE, width = "80%", 
						 						label = "Select machine"))
						 ), 
						 
						 #tags$hr(),
						 tags$br(),
						 tags$hr(),
						 h4("Sample sheet preview"),
						 tableOutput("shPreview")
						 ),
		tabPanel("3. Get samplesheet",
						 h4("This is the third panel"), 
						 downloadButton("download")
						 )
	)
)

server <- function(input, output, session) {
	
	
	# ------------------------------------------------------------- data input handling
	# setup reactives for
	# pasted data and indexkits data
	values <- reactiveValues(csv_data = NULL, 
													 pasted_samples = NULL, # number of samples in pasted data
													 matched_samples = NULL, # number of samples with matched index well
													 index_unique = TRUE) # tracks if indexes are unique in sample sheet
	
	# ------------------------------------------------------------- read in pasted data
	observeEvent(input$read, {
		if(input$csv != '') {
		
		values$csv_data <- try(
			fread(text = input$csv, header = FALSE, col.names = c("Sample_ID", "Index_Plate_Well", "Index_Plate")) %>%
				# to capture cases where set 1,2... is used instead of set A, B...
				mutate(Index_Plate = case_when(
																			 Index_Plate == 1 ~ "A",
																			 Index_Plate == 2 ~ "B",
																			 Index_Plate == 3 ~ "C",
																			 Index_Plate == 4 ~ "D",
																			 TRUE ~ as.character(Index_Plate)
																			 )
							 ) 
													)
			
		} else {
			nx_notify_error("Paste something first!")
		}
	}
							 )
	
	# --------------------------------------------------------- filter index data
	indexdata <- reactive({
		validate(need(input$set, message = "Set is required!"))
		indexcsv %>% dplyr::filter(indexkit == input$indexkit, set %in% input$set, workflow == input$machine) 
	})
	
	#------------------------------------------------------------ join
	joindata <- reactive({
		validate(need(values$csv_data, "pasted data not ok"))
		
		# values$csv_data <- values$csv_data %>% mutate(well_new = str_replace(well, "0", "0?"))
		values$csv_data %>% 
			inner_join(indexdata(), by = c("Index_Plate_Well" = "Index_Plate_Well", "Index_Plate" = "Index_Plate"))
			
	})
	
	observe({
		# both indexes are not unique
		if ( length(unique(joindata()$index_check)) < length(joindata()$index_check) ) {
			
			values$index_unique <- FALSE
			nx_report_error("Index clash!", message = "Two or more samples (highlighted in red) have the same indexes! Please check your input.")
		
		# i7 or i5 is not unique, i.e. CD indexing schemes
		} else if( length(unique(joindata()$index)) < length(joindata()$index) ) {
			nx_report_warning("Warning!", 
												message = "Two or more samples have the same i7 or i5 index.\n 
												This is OK if you are using combinatorial dual indexing scheme, but consider using UDIs!")
		}
	})	
	# ------------------------------ RENDER OUTPUTS
	output$csvread <- function(){
		validate(
			need(values$csv_data, message = "The pasted data must have 3 columns (any separator) and at least 1 new line")
		)
		# check for samples with the same well and same set
		dups <- values$csv_data[ , c("Index_Plate_Well", "Index_Plate")]
		dups_indexes <- which( duplicated(dups) | duplicated(dups, fromLast = T) )
		
		knitr::kable(values$csv_data) %>%
			kable_styling(bootstrap_options = c("hover")) %>%
			row_spec(c(dups_indexes), color = "white", background = "#D7261E")
	}
	
		
		output$shPreview <- function(){
			# first find duplicate indexes to mark them in kable
			dups <- joindata()[ , "index_check"]
			dups_indexes <- which( duplicated(dups) | duplicated(dups, fromLast = T) )
			
			knitr::kable( joindata(), "html") %>% 
				kable_styling(bootstrap_options = c("hover")) %>%
				row_spec(c(dups_indexes), color = "white", background = "#D7261E")
			
		}
	#}
	#})
	# -------------------------------------------------------------TAB3 generate samplesheet and download
	
}

shinyApp(ui, server)