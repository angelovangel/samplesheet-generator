## app.R ##
#test
library(shiny)
library(shinydashboard)
library(shinythemes)
library(shinyWidgets)
library(shinyjs)
library(shinypop) # remotes::install_github("dreamRs/shinypop")
library(data.table)
library(dplyr)
library(stringr)
library(kableExtra)

#### 
# setup global variables
####
# ------------------------------------------------------------------comment this out before deployment
commit_hash <- system("git rev-parse --short HEAD", intern = TRUE)
# ------------------------------------------------------------------comment this out before deployment

indexkitslist <- list(
	"Illumina" = list(
		"IDT for Illumina DNA UD Indexes, Tagmentation" = "idt-udp",
		"IDT for Illumina DNA/RNA UD Indexes, Tagmentation, ver2" = "idt-udp-ver2",
		"Nextera DNA CD Indexes (96 Indexes, 96 Samples)" = "nextera-dna-cd96"),
	"NEB" = list(
		"NEBNext Multiplex Oligos for Illumina (96 Unique Dual Index Primer Pairs)" = "neb"),
	"Zymo" = list(
		"Zymo-Seq UDI Primer Set" = "zymo")
)

machines <- list(
	"forward strand workflow" = list(
		"MiSeq" = "miseq", "NextSeq 2000" = "miseq", "HiSeq 2000/2500" = "miseq", "NovaSeq v1.0 reagents" = "miseq"),
	"reverse complement workflow"= list(
		"iSeq" = "nextseq", "MiniSeq" = "nextseq", "NextSeq 500/550" = "nextseq", "HiSeq 3000/4000/X" = "nextseq", "NovaSeq v1.5 reagents" = "nextseq")
)
# load data and make it available for all sessions
 indexcsv <- fread("indexdata/indexcsv.csv")
 
 sh_colnames <- c("Sample_ID", "Index_Plate_Well", "Index_Plate", 
 								 "I7_Index_ID", "index", "I5_Index_ID", "index2", 
 								 "Sample_Project", "Description")

demo_data <- "#Demo data, delete it and paste yours#\nsample01\tA01\tA\nsample02\tA02\tA\nsample03\tA01\tB"
#####
ui <- fluidPage(
	
	# include css needed for shinydashboard to work. I am NOT putting everything in dashboardPage and they are NOT loaded
	includeCSS("css/AdminLTE.css"),
	includeCSS("css/shinydashboard.css"),
	
	useShinyjs(),
	use_notiflix_notify(position = "right-bottom", width = "480px", timeout = 4000),
	use_notiflix_report(cssAnimationDuration = 100, width = "100%", messageMaxLength = 1800),
	#use_notie(), 
	
	theme = shinytheme("cosmo"),
	titlePanel("Illumina Samplesheet Generator", 
						 windowTitle = "Illumina Samplesheet Generator"),
	fluidRow(
		column(2,
	tags$caption(
	#	),
	#column(2, 
				 actionBttn("supportedkits", label = "Supported kits", size = "lg", style = "fill", color = "success")
	)),
		column(10, 
					 valueBoxOutput("samples_pasted", width = 4),
					 valueBoxOutput("samples_matched", width = 4),
					 valueBoxOutput("samples_clashed", width = 4)
					 )
	),
	#tags$h5("This tool generates Illumina sequencing sample sheets (double indexing only , UDI and CD)."),
	tags$hr(),
	
	navlistPanel(
		well = F, fluid = F,
		widths = c(2,10),
		tabPanel(
			"1. Paste sample-well",
			tags$h4("Paste sample-well-set mapping and press the Read button"),
			tags$h5(
				"You can paste a 3-column data from excel, for example 'sample01; A01; A' or 'sample01  A01  A'"
			),
			fluidRow(column(
				6,
				textAreaInput(
					"csv",
					value = demo_data,
					label = "",
					height = '300px'
				)
			),
			column(6,
						 tableOutput("csvread"))),
			actionBttn("read", "Read data", style = "fill", color = "success")
		), 
		tabPanel(
			"2. Select index kit and machine",
			fluidRow(
				column(
					6,
					pickerInput(
						"indexkit",
						choices = indexkitslist,
						multiple = FALSE,
						width = "100%",
						label = "Select Index Kit", 
						inline = FALSE
					)
				),
				column(
					2,
					pickerInput(
						"set",
						choices = c("A", "B", "C", "D"),
						selected = "A",
						multiple = TRUE, 
						width = "100%",
						label = "Select Set(s)", 
						inline = FALSE
					)
				),
				column(
					4,
					pickerInput(
						"machine",
						choices = machines,
						multiple = FALSE,
						width = "80%",
						label = "Select machine", 
						inline = FALSE
					)
				)
			),
			
			#tags$hr(),
			tags$br(),
			tags$hr(),
			h4("Sample sheet preview"),
			tableOutput("shPreview1")
		),
		tabPanel(
			"3. Add run details and get samplesheet",
			tags$h4("Fill in optional fileds"),
			tags$h6(
				"For running bcl2fastq these fields are optional, but may be needed if the sample sheet is needed for other software.
				The 'Read1' and 'Read2' fields are required only if a sample sheet file is used to set up a sequencing run through the MiSeq Control Software."
				
			),
			# inputs for Header
			fluidRow(column(
				3,
				airDatepickerInput(inputId = "date", "Date", value = Sys.Date())
			), column(
				3,
				textInput(inputId = "investigator", "Investigator"),
			), column(
				3,
				textInput("description", "Description")
			), column(
				1,
				numericInput("read1", label = "Read1", min = 51, max = 301, step = 1, value = NULL, width = "100%")
			), column(
				1,
				numericInput("read2", label = "Read2", min = 51, max = 301, step = 1, value = NULL, width = "100%")
			)),
			fluidRow(column(
				3,
				prettyCheckbox("trimming", "Adapter trimming", 
											 value = FALSE, 
											 status = "info", fill = TRUE)
			)),
			tags$hr(),
			
			tags$h4("Download samplesheet or sample-index mapping"),
			tags$h6(
				"Download complete sample sheet or just the sample-index mapping, to be inserted after the [DATA] field in a sample sheet. 
				A preview of the sample sheet is shown below."
			),
			downloadBttn(
				"download1",
				style = "fill", color = "success",
				label = "Download sample sheet",
				size = "sm"
			),
			downloadBttn(
				"download2",
				style = "fill", color = "success",
				label = "Download sample-index mapping",
				size = "sm"
			),
			# preview complete sample sheet
			verbatimTextOutput("shPreview2")
		)
	)
)

server <- function(input, output, session) {
	
	
	# ------------------------------------------------------------- data input handling
	# setup reactives for
	# pasted data and indexkits data
	values <- reactiveValues(csv_data = NULL, 
													 samples_pasted = 0, # number of samples in pasted data
													 samples_matched = 0, # number of samples with matched index well
													 samples_clashed = 0, # tracks if indexes are unique in sample sheet
													 sample_id_valid = TRUE,
													 index_well_valid = TRUE)
	# these are the reactives for the sample sheet sections apart from [DATA]
	sh_values <- reactiveValues(date = NULL,
															investigator = NULL,
															description = NULL,
															trimm_seq1 = NULL,
															trimm_seq2 = NULL,
															read1 = NULL, 
															read2 = NULL)
	# ------------------------------------------------------------- read in pasted data
	observeEvent(input$read, {
		if(input$csv != '') {
		
		values$csv_data <- try(
			fread(text = input$csv, header = FALSE, col.names = c("Sample_ID", "Index_Plate_Well", "Index_Plate")) %>%
				# to capture cases where set 1,2... or a,b... is used instead of set A, B...
				mutate(Index_Plate = case_when(
																			 Index_Plate == 1 | Index_Plate == "a" ~ "A",
																			 Index_Plate == 2 | Index_Plate == "b" ~ "B",
																			 Index_Plate == 3 | Index_Plate == "c" ~ "C",
																			 Index_Plate == 4 | Index_Plate == "d" ~ "D",
																			 TRUE ~ as.character(Index_Plate)
																			 ),
							 Index_Plate_Well = ifelse(str_length(Index_Plate_Well) == 2, 
							 													yes = str_replace(Index_Plate_Well, "([1-9])", "0\\1"), 
							 													no = Index_Plate_Well) %>% toupper()
							 )
													)
		# solution to insert leading zeros for 1-9
		# ifelse(str_length(chr) == 2, yes = str_replace(chr, "([1-9])", "0\\1"), no = chr) # \1 is the capture group
		
		values$samples_pasted <- nrow(values$csv_data)
		
		# check for valid index plate well names, check for valid Sample_ID names
		# now this is a logical vector, to store positions of rows that do match pattern
		values$index_well_valid <-  str_detect(values$csv_data$Index_Plate_Well, "^[A-H][0-1][0-2]$") 
		# The field for the Sample_ID column has special character restrictions 
		#as only alphanumeric (ASCII codes 48-57, 65- 90, and 97-122), 
		#dash (ASCII code 45), and underscore (ASCII code 95) are permitted. 
		#The Sample_ID length is limited to 100 characters maximum
		values$sample_id_valid <- str_detect(values$csv_data$Sample_ID, "^[-_0-9A-Za-z]{2,100}$")
		
		} else {
			nx_notify_error("Paste something first!")
		}
	}
							 )
	#-----------------------------------------------------------REACTIVES
	# --------------------------------------------------------- filter index data
	indexdata <- reactive({
		validate(need(input$set, message = "Set is required!"))
		indexcsv %>% dplyr::filter(indexkit == input$indexkit, set %in% input$set, workflow == input$machine) 
	})
	
	#------------------------------------------------------------ join
	joindata <- reactive({
		validate(need(values$csv_data, "No samples data pasted"))
		
		values$csv_data %>% 
			inner_join(indexdata(), by = c("Index_Plate_Well" = "Index_Plate_Well", "Index_Plate" = "Index_Plate")) %>%
			as.data.table() # make sure it is not something else after join
			
	})
	#------------------------------------------------------------ header of sample sheet
	sh <- reactive({
		c(
			"[Header]",
			mapply(
				paste,
				list("Date", "Investigator", "Description", "Workflow"),
				list(sh_values$date, sh_values$investigator, sh_values$description,"GenerateFASTQ"),
				MoreArgs = list(sep = ",")
			),
			"", #empty lines are ignored - here just for clarity
			"[SoftwareInfo]",
			paste("name", "samplesheet-generator", sep = ","),
			paste("git_commit", commit_hash, sep = ","),
			# this app must be under git control!
			paste("executed_on", Sys.time(), sep = ","),
			paste("author", "Angel Angelov", sep = ","),
			"",
			"[Settings]",
			paste("Adapter", sh_values$trimm_seq1, sep = ","),
			paste("AdapterRead2", sh_values$trimm_seq2, sep = ","),
			"",
			"[Reads]",
			#  only required when using a sample sheet file to set up a sequencing run through the MiSeq® Control Software.
			sh_values$read1,
			sh_values$read2,
			"",
			"[Data]"
		)
	})
	
	# observe index clashes and number of matched and clashed samples
	observe({
		# both indexes are not unique
		if ( length(unique(joindata()$index_check)) < length(joindata()$index_check) ) {
			
			values$samples_matched <- nrow( joindata() )
			values$samples_clashed <- length(joindata()$index_check) - length(unique(joindata()$index_check))
				
			#nx_report_error("Index clash!", message = "Two or more samples (highlighted in red) have the same indexes! Please check your input.")
		
		# i7 or i5 is not unique, i.e. CD indexing schemes
		} else if( length(unique(joindata()$index)) < length(joindata()$index) ) {
			
			values$samples_matched <- nrow( joindata() )
			
			nx_report_warning("Warning!", 
												message = "Two or more samples have the same i7 or i5 index.\n 
												This is OK if you are using combinatorial dual indexing scheme, but consider using UDIs!")
		} else {
			values$samples_matched <- nrow( joindata() )
			values$samples_clashed <- 0
		}
		
	})
	
	# separate observer for valid index well and sample id names
	observe({
		if( !all(values$index_well_valid ) ) {
			nx_notify_warning("Index_Plate_Well name is not valid! Only A01 to H12 are accepted")
			
		} 
		if( !all(values$sample_id_valid) ){
			nx_notify_error("Sample_ID name is not valid! Only '0-9', 'A-Z', 'a-z', '-' and '_' allowed")
			
		}
	})
		
	# various other OBSERVERS
	#-------------------------------- list supported kits
	observeEvent(input$supportedkits, {
		nx_report_info("Supported indexing kits", 
									 message = tags$p(
									 	style = "text-align: left;", 
									 	tags$ul(tags$li(
									 	tags$a(href = "https://emea.support.illumina.com/bulletins/2020/06/illumina-library-prep-kits-and-associated-index-kits.html", 
									 				 "IDT for Illumina DNA UD Indexes, Tagmentation, Sets A-D, Cat.# 20027213, 20027214, 20027215, 20027216", 
									 				 target = "_blank")),
									 	tags$li(
									 		tags$a(href = "https://emea.support.illumina.com/bulletins/2020/06/illumina-library-prep-kits-and-associated-index-kits.html", 
									 					 "IDT for Illumina DNA/RNA UD Indexes, Tagmentation, Sets A-D ver2, Cat.# 20027213, 20027214, 20042666, 20742667", 
									 					 target = "_blank")),
									 	tags$li(
									 		tags$a(href = "https://emea.support.illumina.com/bulletins/2020/06/illumina-library-prep-kits-and-associated-index-kits.html", 
									 					 "Nextera DNA CD Indexes (96 Indexes, 96 Samples), Cat.# 20018708", 
									 					 target = "_blank")),
									 	tags$li(
									 		tags$a(
									 			href = "https://international.neb.com/tools-and-resources/selection-charts/nebnext-multiplex-oligos-selection-chart",
									 			"NEBNext Multiplex Oligos for Illumina (96 Unique Dual Index Primer Pairs), Sets 1-4, Cat.# E6440, E6442, E6444, E6446", 
									 			target = "_blank")),
									 	tags$li(tags$a(
									 		href = "https://www.zymoresearch.de/products/zymo-seq-udi-primer-sets",
									 		"Zymo-Seq UDI Primer Set, set A, Cat.# D3096", target = "_blank"))
									 	),
									 	tags$a(href = "https://github.com/angelovangel/samplesheet-generator/issues/new?labels=new_kit&title=New+index+kit+request", 
									 				 "Request to include a new kit by opening an issue on GitHub", 
									 				 target = "_blank")
									 )
		)
	})
	
	# -------------------------------observer to update set input based on kit selected
	# for kits with one set only
	observe({
		if(input$indexkit == "zymo" | input$indexkit == "nextera-dna-cd96") {
			updatePickerInput(session = session, 
												inputId = "set", 
												choices = c("A"), 
												selected = "A")
		} else {
			updatePickerInput(session = session,
												inputId = "set",
												choices = c("A", "B", "C", "D"),
												selected = "A")
			}
	})
	
	
	#---------------------------------------------------------observer to update sh_values
	observe({
		sh_values$date <- input$date
		sh_values$investigator <- input$investigator
		sh_values$description <- input$description
		
		# assign trimm_seq depending on kit selected, and depending if input$trimming is selected
		if( (input$indexkit == "idt-udp" | input$indexkit == "idt-udp-ver2" | input$indexkit == "nextera-dna-cd96") & input$trimming ) {
			sh_values$trimm_seq1 <- "CTGTCTCTTATACACATCT"
		
		# neb and zymo have TruSeq adapters
		} else if( (input$indexkit == "neb" | input$indexkit == "zymo") & input$trimming ) {
			sh_values$trimm_seq1 <- "AGATCGGAAGAGCACACGTCTGAACTCCAGTCA"
			sh_values$trimm_seq2 <- "AGATCGGAAGAGCGTCGTGTAGGGAAAGAGTGT"
		} else {
			# this may seem unnecessary, but I need NULL, not NA, which happens if directly input$trimm_seq1 is used
			sh_values$trimm_seq1 <- NULL
			sh_values$trimm_seq2 <- NULL
		}
		# this may seem unnecessary, but I need NULL, not NA
		if( !is.na(input$read1) | !is.na(input$read2) ) {
			sh_values$read1 <- input$read1
			sh_values$read2 <- input$read2
		} else {
			sh_values$read1 <- NULL
			sh_values$read2 <- NULL
		}
		
	})
		
	# ------------------------------ ---------------------------RENDER OUTPUTS
	output$csvread <- function(){
		validate(
			need(values$csv_data, message = "The pasted data must have 3 columns (any separator) and at least 1 new line. No empty lines are allowed.")
		)
		# check for samples with the same well and same set
		dups <- values$csv_data[ , c("Index_Plate_Well", "Index_Plate")]
		dups_indexes <- which( duplicated(dups) | duplicated(dups, fromLast = T) )
		
		kableExtra::kable(values$csv_data) %>%
			kable_styling(fixed_thead = TRUE,
										bootstrap_options = c("hover")) %>%
			row_spec(c(dups_indexes), color = "white", background = "#D7261E") %>%
			# invalid index well names
			row_spec( which(
				!str_detect(values$csv_data$Index_Plate_Well, "^[A-H][0-1][0-2]$")
				), color = "white", background = "#F1C40F" ) %>%
			# invalid sample id names
			row_spec( which(
				!str_detect(values$csv_data$Sample_ID, "^[-_0-9A-Za-z]{2,100}$")
				), color = "white", background = "#D7261E")
	}
	
		#---------------------------------------------------------preview data
		output$shPreview1 <- function(){
			# first find duplicate indexes to mark them in kable
			dups <- joindata()[ , "index_check"]
			dups_indexes <- which( duplicated(dups) | duplicated(dups, fromLast = T) )
			
			kableExtra::kable( joindata()[, ..sh_colnames], "html") %>% # for ..sh_colnames --> Perhaps you intended DT[, ..sh_colnames]. This difference to data.frame is deliberate and explained in FAQ 1.1.
				kable_styling(fixed_thead = TRUE, 
											bootstrap_options = c("hover")) %>%
				row_spec(c(dups_indexes), color = "white", background = "#D7261E") %>%
				# invalid sample id names
				row_spec( which(
					!str_detect(values$csv_data$Sample_ID, "^[-_0-9A-Za-z]{2,100}$")
				), color = "white", background = "#D7261E")
			
		}
		#---------------------------------------------------------preview sample sheet header
		output$shPreview2 <- renderPrint({
			write( sh(), file = "");
			write.table(joindata()[, ..sh_colnames], 
									file = "", 
									append = TRUE, 
									sep = ",",
									quote = FALSE, 
									col.names = TRUE, 
									row.names = FALSE)
		})
		
		#---------------------------------------------------------value boxes renders
		output$samples_pasted <- renderValueBox({
			color <- ifelse(values$samples_pasted > 0, "green", "yellow")
			valueBox(values$samples_pasted, "samples pasted", color = color)
		})
		
		output$samples_matched <- renderValueBox({
			color <- ifelse(values$samples_matched == values$samples_pasted && values$samples_matched != 0, 
											"green", "yellow")
			validate(need(indexdata(), message = "Set is required!"))
			valueBox(values$samples_matched, "samples with matched indexes", color = color)
		})
		
		output$samples_clashed <- renderValueBox({
			color <- ifelse(values$samples_clashed == 0, "green", "red")
			valueBox(values$samples_clashed, "samples with clash indexes", color = color)
		})
	#}
	#})
	# -------------------------------------------------------------TAB3 generate samplesheet and download
	output$download1 <- downloadHandler(
		filename = paste(Sys.Date(), "-samplesheet.csv", sep = ""),
		content = function(file) { 
			
			write(sh(), file = file) #----------------------------------sh is constructed in REACTIVES above
			write.table(joindata()[, ..sh_colnames], 
									file = file, 
									append = TRUE, 
									sep = ",",
									quote = FALSE, 
									col.names = TRUE, 
									row.names = FALSE) # important!
			#nx_report_error("Error!", "This feature is still in development") 
			}
		
	)
	
	output$download2 <- downloadHandler(
		filename = paste(Sys.Date(), "-sample-index.csv", sep = ""),
		content = function(file) {fwrite( joindata()[, ..sh_colnames], sep = ",", file = file )}
	)

}

shinyApp(ui, server)