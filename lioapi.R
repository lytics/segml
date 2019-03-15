lioapi <- setRefClass("lioapi",
	fields = list(
		lioapi     = "character",
		lioaid     = "numeric",
		liokey     = "character",
		account    = "list"
	),
	methods = list(
		initialize = function() {
			lioapi <<- "https://api.lytics.io"
			lioaid <<- as.integer(Sys.getenv("LIOAID"))
			liokey <<- Sys.getenv("LIOKEY")
		},
		is.failed.response = function(response) {
			if(is.null(response)) {
				return(TRUE)
			}
			if(is.list(response)) {
				# freaking query endpoint
				if(is.null(response[["message"]])) {
					return(FALSE)
				}
				check.status <- check.value(response, "status", 300, presume = FALSE, op = `>=`)
				check.message <- check.value(response, "message", "failure", presume = FALSE, op = `==`) # freaking query endpoint
				return(check.status || check.message)
			}
			# otherwise no reason to assume it's failed?
			return(FALSE)
		},
		exec = function(path, params = list(), body = list(), method = c("GET", "POST", "PUT"), max.retry = 0, return.all = FALSE) {
			method <- match.arg(method)

			# authenticate
			params$key = .self$liokey
			#params$account_id = .self$account$id

			url <- sprintf("%s/%s", .self$lioapi, path)

			retrycount <- 0
			response <- NULL

			while(is.failed.response(response) && retrycount <= max.retry) {
				if(method == "GET") {
					raw.response <- .self$get(url, params)
				} else if(method == "POST") {
					raw.response <- .self$post(url, params, body)
				} else if(method == "PUT") {
					raw.response <- .self$put(url, params, body)
				}
				response <- httr::content(raw.response)
				if(is.failed.response(response)) {
					retrycount <- retrycount + 1
					Sys.sleep(1)
				}
			}
			if(retrycount > max.retry) {
				stop(paste0("API Error (", response$status, "): ", response$message))
			}

			if(return.all) {
				return(response)
			}

			return(response$data)
		},
		get = function(url, params) {
			return(httr::GET(url = url, query = params))
		},
		post = function(url, params, body) {
			return(httr::POST(url = url, query = params, body = body, encode = "json"))
		},
		put = function(url, params, body) {
			return(httr::PUT(url = url, query = params, body = body, encode = "json"))
		},
		set.credentials = function(aid, key) {
			aid <- as.integer(aid)
			lioaid <<- aid
			liokey <<- key

			return(.self$set.account(aid))
		},
		# TODO: account
		set.account = function(aid) {
			response <- .self$get.account(aid)
			.self$account <- response
			invisible(response)
		},
		get.account = function(aid, state = TRUE) {
			return(.self$exec(sprintf("api/account/%d", aid), list(state = state), max.retry = 10))
		},
		put.account = function(body = list()) {
			return(.self$exec(sprintf("api/account/%d", .self$account$aid), body = body, method = "PUT"))
		},
		get.schema = function(schema = "user") {
			return(.self$exec(sprintf("api/schema/%s", schema)))
		},
		get.fieldinfo = function(fields, schema = "user") {
			params <- list(fields = paste(fields, collapse = ","), limit = 100)
			return(.self$exec(sprintf("api/schema/%s/fieldinfo", schema), params))
		},
		get.segment.fieldinfo = function(fields, ids = c(), ...) {
			params <- list(fields = paste(fields, collapse = ","), ids = paste(ids, collapse = ","), ...)
			return(.self$exec("api/segment/fieldinfo", params))
		},
		get.segments = function(table = "user") {
			return(.self$exec("api/segment", list(table = table)))
		},
		get.segment = function(id = "") {
			if (id == "" || is.null(id)) stop("id field must be specified")
			route <- sprintf("api/segment/%s", id)
			return (.self$exec(route))
		},
		get.segment.size = function(id = NULL, ids = NULL, segments = NULL){
			params <- list()
			route <- "api/segment/size"

			if(!is.null(id)) {
				route <- sprintf("api/segment/%s/size", id)
			}

			if(!is.null(ids)) {
				params$ids = paste(ids, collapse = ",")
			}

			if(!is.null(segments)) {
				if (is.character(segments)) {
					params$segments = segments
				} else {
					params$segments = .self$.jsonify(segments)
				}
			}

			return(.self$exec(route, params))
		},
		get.segmentml.dependencies = function(model = "") {
			route <- sprintf("api/segmentml/%s/_dependencies", model)
			return(.self$exec(route))
		},
		get.segment.predictions = function() {
		  	return(.self$exec("api/segmentml"))
		},
		post.segment.predictions = function(target = "", source = "", targetfield = "", aspects = c(), fields = c(), tags = c(), use_scores = TRUE, use_content = TRUE, build_only = FALSE, save_segment = FALSE, size = 5000, eval_only = FALSE, tune_model = FALSE, model_type = "rf") {
			model_type <- match.arg(model_type, c("rf", "gbm"))

			config <- list(
				source = source,
				save_segment = save_segment,
				use_scores = use_scores,
				use_content = use_content,
				model_only = build_only,
				num_to_train = size,
				eval_only = eval_only,
				tune_model = tune_model,
				tags = tags,
				model_type = model_type
			)

			if(!is.empty(targetfield)) {
				config$target_field = targetfield
			} else {
				config$target = target
			}

			# annoying json formatting issues for vectors of different lengths
			if(length(aspects) > 0) {
				if(length(aspects) == 1) {
					aspects <- list(aspects)
				}
				config$aspect_collections <- aspects
			}
			if(length(fields) > 0) {
				if(length(fields) == 1) {
					fields <- list(fields)
				}
				config$additional_fields <- fields
			}
			if(length(tags) > 0) {
				if(length(tags) == 1) {
					tags <- list(tags)
				}
				config$tags <- tags
			}

			return(.self$post.work("cab095c2772947fb9206f7334a5c2398", config))
		},
		post.work = function(workflow.id, config, ...) {
			body <- list(workflow_id = workflow.id, config = config, ...)
			return(.self$exec("api/work", params = list(), body = body, method = "POST"))
		},
		post.audience.report = function(target = "", source = "", modelId = "", label = "", description = "", key="") {
			config <- list(
				label = label,
				description = description,
				audiences = list(
					source = list(id = source),
					target = list(id = target)
				),
				segml_model_id = modelId
			)

			url <- sprintf("%s/api/report", .self$lioapi)
			response <- .self$post(url, body = config, params = list(account_id = .self$account$id, key= key))
			return(response$data)
		},
		create.token = function(scopes = c(), name = "", label="", expiry="") {
			config <- list(
				name = name,
				label = label,
				expires = expiry,
				scopes = scopes
			)
			return (.self$exec("api/auth/createtoken", method = "POST", body = config, params = list(
				account_id = .self$account$id,
				auth_type = "api_token"
			)))
		}
	)
)


check.value <- function(x, key, value, presume = FALSE, as = identity, op = `==`) {
	if(!is.null(x[[key]])) {
		return(op(as(x[[key]]), value))
	}
	return(presume)
}

trim <- function (x) gsub("^\\s+|\\s+$", "", x)

is.empty <- function(obj, key = NULL) {
	if(!is.null(key)) {
		obj <- obj[[key]]
	}

	if(is.null(obj)) {
		return(TRUE)
	}

	if(is.na(obj)) {
		return(TRUE)
	}

	if(is.character(obj)) {
		return(nchar(obj) == 0)
	}

	if(is.numeric(obj)) {
		return(obj == 0)
	}

	return(FALSE)
}
