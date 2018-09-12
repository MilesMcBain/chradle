##' Initialise a chradle
##'
##' @title chr_init
##' @param url url of the session to be inspected
##' @param debug_port port to open the chromium debugger on
##' @param bin name of the browser binary to use, assumed to be on PATH.
##' @param block_on_message_pattern a text pattern appearing in a message to
##'   wait for. Initialisation will not complete until this message is received.
##'   Initialisation will fail if it is not received in around 30 seconds.
##' @return a chromium session as a processx background process
##' @export
chr_init <- function(url = "http://localhost:8080",
                     debug_port=9222,
                     bin="chromium-browser",
                     block_on_message_pattern="Runtime.executionContextCreated"){
  ## launch chromium window
  debug_process <- processx::process$new(bin,
                                              c("--new-window",
                                                url,
                                                "--user-data-dir=remote-profile",
                                                glue::glue("--remote-debugging-port={debug_port}")))
  debugger_200_ok(debug_port)

  ## Browser up
  ## Connect and enable runtime
  session_num <- 1
  result_env <- new.env()
  result_env$result_value <- NULL

  open_debuggers <-
    jsonlite::read_json(glue::glue("http://localhost:{debug_port}/json"))

  ws_addr <- open_debuggers[[session_num]]$webSocketDebuggerUrl
  ws_con <- websocket::WebSocket$new(ws_addr, autoConnect = FALSE)

  ws_con$onOpen(function(event){
    message("chradle: opened ws connection.")
    chr_message(id = 1,
                method = "Runtime.enable") %>%
      ws_con$send()
  })

  ## Block on a message indicating runtime enabled or a later message of the
  ## user's nomination.
  ws_con$onMessage(function(event){
    if (str_detect(event$data, block_on_message_pattern)) {
      message("chradle got message with pattern: ", block_on_message_pattern)
      result_env$result_value <- TRUE
    }
  })

  ws_con$connect()
  block_on_NULL("result_value", max_attempts = 150, envir = result_env)
  ws_con$close()

  instance <- list(debug_process = debug_process,
                   debug_port = debug_port)
  instance
}

##' Make a call to a chadle
##'
##' @title chr_call
##' @param instance a chromium session returned by chr_init
##' @param query a message to be sent to the chromium session
##' @return The chromium debugger response to the query.
##' @author Miles McBain
##' @export
chr_call <- function(instance, query){
  session_num <- 1
  result_env <- new.env()
  result_env$result_value <- NULL
  open_debuggers <-
    jsonlite::read_json(glue::glue("http://localhost:{instance$debug_port}/json"))

  ws_addr <- open_debuggers[[session_num]]$webSocketDebuggerUrl
  ws_con <- websocket::WebSocket$new(ws_addr, autoConnect = FALSE)

  ws_con$onMessage(function(event){
    response <- jsonlite::fromJSON(event$data)
    if (is_response_frame(response)) {
      message("chradle got a resonse")
      result_env$result_value <- response
    }
  })

  ws_con$onOpen(function(event){
    message("chradle: opened ws connection.")
    ## enable runtime
    chr_message(id = 1,
                method = "Runtime.enable") %>%
      ws_con$send()
    ## perform the query
    query %>%
      ws_con$send()
  })

  ws_con$connect()

  block_on_NULL("result_value", max_attempts = 150, envir = result_env)
  ws_con$close()

  result_env$result_value
}

##' Compose a gettAttributeNames() call to be invoked in a chromium session.
##'
##' @title get_attribute_names
##' @param id of the html element to get attribute names
##' @return A message to be sent with chr_call().
##' @export
get_attribute_names <- function(id){
  chr_message(id = 2,
              method = "Runtime.evaluate",
              params = list(
                expression = glue::glue('document.querySelector("{id}").getAttributeNames()'),
                returnByValue = TRUE))
}

##' Compose a gettAttribute() call to be invoked in a chromium session.
##'
##' @title get_attributes()
##' @param id the id of the html element to get an attribute value from.
##' @param attribute the name of the attribute to get the value of.
##' @return a message to be sent with chr_call().
##' @export
get_attributes <- function(id, attribute){
  chr_message(id = 2,
              method = "Runtime.evaluate",
              params = list(
                expression = glue::glue('document.querySelector("{id}").getAttribute("{attribute}")'),
                returnByValue = TRUE))
}

##' Kill a background chromium process created with chr_init()
##'
##' @title chr_kill
##' @param instance a chromium process running in background. Created with chr_init
##' @return a response code for kill signal.
##' @export
chr_kill <- function(instance){
  instance$debug_process$kill()
}

##' Clean up chromium process temp files
##'
##' The background process will create a bunch of temporary files in the current
##' working directory under the folder 'remote-profile'. This will delete them.
##'
##' @title chr_clean
##' @return nothing
##' @author Miles McBain
##' @export
chr_clean <- function(){
  processx::process$new("rm",
                        c("-rf", "remote-profile"))
  invisible()
}


################################################################################
## Helpers
################################################################################

chr_message <- function(id, method, params = NULL){
  args <- list()
  args$id <- id
  args$method <- method
  args$params <- params
  package <- jsonlite::toJSON(args, auto_unbox = TRUE)
  package
}

debugger_200_ok <- function(port, retry_delay=0.2, max_attempts=15, attempt=1){

  if (max_attempts <= attempt){
    stop(glue::glue("Reached max attempts ({max_attempts}) without HTTP 200 response from debugger on http://localhost:{port}")) 
  }

  url <- glue::glue("http://localhost:{port}")
  check_url <-
    purrr::safely( httr::GET,
                  otherwise = NA)
  response <- check_url(url)

  if (is.na(response$result) || response$result$status_code != 200){
    Sys.sleep(retry_delay)
    Recall(port, retry_delay, max_attempts, attempt = attempt + 1)
  }
  else
    TRUE
}

is_response_frame <- function(ws_msg){
  !is.null(ws_msg$id) && !is.null(ws_msg$result) && ws_msg$id > 1
}

promise_status <- function(a_promise){
  attr(a_promise, "promise_impl")$status()
}

block_on_NULL <- function(a_value_name,
                          retry_delay = 0.2,
                          max_attempts=15,
                          attempt=1,
                          envir = parent.frame()){
  if (max_attempts <= attempt){
    stop(glue::glue("Reached max attempts ({max_attempts}) without value resolution."))
  }
  a_value <- get(a_value_name, envir = envir)

  if (is.null(a_value)){
    later::run_now(retry_delay)
    Recall(a_value_name,
           retry_delay,
           max_attempts,
           attempt = attempt + 1,
           envir = envir)
  }
  else
    TRUE
}
