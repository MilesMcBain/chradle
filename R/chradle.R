##' Initialise a chradle
##'
##' @title chr_init
##' @param url url of the session to be inspected
##' @param debug_port port to open the chromium debugger on
##' @param bin name of the browser binary to use, assumed to be on PATH.
##' @return a chromium session as a processx background process
##' @export
chr_init <- function(url = "http://localhost:8080",
                     debug_port=9222,
                     bin="chromium-browser"){
  ## launch chromium window
  debug_process <- processx::process$new(bin,
                                              c("--new-window",
                                                url,
                                                "--user-data-dir=remote-profile",
                                                glue::glue("--remote-debugging-port={debug_port}")))
  debugger_200_ok(debug_port)
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
  open_debuggers <-
    jsonlite::read_json(glue::glue("http://localhost:{instance$debug_port}/json"))
  ws_addr <- open_debuggers[[session_num]]$webSocketDebuggerUrl

  result <- promises::promise(function(resolve, reject){
    ws_con <- websocket::WebSocket$new(ws_addr, autoConnect = FALSE)

    ws_con$onMessage(function(event){
      response <- jsonlite::fromJSON(event$data)
      if (is_response_frame(response)) promises::promise_resolve(response)
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
  }) 
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

debugger_200_ok <- function(port){

  url <- glue::glue("http://localhost:{port}")
  check_url <-
    purrr::safely( httr::GET,
                  otherwise = NA)
  response <- check_url(url)

  if (is.na(response$result) || response$result$status_code != 200){
    Sys.sleep(0.2)
    Recall(port)
  }
  else
    TRUE
}

is_response_frame <- function(ws_msg){
  !is.null(ws_msg$id) && !is.null(ws_msg$result) && ws_msg$id > 1
}
