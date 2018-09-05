Chradle <-
  R6::R6Class("Chradle",
              public = list(
                ws_con = NULL,
                session_num = NULL,
                debug_port = NULL,
                session_url = NULL,
                debug_process = NULL,
                message_ctr = NULL,
                response = NULL,
                waiting_for_reply = NULL,
                initialize = function(url = "http://localhost:8080", debug_port=9222, bin="chromium-browser"){

                  self$session_num <- 1
                  self$session_url <- url
                  self$message_ctr <- 1
                  self$waiting_for_reply <- FALSE

                  ## launch chromium window
                  self$debug_process <- processx::process$new(bin,
                                        c("--new-window",
                                         url,
                                          "--user-data-dir=remote-profile",
                                          glue::glue("--remote-debugging-port={debug_port}")))
                  Sys.sleep(3)
                  open_debugegrs <-
                    jsonlite::read_json(glue::glue("http://localhost:{debug_port}/json"))

                  ws_addr <- open_debugegrs[[self$session_num]]$webSocketDebuggerUrl
                  self$ws_con <- websocket::WebSocket$new(ws_addr, autoConnect = FALSE)

                  self$ws_con$onMessage(function(event){
                    self$response <- event$data
                    self$waiting_for_reply <- FALSE
                  })

                  self$ws_con$connect()

                  self$send_message(method = "Runtime.enable")
                },

                get_message_id = function(){
                  current_id <- self$message_ctr
                  self$message_ctr <- self$message_ctr + 1
                  current_id
                },

                send_message = function(method, params = NULL){
                  args <- list()
                  args$id <- self$get_message_id()
                  args$method <- method
                  args$params <- params
                  package <- jsonlite::toJSON(args, auto_unbox = TRUE)
                  self$ws_con$send(package)
                },

                get_attribute_names = function(id){
                  self$waiting_for_reply <- TRUE
                  self$send_message(method = "Runtime.evaluate",
                                    params = list(
                                      expression = glue::glue('document.querySelector("{id}").getAttributeNames()')))
                  f <- future({self$get_reply()})
                  value(f)
                },

                get_attributes = function(id, attribute){
                  self$waiting_for_reply <- TRUE
                  self$send_message(method = "Runtime.evaluate",
                                    params = list(
                                      expression = glue::glue('document.querySelector("{id}").getAttribute("{attribute}")')))
                  f <- future({self$get_reply()})
                  value(f)
                }

              ),

              private = list(

                get_reply = function(){
                  while(self$waiting_for_reply){}
                  self$response
                })
              )
