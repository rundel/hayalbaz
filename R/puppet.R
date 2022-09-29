fake_chrome = "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/105.0.0.0 Safari/537.36"

#' @export
puppet = R6::R6Class(
  "puppet",
  public = list(
    initialize = function(url = NULL, cookies = NULL) {
      #m = chromote::Chromote$new(
      #  browser = chromote::Chrome$new(
      #    path = "/Applications/Chromium.app/Contents/MacOS/Chromium"
      #  )
      #)
      #private$session <- m$new_session()

      private$session = chromote::ChromoteSession$new()

      if(!is.null(cookies))
        self$set_cookies(cookies)

      if (!is.null(url))
        self$goto(url)

      invisible(self)
    },
    get_source = function() {
      self$content()
    },

    set_user_agent = function(user_agent) {
      private$session$Network$setUserAgentOverride(user_agent)

      invisible(self)
    },

    screenshot = function(
      filename = "screenshot.png", selector = "html", cliprect = NULL,
      region = c("content", "padding", "border", "margin"), expand = NULL,
      scale = 1, show = FALSE, delay = 0.5, wait_ = TRUE
    ) {
      private$session$screenshot(
        filename = filename, selector = selector, cliprect = cliprect,
        region = region, expand = expand, scale = scale, show = show,
        delay = delay, wait_= wait_
      )
    },
    content = function() {
      private$session$Runtime$evaluate(
        paste0("document.documentElement.outerHTML")
      )$result$value
    },
    set_value = function(selector, value) {
      doc = private$get_document()
      ids = private$session$DOM$querySelectorAll(doc$root$nodeId, selector)$nodeIds

      if (length(ids) > 1)
        warning("Multiple nodes matched the given selector, only the first will be altered.", call. = FALSE)
      else if (length(ids) == 0)
        stop("Selector did not match any nodes in the current document.", call. = FALSE)


      private$session$Runtime$evaluate(
        glue::glue('document.querySelector("{selector}").value = "{value}"')
      )

      invisible(self)
    },

    get_js_object = function(name) {
      private$session$Runtime$evaluate(
        name, returnByValue = TRUE
      )$result$value
    },
    goto = function(url) {
      private$session$Page$navigate(url)
      invisible(self)
    },
    set_cookies = function(cookies) {
      private$session$Network$setCookies(cookies = cookies)
      invisible(self)
    },
    get_cookies = function() {
      private$session$Network$getCookies()$cookies
    },
    view = function() {
      private$session$view()
      invisible(self)
    },

    wait_on_load = function() {
      private$session$Page$loadEventFired()
      invisible(self)
    },

    get_element = function(selector, as_xml2 = TRUE) {
      id = private$get_node(selector)
      html = private$get_node_html(id)

      if (as_xml2)
        xml2::read_html(html)
      else
        html
    },

    get_elements = function(selector, as_xml2 = TRUE) {
      ids = private$get_all_nodes(selector)
      html = purrr::map(ids, private$get_node_html)

      if (as_xml2)
        xml2:::xml_nodeset( purrr::map(html, xml2::read_html) )
      else
        html
    },

    click = function(selector, set_focus=TRUE, scroll=TRUE, wait_for_selector=TRUE) {
      if (wait_for_selector)
        self$wait_for_selector(selector)

      #if (set_focus)
      #  private$session$DOM$focus(id)

      if (scroll) {
        # Not currently supported by Chromote
        #private$session$DOM$scrollIntoViewIfNeeded(id)

        # Based on https://github.com/cyrus-and/chrome-remote-interface/issues/180
        private$session$Runtime$evaluate(
          paste0("document.querySelector('", selector ,"').scrollIntoView()")
        )
      }

      id = private$get_node(selector)
      xy = private$get_node_center(id)

      private$mouse_down(x = xy[1], y = xy[2])
      Sys.sleep(0.5)
      private$mouse_up(x = xy[1], y = xy[2])

      invisible(self)
    },

    focus = function(selector) {
      id = private$get_node(selector)
      private$session$DOM$focus(id)

      invisible(self)
    },

    set_debug_msgs = function(flag) {
      private$session$parent$debug_messages(flag)

      invisible(self)
    },

    type = function(selector = NULL, text) {
      if (!is.null(selector))
        self$focus(selector)

      for(key in strsplit(text, "")[[1]]) {
        #private$key_press(key)
        private$press(key)
        Sys.sleep(0.1)
      }

      invisible(self)
    },

    download_enable = function(path, report = TRUE, progress = report) {
      path = normalizePath(path)
      stopifnot(dir.exists(path))

      private$download_path = path
      private$session$Browser$setDownloadBehavior("allow", downloadPath = path)

      private$watch_download(report = report, progress = progress)

      invisible(self)
    },

    wait_for_selector = function(selector, timeout = 30, polling = 0.1) {
      start = Sys.time()

      repeat {
        id = purrr::possibly(private$get_node, 0)(selector)
        if (id != 0)
          break

        if (Sys.time() - start > timeout)
          stop("Timeout exceeded while waiting for selector")

        Sys.sleep(polling)
      }

      invisible(self)
    },

    attach_file = function(selector, file) {
      stopifnot(file.exists(file))
      id = private$get_node(selector)

      file = fs::path_abs(file)
      file = fs::path_expand(file)

      private$session$DOM$setFileInputFiles(list(file), id)

      invisible(self)
    },

    close = function() {
      private$session$close()

      invisible()
    }
  ),
  private = list(
    session = NULL,
    download_path = NULL,
    download_pb = NULL,

    watch_download = function(start = TRUE, report = TRUE, progress = report) {
      if (!start) {
        private$session$Page$downloadWillBegin(callback_ = NULL, wait_ = FALSE)
        private$session$Page$downloadProgress(callback_ = NULL, wait_ = FALSE)

        return()
      }

      f = function(private) {
        function(l) {
          if (report) {
            message(
              "Downloading ", l$suggestedFilename,
              " from ", l$url,
              " to ", private$download_path,
              "."
            )
          }

          private$download_pb = NULL
        }
      }

      private$session$Page$downloadWillBegin(
        callback_ = f(private),
        wait_ = FALSE
      )

      g = function(private) {
        function(l) {
          if (!progress)
            return()

          if (is.null(private$download_pb)) {
            private$download_pb = progress::progress_bar$new(
              total = l$totalBytes,
              format = "[:bar :percent] downloading @ :rate (:eta)"
            )
          }
          if (!private$download_pb$finished)
            private$download_pb$update(l$receivedBytes/ l$totalBytes)

          if (l$state == "completed") {
            private$download_pb$terminate()
            message("Download completed.")

            private$session$Page$downloadProgress(callback_ = NULL, wait_ = FALSE)

            return()
          } else if (l$state == "canceled") {
            private$download_pb$terminate()
            message("Download canceled.")
            return()
          }
        }
      }

      private$session$Page$downloadProgress(
        callback_ = g(private),
        wait_ = FALSE
      )

      invisible(self)
    },

    get_document = function() {
      private$session$DOM$getDocument()
    },

    get_node = function(selector, all = FALSE) {
      doc = private$get_document()
      private$session$DOM$querySelector(doc$root$nodeId, selector)$nodeId
    },

    get_all_nodes = function(selector) {
      doc = private$get_document()
      private$session$DOM$querySelectorAll(doc$root$nodeId, selector)$nodeIds
    },

    get_node_box = function(node_id) {
      private$session$DOM$getBoxModel(node_id)
    },

    get_node_html = function(node_id) {
      private$session$DOM$getOuterHTML(node_id)$outerHTML
    },

    get_node_center = function(node_id) {
      b = private$get_node_box(node_id)$model$border
      c(x = (b[[1]] + b[[5]])/2,
        y = (b[[2]] + b[[6]])/2 )
    },

    mouse_down = function(x, y, button = "left", click_count = 1) {
      private$session$Input$dispatchMouseEvent(type="mousePressed", x=x, y=y, button=button, clickCount=click_count)
    },

    mouse_up = function(x, y, button = "left", click_count = 1) {
      private$session$Input$dispatchMouseEvent(type="mouseReleased", x=x, y=y, button=button, clickCount=click_count)
    },

    key_down = function(key) {
      private$session$Input$dispatchKeyEvent(
        type="keyDown", text = key, unmodifiedText = key
      )
    },

    key_up = function(key) {
      private$session$Input$dispatchKeyEvent(
        type="keyUp", text = key, unmodifiedText = key
      )
    },

    key_press = function(key) {
      private$session$Input$dispatchKeyEvent(
        type="char", text = key
      )
    },

    press = function(key) {
      private$key_down(key)
      private$key_up(key)
    }
  )
)
