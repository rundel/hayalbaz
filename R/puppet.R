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

    click = function(selector, set_focus=TRUE) {
      id = private$get_node(selector)
      xy = private$get_node_center(id)

      if (set_focus)
        private$session$DOM$focus(id)

      private$mouse_down(x = xy[1], y = xy[2])
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
