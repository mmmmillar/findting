import gleam/bool
import nakai
import nakai/html
import wisp

pub type Context {
  Context(static_directory: String, items: List(String))
}

pub fn middleware(
  req: wisp.Request,
  ctx: Context,
  handle_request: fn(wisp.Request) -> wisp.Response,
) -> wisp.Response {
  let req = wisp.method_override(req)

  use <- wisp.serve_static(req, under: "/static", from: ctx.static_directory)
  use <- wisp.log_request(req)
  use <- wisp.rescue_crashes
  use req <- wisp.handle_head(req)
  use <- default_responses

  handle_request(req)
}

pub fn default_responses(handle_request: fn() -> wisp.Response) -> wisp.Response {
  let response = handle_request()

  use <- bool.guard(when: response.body != wisp.Empty, return: response)

  case response.status {
    status if status >= 400 -> {
      let error_message = case status {
        400 -> "Bad Request"
        401 -> "Unauthorized"
        403 -> "Forbidden"
        404 -> "Not Found"
        405 -> "Method Not Allowed"
        413 -> "Request Entity Too Large"
        422 -> "Unprocessable Entity"
        429 -> "Too Many Requests"
        500 -> "Internal Server Error"
        501 -> "Not Implemented"
        502 -> "Bad Gateway"
        503 -> "Service Unavailable"
        504 -> "Gateway Timeout"
        _ -> "Unknown Error"
      }

      html.Text(error_message)
      |> nakai.to_string_tree
      |> wisp.html_response(status)
    }
    _ -> response
  }
}
