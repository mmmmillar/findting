import app/pages/hello_page
import app/pages/user_page
import app/web.{type Context}
import gleam/bit_array
import gleam/uri
import nakai
import wisp.{type Request, type Response}
import youid/uuid

pub fn handle_request(req: Request, ctx: Context) -> Response {
  use _req <- web.middleware(req, ctx)

  case wisp.path_segments(req) {
    [] -> {
      let id =
        uuid.v4()
        |> uuid.to_bit_array
        |> bit_array.base64_encode(False)
        |> uri.percent_encode
      wisp.redirect("/user/" <> id)
    }

    ["user", id] -> {
      user_page.content(id)
      |> nakai.to_string_tree
      |> wisp.html_response(200)
    }

    ["hello", id] -> {
      hello_page.content(id)
      |> nakai.to_string_tree
      |> wisp.html_response(200)
    }

    _ -> wisp.not_found()
  }
}
