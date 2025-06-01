import app/pages/user_page
import app/web.{type Context}
import ids/uuid
import nakai
import wisp.{type Request, type Response}

pub fn handle_request(req: Request, ctx: Context) -> Response {
  use _req <- web.middleware(req, ctx)

  case wisp.path_segments(req) {
    [] -> {
      let assert Ok(id) = uuid.generate_v4()
      wisp.redirect("/user/" <> id)
    }

    ["user", id] -> {
      user_page.content(id)
      |> nakai.to_string_tree
      |> wisp.html_response(200)
    }

    _ -> wisp.not_found()
  }
}
