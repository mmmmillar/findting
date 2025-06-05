import gleam/uri
import nakai/html.{type Node}

pub fn content(id: String) -> Node {
  let assert Ok(id) = uri.percent_decode(id)

  html.div([], [
    html.h1([], [html.Text("Hello!")]),
    html.p([], [
      html.Text(
        "Someone has shared their Find Ting ID with you via QR code:" <> id,
      ),
    ]),
  ])
}
