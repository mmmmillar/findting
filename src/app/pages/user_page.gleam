import app/api
import gleam/uri
import nakai/attr
import nakai/html.{type Node}

pub fn content(id: String) -> Node {
  let assert Ok(id) = uri.percent_decode(id)

  let qr_code = case api.create_qr_for_id(id) {
    Ok(qr) ->
      html.p([attr.style("max-width: 300px; height: auto;")], [
        html.UnsafeInlineHtml(qr),
      ])
    Error(_) ->
      html.p([attr.style("color: red;")], [
        html.Text("Failed to generate QR code"),
      ])
  }

  html.div([], [
    html.h1([], [html.Text("Your Personal Page")]),
    html.p([], [html.Text("This is your unique ID: " <> id)]),
    html.div([attr.class("qr-code")], [
      html.p([], [
        html.Text("Share this QR code to allow people to connect with you:"),
      ]),
      qr_code,
    ]),
  ])
}
