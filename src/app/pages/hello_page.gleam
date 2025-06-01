import nakai/attr
import nakai/html.{type Node}

pub fn content(from_encrypted_uuid: String) -> Node {
  html.div([], [
    html.h1([], [html.Text("Hello!")]),
    html.p([], [
      html.Text("Someone has shared their Find Ting ID with you via QR code."),
    ]),
    html.p([], [html.Text("Their encrypted ID is: " <> from_encrypted_uuid)]),
    html.a([attr.href("/")], [html.Text("Create your own ID")]),
  ])
}
