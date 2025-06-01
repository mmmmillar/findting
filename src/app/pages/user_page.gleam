import lib/crypto
import nakai/attr
import nakai/html.{type Node}

pub fn content(uuid: String) -> Node {
  let assert Ok(key) = crypto.generate_key()
  let assert Ok(enc) = crypto.encrypt(key, "hello")

  html.div([], [
    html.h1([], [html.Text("Your Personal Page")]),
    html.p([], [html.Text("This is your unique ID: " <> uuid)]),
    html.p([], [html.Text("enc: " <> enc)]),
    html.div([attr.class("qr-code")], [
      html.p([], [html.Text("Scan this QR code to connect with me:")]),
      html.img([
        attr.src("/qrcode/" <> uuid),
        attr.alt("QR Code"),
        attr.width("250"),
        attr.height("250"),
      ]),
    ]),
  ])
}
