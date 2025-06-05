import gleam/result
import gleam/uri
import lib/crypto
import lib/qr

pub type CreateQrError {
  CryptoError(crypto.CryptoError)
  QrError(qr.QrError)
}

pub fn create_qr_for_id(id: String) -> Result(String, CreateQrError) {
  use key <- result.try(
    crypto.generate_key()
    |> result.map_error(fn(e) { CryptoError(e) }),
  )
  use ciphertext <- result.try(
    crypto.encrypt(key, id)
    |> result.map_error(fn(e) { CryptoError(e) }),
  )
  let url = "http://localhost:8000/hello/" <> uri.percent_encode(ciphertext)
  url
  |> qr.generate_qr(qr.High)
  |> result.map_error(fn(e) { QrError(e) })
}
