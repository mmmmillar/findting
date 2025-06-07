import dot_env/env
import gleam/bit_array
import gleam/result
import gleam/uri
import lib/crypto
import lib/qr

pub type CreateQrError {
  CryptoError(crypto.CryptoError)
  QrGenerationError(qr.QrError)
}

fn get_key() {
  let assert Ok(key_string) = env.get_string("32_BYTE_ENCRYPTION_KEY")
  crypto.CipherKey(key_string |> bit_array.from_string)
}

fn generate_url(ciphertext: String) {
  let assert Ok(base_url) = env.get_string("BASE_URL")
  base_url <> uri.percent_encode(ciphertext)
}

pub fn create_qr_for_id(id: String) -> Result(String, CreateQrError) {
  use ciphertext <- result.try(
    get_key()
    |> crypto.encrypt(id)
    |> result.map_error(fn(e) { CryptoError(e) }),
  )
  generate_url(ciphertext)
  |> qr.generate_qr(qr.High)
  |> result.map_error(fn(e) { QrGenerationError(e) })
}
