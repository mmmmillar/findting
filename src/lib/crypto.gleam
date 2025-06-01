import gleam/bit_array
import gleam/result

@external(erlang, "xchacha20", "generate_key")
fn generate_key_ffi() -> Result(BitArray, String)

@external(erlang, "xchacha20", "generate_nonce")
fn generate_nonce_ffi() -> Result(BitArray, String)

@external(erlang, "xchacha20", "encrypt")
fn encrypt_ffi(
  key: BitArray,
  iv: BitArray,
  plaintext: BitArray,
) -> Result(BitArray, String)

@external(erlang, "xchacha20", "decrypt")
fn decrypt_ffi(
  key: BitArray,
  iv: BitArray,
  ciphertext: BitArray,
) -> Result(BitArray, String)

pub type CipherKey {
  CipherKey(data: BitArray)
}

pub type Nonce {
  Nonce(data: BitArray)
}

pub type CryptoError {
  GenerateKeyError
  GenerateNonceError
  EncryptError
  Base64DecodeError
  DecryptError
  FormatError
}

pub fn generate_key() -> Result(CipherKey, CryptoError) {
  case generate_key_ffi() {
    Ok(result) -> Ok(CipherKey(result))
    Error(_) -> Error(GenerateKeyError)
  }
}

pub fn encrypt(key: CipherKey, plaintext: String) -> Result(String, CryptoError) {
  use nonce <- result.try({
    case generate_nonce_ffi() {
      Ok(result) -> Ok(result)
      Error(_) -> Error(GenerateNonceError)
    }
  })

  use encrypted <- result.try({
    case encrypt_ffi(key.data, nonce, bit_array.from_string(plaintext)) {
      Ok(result) -> Ok(result)
      Error(_) -> Error(EncryptError)
    }
  })

  Ok(bit_array.base64_encode(encrypted, False))
}

pub fn decrypt(
  key: CipherKey,
  ciphertext: String,
) -> Result(String, CryptoError) {
  use nonce <- result.try({
    case generate_nonce_ffi() {
      Ok(result) -> Ok(result)
      Error(_) -> Error(GenerateNonceError)
    }
  })

  use decoded <- result.try({
    case bit_array.base64_decode(ciphertext) {
      Ok(result) -> Ok(result)
      Error(_) -> Error(Base64DecodeError)
    }
  })

  use decrypted <- result.try({
    case decrypt_ffi(key.data, nonce, decoded) {
      Ok(result) -> Ok(result)
      Error(_) -> Error(DecryptError)
    }
  })

  case bit_array.to_string(decrypted) {
    Ok(result) -> Ok(result)
    Error(_) -> Error(FormatError)
  }
}
