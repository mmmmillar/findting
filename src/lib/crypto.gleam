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
  InvalidLengthError
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

  Ok(bit_array.base64_encode(bit_array.concat([nonce, encrypted]), False))
}

pub fn decrypt(key: CipherKey, encrypted: String) -> Result(String, CryptoError) {
  use decoded <- result.try({
    case bit_array.base64_decode(encrypted) {
      Ok(result) -> Ok(result)
      Error(_) -> Error(Base64DecodeError)
    }
  })

  use _ <- result.try({
    case bit_array.byte_size(decoded) < 24 {
      True -> Error(InvalidLengthError)
      False -> Ok(Nil)
    }
  })

  use decrypted <- result.try({
    let assert <<nonce:bytes-size(24), ciphertext:bytes>> = decoded

    case decrypt_ffi(key.data, nonce, ciphertext) {
      Ok(result) -> Ok(result)
      Error(_) -> Error(DecryptError)
    }
  })

  case bit_array.to_string(decrypted) {
    Ok(result) -> Ok(result)
    Error(_) -> Error(FormatError)
  }
}
