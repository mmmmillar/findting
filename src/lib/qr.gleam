import gleam/bit_array
import gleam/result

@external(erlang, "qr", "generate_qr")
fn generate_qr_ffi(
  text_to_encode: BitArray,
  size: QrSize,
) -> Result(BitArray, String)

pub type QrSize {
  Low
  Medium
  High
  Quartile
}

pub type QrError {
  GenerateQrError
  FormatError
}

pub fn generate_qr(
  text_to_encode: String,
  size: QrSize,
) -> Result(String, QrError) {
  use qr <- result.try({
    case generate_qr_ffi(bit_array.from_string(text_to_encode), size) {
      Ok(result) -> Ok(result)
      Error(error) -> {
        echo error
        Error(GenerateQrError)
      }
    }
  })

  case bit_array.to_string(qr) {
    Ok(result) -> Ok(result)
    Error(_) -> Error(FormatError)
  }
}
