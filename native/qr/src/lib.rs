use qrcodegen::{QrCode, QrCodeEcc};
use rustler::types::atom;
use rustler::{Binary, Encoder, Env, NifUnitEnum, OwnedBinary, Term};

#[derive(NifUnitEnum)]
pub enum QrSize {
    Low,
    Medium,
    High,
    Quartile,
}

#[rustler::nif(name = "generate_qr")]
pub fn generate_qr<'a>(env: Env<'a>, text: Binary, size: QrSize) -> Term<'a> {
    let input = match std::str::from_utf8(text.as_slice()) {
        Ok(s) => s,
        Err(_) => return (atom::error(), "Invalid UTF-8").encode(env),
    };

    let ecc = match size {
        QrSize::Low => QrCodeEcc::Low,
        QrSize::Medium => QrCodeEcc::Medium,
        QrSize::High => QrCodeEcc::High,
        QrSize::Quartile => QrCodeEcc::Quartile,
    };

    let qr = match QrCode::encode_text(input, ecc) {
        Ok(qr) => qr,
        Err(_) => return (atom::error(), "QR generation failed").encode(env),
    };

    let svg = to_svg_string(&qr, 0);

    let mut bin = match OwnedBinary::new(svg.len()) {
        Some(bin) => bin,
        None => return (atom::error(), "Allocation failed").encode(env),
    };

    bin.as_mut_slice().copy_from_slice(svg.as_bytes());
    (atom::ok(), bin.release(env)).encode(env)
}

fn to_svg_string(qr: &QrCode, border: i32) -> String {
    assert!(border >= 0, "Border must be non-negative");
    let mut result = String::new();
    result += "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n";
    result += "<!DOCTYPE svg PUBLIC \"-//W3C//DTD SVG 1.1//EN\" \"http://www.w3.org/Graphics/SVG/1.1/DTD/svg11.dtd\">\n";
    let dimension = qr
        .size()
        .checked_add(border.checked_mul(2).unwrap())
        .unwrap();
    result += &format!(
        "<svg xmlns=\"http://www.w3.org/2000/svg\" version=\"1.1\" viewBox=\"0 0 {0} {0}\" stroke=\"none\">\n",
        dimension
    );
    result += "\t<rect width=\"100%\" height=\"100%\" fill=\"#FFFFFF\"/>\n";
    result += "\t<path d=\"";
    for y in 0..qr.size() {
        for x in 0..qr.size() {
            if qr.get_module(x, y) {
                if x != 0 || y != 0 {
                    result += " ";
                }
                result += &format!("M{},{}h1v1h-1z", x + border, y + border);
            }
        }
    }
    result += "\" fill=\"#000000\"/>\n";
    result += "</svg>\n";
    result
}

rustler::init!("qr");
