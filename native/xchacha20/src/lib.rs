use orion::hazardous::stream::xchacha20;
use rustler::types::atom;
use rustler::{Binary, Encoder, Env, OwnedBinary, Term};

#[rustler::nif(name = "generate_key")]
pub fn generate_key<'a>(env: Env<'a>) -> Term<'a> {
    let key = xchacha20::SecretKey::generate();
    let key_bytes = key.unprotected_as_bytes();

    let mut bin = match OwnedBinary::new(32) {
        Some(b) => b,
        None => return (atom::error(), "Binary allocation failed").encode(env),
    };

    bin.clone_from_slice(key_bytes);
    (atom::ok(), bin.release(env)).encode(env)
}

#[rustler::nif(name = "generate_nonce")]
pub fn generate_nonce<'a>(env: Env<'a>) -> Term<'a> {
    let nonce = xchacha20::Nonce::generate();
    let nonce_bytes = nonce.as_ref();

    let mut bin = match OwnedBinary::new(24) {
        Some(b) => b,
        None => return (atom::error(), "Binary allocation failed").encode(env),
    };

    bin.clone_from_slice(nonce_bytes);
    (atom::ok(), bin.release(env)).encode(env)
}

#[rustler::nif(name = "encrypt")]
pub fn encrypt<'a>(env: Env<'a>, key: Binary, nonce: Binary, plaintext: Binary) -> Term<'a> {
    let sk = match xchacha20::SecretKey::from_slice(&key) {
        Ok(sk) => sk,
        Err(_) => return (atom::error(), "Invalid key length").encode(env),
    };

    let nonce = match xchacha20::Nonce::from_slice(&nonce) {
        Ok(nonce) => nonce,
        Err(_) => return (atom::error(), "Invalid nonce length").encode(env),
    };

    let plaintext_bytes = plaintext.as_slice();
    let mut buff = vec![0u8; plaintext_bytes.len()];

    if let Err(_) = xchacha20::encrypt(&sk, &nonce, 0, plaintext_bytes, &mut buff) {
        return (atom::error(), "Encryption failed").encode(env);
    }

    let mut bin = match OwnedBinary::new(buff.len()) {
        Some(b) => b,
        None => return (atom::error(), "Binary allocation failed").encode(env),
    };

    bin.clone_from_slice(&buff);
    (atom::ok(), bin.release(env)).encode(env)
}

#[rustler::nif(name = "decrypt")]
pub fn decrypt<'a>(env: Env<'a>, key: Binary, nonce: Binary, ciphertext: Binary) -> Term<'a> {
    let sk = match xchacha20::SecretKey::from_slice(&key) {
        Ok(sk) => sk,
        Err(_) => return (atom::error(), "Invalid key length").encode(env),
    };

    let nonce = match xchacha20::Nonce::from_slice(&nonce) {
        Ok(nonce) => nonce,
        Err(_) => return (atom::error(), "Invalid nonce length").encode(env),
    };

    let ciphertext_bytes = ciphertext.as_slice();
    let mut buff = vec![0u8; ciphertext_bytes.len()];

    if let Err(_) = xchacha20::encrypt(&sk, &nonce, 0, ciphertext_bytes, &mut buff) {
        return (atom::error(), "Decryption failed").encode(env);
    }

    let mut bin = match OwnedBinary::new(buff.len()) {
        Some(b) => b,
        None => return (atom::error(), "Binary allocation failed").encode(env),
    };

    bin.clone_from_slice(&buff);
    (atom::ok(), bin.release(env)).encode(env)
}

rustler::init!("xchacha20");
