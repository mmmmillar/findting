build-native:
    cargo build --release --manifest-path native/qr/Cargo.toml
    cp -f native/qr/target/release/libqr.so priv/libqr.so

    cargo build --release --manifest-path native/xchacha20/Cargo.toml
    cp -f native/xchacha20/target/release/libxchacha20.so priv/libxchacha20.so
