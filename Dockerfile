FROM rust:1.64 as builder
WORKDIR /usr/src/waypoint
COPY src ./src
COPY Cargo.toml Cargo.lock ./
RUN cargo install --path .

FROM debian:buster-slim
# RUN apt-get update && apt-get install -y extra-runtime-dependencies && rm -rf /var/lib/apt/lists/*
COPY --from=builder /usr/local/cargo/bin/waypoint /usr/local/bin/waypoint
ENV RUST_LOG="info,waypoint=debug"
ENTRYPOINT ["waypoint"]