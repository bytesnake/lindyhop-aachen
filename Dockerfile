FROM node:lts AS admin
WORKDIR /admin
COPY ./admin/package.json .
RUN npm install
COPY ./admin/elm.json .
# Cache compiled dependencies (inspired by http://whitfin.io/speeding-up-rust-docker-builds/)
RUN mkdir src && echo "import Html\nmain = Html.text \"Hello World\"" >> src/Main.elm
RUN npm run build
RUN rm -r src
# Actual build
COPY ./admin/src ./src
RUN npm run build

FROM rust:latest AS lindyhop-aachen
RUN rustup toolchain install nightly-2019-03-23 && rustup default nightly-2019-03-23
# Cache compiled dependencies (see http://whitfin.io/speeding-up-rust-docker-builds/)
WORKDIR /
RUN USER=root cargo new lindyhop-aachen --bin
WORKDIR /lindyhop-aachen
COPY ./Cargo.toml .
COPY ./Cargo.lock .
RUN cargo build --release
RUN rm ./target/release/deps/lindyhop_aachen*
RUN rm -r ./src
# Actual build
COPY ./src ./src
RUN cargo build --release

FROM rust:slim
WORKDIR /lindyhop-aachen
COPY --from=admin /admin/dist ./admin/dist
COPY --from=lindyhop-aachen /lindyhop-aachen/target/release/lindyhop-aachen .
CMD [ "./lindyhop-aachen" ]