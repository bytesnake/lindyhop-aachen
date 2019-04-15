FROM node:lts
WORKDIR /admin
COPY admin .
RUN npm install && npm build

FROM rust:latest
WORKDIR /lindyhop-aachen
COPY . .
RUN cargo build --release

FROM alpine:latest
WORKDIR /lindyhop-aachen
COPY --from=0 /admin ./admin/dist
COPY --from=1 /lindyhop-aachen/target/release/lindyhop-aachen .
CMD [ "./lindyhop-aachen" ]