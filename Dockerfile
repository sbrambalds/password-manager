#
# 	Frontend
# 
FROM node:22-slim AS frontend
WORKDIR /app
COPY ./src ./src
COPY package.json     	  package.json
COPY spago.lock   	      spago.lock
COPY spago.yaml     	  spago.yaml
COPY webpack.config.js	  webpack.config.js
COPY package-lock.json	  package-lock.json
COPY clipperz.webmanifest clipperz.webmanifest
COPY --chown=root ./.git ./.git

RUN apt-get update && apt-get install -y --no-install-recommends git ca-certificates && rm -rf /var/lib/apt/lists/*
RUN npm ci
RUN mkdir ./target
RUN npm run build
RUN ls ./src/main/js && npm run package -- --env production

#   
# 	Backend
# 
FROM sbtscala/scala-sbt:eclipse-temurin-21.0.8_9_1.12.11_3.8.3 AS backend
WORKDIR /app
COPY --from=frontend /app/spago.yaml ./spago.yaml
COPY ./ ./
# remove option to remove tests when everything else works
RUN sbt "set assembly / test := {}" clean assembly

#
# 	Deploy
#
FROM eclipse-temurin:26.0.1_8-jre-alpine
WORKDIR /app
COPY --from=frontend /app/target/output.webpack ./target/output.webpack
COPY --from=backend '/app/target/*/*.jar' ./target/clipperz.jar
RUN chmod -R 755 ./ && \
    addgroup --system clipperz && adduser --system clipperz --ingroup clipperz && \
    chown -R clipperz: ./
USER clipperz
ENTRYPOINT [ "java", "-jar", "/app/target/clipperz.jar"]
