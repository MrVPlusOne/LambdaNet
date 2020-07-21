FROM openjdk:11
#FROM ubuntu:latest

WORKDIR /usr/src/lambdanet

# instsll typescript dependencies
RUN curl -sL https://deb.nodesource.com/setup_14.x | bash -
RUN apt-get install -y nodejs
RUN npm install -g typescript@3.8.2 cloc
RUN npm i fs source-map-support yargs@13.3.2 typescript@3.8.2
RUN npm i @types/node @types/source-map-support @types/yargs


# install scala dependencies
RUN apt-get install -y zip unzip

RUN echo "deb https://dl.bintray.com/sbt/debian /" | tee -a /etc/apt/sources.list.d/sbt.list
RUN curl -sL "https://keyserver.ubuntu.com/pks/lookup?op=get&search=0x2EE0EA64E40A89B84B2DF73499E82A75642AC823" | apt-key add
RUN apt-get update
RUN apt-get install sbt


# Prepare sbt (warm cache)
WORKDIR /usr/src/sbt-temp

# Env variables
ARG SCALA_VERSION
ENV SCALA_VERSION ${SCALA_VERSION:-2.12.10}
ARG SBT_VERSION
ENV SBT_VERSION ${SBT_VERSION:-1.3.13}

RUN \
  sbt sbtVersion && \
  mkdir -p project && \
  echo "scalaVersion := \"${SCALA_VERSION}\"" > build.sbt && \
  echo "sbt.version=${SBT_VERSION}" > project/build.properties && \
  echo "case object Temp" > Temp.scala && \
  sbt compile && \
  rm -r project && rm build.sbt && rm Temp.scala && rm -r target


WORKDIR /usr/src/lambdanet

# Copy all files
COPY . .

# Compile TS scripts
WORKDIR scripts/ts
RUN tsc || echo "tsc errors encountered."
WORKDIR /usr/src/lambdanet

# Compile Scala files
RUN sbt compile

CMD ["/bin/sh"]
