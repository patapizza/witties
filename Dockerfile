FROM clojure
MAINTAINER Julien Odent <julien@wit.ai>

ADD . /app
WORKDIR /app
RUN lein uberjar && cp target/*standalone*.jar app.jar

CMD ["java", "-jar", "app.jar"]