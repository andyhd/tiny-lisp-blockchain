FROM alpine

WORKDIR /root

ENV HOME /root

RUN apk update && \
    apk add \
        build-base \
        --repository http://dl-3.alpinelinux.org/alpine/edge/testing/ sbcl \
        libev \
        libssl1.0

RUN wget http://beta.quicklisp.org/quicklisp.lisp -O /tmp/install.lisp && \
    sbcl --non-interactive \
         --load /tmp/install.lisp \
         --eval '(quicklisp-quickstart:install)'
ADD sbclrc ./.sbclrc

# Copy all project files and chdir
COPY . /root/quicklisp/local-projects/tiny-lisp-blockchain
COPY run-node.lisp ./

# Install requirements
RUN sbcl --eval "(ql:quickload '(tiny-lisp-blockchain woo))"

EXPOSE 5000

CMD ["--noinform", "--load", "run-node.lisp", "--quit"]
ENTRYPOINT ["sbcl"]
