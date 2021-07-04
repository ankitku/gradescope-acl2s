FROM atwalter/acl2s_gradescope_autograder

RUN apt-get update && apt-get install -y curl unzip dos2unix && apt-get clean && rm -rf /var/lib/apt/lists/* /tmp/* /var/tmp/*

RUN mkdir -p /autograder 

COPY . /autograder/

RUN apt-get update && apt-get clean && rm -rf /var/lib/apt/lists/* /tmp/* /var/tmp/*

export HOME=/

RUN curl -O https://beta.quicklisp.org/quicklisp.lisp

RUN sbcl --load quicklisp.lisp

WORKDIR /autograder

RUN acl2s < example.lisp