# gradescope-acl2s
A library to automate grading of ACL2s forms on Gradescope. See example.lisp on how to create autogradeable tests.
Autograder specifications are available [here](https://gradescope-autograders.readthedocs.io/en/latest/specs/). 
To create an executable "run_autograder" for testing, simply run

```
acl2s < example.lisp
```

To create an autograder docker image, a Dockerfile is provided

```
docker build -t mytest .
docker tag mytest my_dockerhub_userid/mytest
docker push my_dockerhub_userid/mytest
```

Finally, set your Gradescope test to use  my_dockerhub_userid/mytest docker image
