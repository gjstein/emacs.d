// Copyright 2015 <Gregory J Stein>
#include <iostream>
#include "rect.h"
#include <Eigen/Dense>

int main() {
  // Hello!
  std::cout << "Hello World!";

  // Define some testing variables here
  int foobar = 1;
  double foobar_d = 0;

  // Now that the above variables are defined, irony should be working.  You can
  // call 'M-x irony-completion-at-point-async' after typing the first few
  // characters 'foo' here:

  // foo
  
  // Upon running this, a new buffer should open which shows both 'foobar' and
  // 'foobar_d' as options. In addition, running the command 'company-irony'
  // will rely on 'company' to show the completion in place (try it and
  // see). This has the added advantage of showing the type of each of the
  // variables, so that 'foobar' is recognized as being an int and 'foobar_d' as
  // a double.

  // DEGUG: if 'irony' does not work, the command 'M-x irony-install-server' may
  // need to be run.

  // Now we can define a class object from the imported 'rect' class.
  rect square = rect();
  // Not only will the object 'square' complete from 'squ', but once it's
  // completed, running 'company-complete' after 'square.' (notice the period)
  // will load the properties as well. Notice that some of the options are shown
  // multiple times. This is because both 'irony' and 'gtags' are capable of
  // finding the completions. However, 'irony' does not seem to scale to
  // extremely large projects, so 'gtags' becomes necessary.
  // squ
  // square.


  return 0;
}
