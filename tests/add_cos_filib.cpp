#include <interval/interval.hpp>
#include <iostream>

typedef filib::interval<double,filib::native_switched,filib::i_mode_extended> interval;

int main(void) 
{
  interval a;
  interval onetenth;
  filib::fp_traits<double>::setup();

  onetenth = 1.;
  onetenth /= 10.;
  interval::precision(17);
  std::cout << "onetenth = " << onetenth << std::endl;
  
  a = interval(0.0);
  for(int i = 0; i < 100000000; i++) {
    a += cos(a);
  }

  std::cout << "sum = " << a << std::endl;
  return(0);
}
