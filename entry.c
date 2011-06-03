int petitml_entry(int a){
  int  b;
  b = 2 ;
  a = a+b ;
  a = toto( a ) ;
  return a;
}

int toto(int a){
  int b;
  b = toto( a) ;
  return a + 1 ;
}
