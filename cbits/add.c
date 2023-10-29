#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <unistd.h>
int add(int a, int b){
  int res = a + b;
  printf("C : %d\n", res);
  return res;
}

unsigned char *processByteArray(unsigned char* data, size_t length) {
  printf("Received byte array of length %ld:\n", length);
  for (size_t i = 0; i < length; i++) {
    printf("%02X ", data[i]);
  }
  printf("\n");
  char *len_prefixed_data = (char *)malloc(sizeof(size_t) + length); // the Haskell Program will free this
  memcpy(len_prefixed_data, &length, sizeof(size_t));
  memcpy(len_prefixed_data + sizeof(size_t), data, length);
  for (int i = 0; i < sizeof(size_t) + length; i++) {
    printf("%02X ", len_prefixed_data[i]);
  }
  //free(len_prefixed_data);
  printf("\n");


  return(len_prefixed_data);
  // return(data);
}

char *processByteArrayDyn(char* data) {
  printf("Inside byte array dynamic (length not known) \n");
  printf("%02X ", data[8]); // bytes 0 - 7 hold the array length = 5;
  printf("\n");
  for(int i = 0; i < 19; i++){
    printf("data[%d] = %02X \n", i, data[i]);
  }
  return(data);
}

void test_ref(int *value, char *data) {
  printf("C land : %d\n", *value);
  sleep(5);
  char *str = "hello\0";
  memcpy(data, str, 5);
  *value = 1;
}
