#ifndef _3DES_
#define _3DES_

extern void handleErrors(void);

extern int encrypt(unsigned char *plaintext, int plaintext_len, unsigned char *key,
  unsigned char *iv, unsigned char *ciphertext);


extern int decrypt(unsigned char *ciphertext, int ciphertext_len, unsigned char *key,
  unsigned char *iv, unsigned char *plaintext);

extern void initialize_3des();

#endif
