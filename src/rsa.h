#ifndef __RSA_H__
#define __RSA_H__

#include <stdint.h>

// Adapted from https://github.com/andrewkiluk/RSA-Library

// This is the header file for the library librsaencrypt.a

struct key_t
{
    long long modulus;
    long long exponent;
};

// This function generates public and private keys, then stores them in the structures you
// provide pointers to.
void rsa_gen_keys(struct key_t *pub, struct key_t *priv);

// This function will encrypt the data pointed to by message. It returns a pointer to a heap
// array containing the encrypted data, or NULL upon failure. This pointer should be freed when
// you are finished. The encrypted data will be 8 times as large as the original data.
long long *rsa_encrypt(const char *message, const unsigned long message_size, const struct key_t *pub);

// This function will decrypt the data pointed to by message. It returns a pointer to a heap
// array containing the decrypted data, or NULL upon failure. This pointer should be freed when
// you are finished. The variable message_size is the size in bytes of the encrypted message.
// The decrypted data will be 1/8th the size of the encrypted data.
char *rsa_decrypt(const long long *message, const unsigned long message_size, const struct key_t *pub);


// Serialize an key into a string
char *serialize_key(const struct key_t *key);

// Parse a string into a key
struct key_t *parse_key(char *input);

// Wrapper functions for string serialized cyphertext
char *encrypt(const char *message, const struct key_t *pub);
char *decrypt(const char *message, const struct key_t *priv);

// Wrapper fns for Ada
void gen(char **pub_out, char **priv_out);
char *encrypt_str(const char *message, const char *pub);
char *decrypt_str(const char *message, const char *priv);

#endif
