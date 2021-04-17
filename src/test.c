#include <stdio.h>
#include "rsa.h"
#include <string.h>
#include <stdlib.h>

int main(int argc, char **argv)
{
    struct key_t *pub = (struct key_t *)malloc(sizeof(struct key_t));
    struct key_t *priv = (struct key_t *)malloc(sizeof(struct key_t));

    rsa_gen_keys(pub, priv);

    char *pub_serialized = serialize_key(pub);
    char *priv_serialized = serialize_key(priv);

    printf("Pub Serialized: %s\nPriv Serialized: %s\n", pub_serialized, priv_serialized);

    char *message = "Hello, world!";

    char *encrypted = encrypt(message, pub);
    printf("Encrypted: %s\n", encrypted);

    char *decrypted = decrypt(encrypted, priv);

    printf("Decrypted: %s\n", decrypted);


    // char *message_encrypted = encrypt(message, pub);

    // struct public_key_class pub[1];
    // struct private_key_class priv[1];
    // rsa_gen_keys(pub, priv);

    // printf("Private Key:\n Modulus: %lld\n Exponent: %lld\n", (long long)priv->modulus, (long long)priv->exponent);
    // printf("Public Key:\n Modulus: %lld\n Exponent: %lld\n", (long long)pub->modulus, (long long)pub->exponent);

    // char message[] = "123abc";
                    // strlen(message) = 6
                    // sizeof(message) = 6 * 8 = 48 bits (6 bytes) + null term = 7 bytes

    // int i;

    // printf("Original:\n");
    // for (i = 0; i < strlen(message); i++)
    // {
    //     printf("%lld\n", (long long)message[i]);
    // }

    // long long *encrypted = rsa_encrypt(message, sizeof(message), pub);

                    // sizeof(encrypted) = sizeof(long long) * message_size
                    //                   = 8 bytes * 7 bytes 
                    // encrypted[0] : long long (size 8 bytes) -- first character of plaintext 

    // if (!encrypted)
    // {
    //     fprintf(stderr, "Error in encryption!\n");
    //     return 1;
    // }
    // printf("Encrypted:\n");
    // for (i = 0; i < strlen(message); i++)
    // {
    //     printf("%lld\n", (long long)encrypted[i]);
    // }

    // char *decrypted = rsa_decrypt(encrypted, 8 * sizeof(message), priv);
    // if (!decrypted)
    // {
    //     fprintf(stderr, "Error in decryption!\n");
    //     return 1;
    // }
    // printf("Decrypted:\n");
    // for (i = 0; i < strlen(message); i++)
    // {
    //     printf("%lld\n", (long long)decrypted[i]);
    // }

    // printf("\n");
    // free(encrypted);
    // free(decrypted);
    // return 0;
}
