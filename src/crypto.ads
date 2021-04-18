with Interfaces.C; use Interfaces.C;
with Interfaces.C.Strings; use Interfaces.C.Strings;

package Crypto is 

subtype Key is String(1..32);

-- Generate a public/private RSA keypair, placing the results 
-- in the specified string buffers.
procedure Gen_Keypair (Public : out Key;
                       Priv : out Key);

-- Encrypt a cleartext message using RSA encryption with the specified 
-- public key, returning the cyphertext as a hex string.
function Encrypt (Message : in String;
                  Public : in Key) return String;

-- Decrypt a cyphertext message (hex string) using RSA encryption 
-- with the specified private key, returning the cleartext as a string. 
function Decrypt (Message : in String;
                  Priv : in Key) return String;

end Crypto;
