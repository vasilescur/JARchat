with Interfaces.C; use Interfaces.C;
with Interfaces.C.Strings; use Interfaces.C.Strings;

package Crypto is 

subtype Key is String(1..32);

procedure Gen_Keypair (Public : out Key;
                       Priv : out Key);


end Crypto;
