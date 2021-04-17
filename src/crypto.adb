with Interfaces.C; use Interfaces.C;
with Interfaces.C.Strings; use Interfaces.C.Strings;

with Ada.Text_IO; use Ada.Text_IO;

package body Crypto is

procedure Gen_Keypair (Public : out Key;
                       Priv : out Key) is
    procedure Gen (pub_out : in out chars_ptr; 
                   priv_out : in out chars_ptr)
        with 
            Import => True,
            Convention => C,
            External_Name => "gen";

    Gen_Public : chars_ptr := Null_Ptr;
    Gen_Private : chars_ptr := Null_Ptr;
begin
    Gen (Gen_Public, Gen_Private);

    Public := Value (Gen_Public, 32);
    Priv := Value (Gen_Private, 32);

    Put_Line ("Just generated (" & Public & ", " & Priv & ")");
end Gen_Keypair;

end Crypto;
