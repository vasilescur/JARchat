with Interfaces.C; use Interfaces.C;
with Interfaces.C.Strings; use Interfaces.C.Strings;

with Ada.Text_IO; use Ada.Text_IO;

package body Crypto is

procedure Gen_Keypair (Public : out Key;
                       Priv   : out Key) is
    procedure Gen (pub_out  : in out chars_ptr; 
                   priv_out : in out chars_ptr)
        with 
            Import => True,
            Convention => C,
            External_Name => "gen";

    Gen_Public  : chars_ptr := Null_Ptr;
    Gen_Private : chars_ptr := Null_Ptr;
begin
    Gen (Gen_Public, Gen_Private);

    Public := Value (Gen_Public, 32);
    Priv := Value (Gen_Private, 32);
end Gen_Keypair;

function Encrypt (Message : in String;
                  Public  : in Key) return String is

    function encrypt_str (message : Interfaces.C.Strings.chars_ptr; 
                          pub : Interfaces.C.Strings.chars_ptr) 
                            return Interfaces.C.Strings.chars_ptr
    with Import => True, 
            Convention => C, 
            External_Name => "encrypt_str";

    Message_Str_Ptr : chars_ptr := New_String (Message);
    Pub_Str_Ptr : chars_ptr := New_String (Public);

    Result_Str : chars_ptr := Null_Ptr;
begin
    Result_Str := encrypt_str (Message_Str_Ptr, Pub_Str_Ptr);
    
    return Value (Result_Str);
end Encrypt;

function Decrypt (Message : in String;
                  Priv : in Key) return String is

    function decrypt_str (message : Interfaces.C.Strings.chars_ptr; 
                        priv : Interfaces.C.Strings.chars_ptr) 
                            return Interfaces.C.Strings.chars_ptr
    with Import => True, 
            Convention => C, 
            External_Name => "decrypt_str";

    Message_Str_Ptr : chars_ptr := New_String (Message);
    Priv_Str_Ptr : chars_ptr := New_String (Priv);

    Result_Str : chars_ptr := Null_Ptr;
begin
    Result_Str := decrypt_str (Message_Str_Ptr, Priv_Str_Ptr);
    
    return Value (Result_Str);
end Decrypt;

end Crypto;
