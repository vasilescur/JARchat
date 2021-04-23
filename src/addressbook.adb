with Ada.Sequential_IO;
with Ada.Text_IO;
with Ada.Containers.Indefinite_Hashed_Maps;
with Ada.Strings.Hash;

package body AddressBook is

function Address_Book_CLI return String
is
    Input           : String(1..256);
    Input_Length    : Natural;

    package IP_Hashed_Maps is new
     Ada.Containers.Indefinite_Hashed_Maps
       (Key_Type        => String,
        Element_Type    => String,
        Hash            => Ada.Strings.Hash,
        Equivalent_Keys => "=");
    use IP_Hashed_Maps;

    Address_Map : Map;

   use IP_Hashed_Maps;

    -- procedure Read_File is
    --     package IHM_IO is new Ada.Sequential_IO (IP_Hashed_Maps);
    --     use IHM_IO;
    -- begin
        
    -- end Read_File;

begin
    -- add [name] [ip]
    -- remove [name]
    -- edit [name] [new-ip]
    -- [name] --> returns the associated IP

    -- loop 
        -- get Input
        -- switch on command
        -- use hardcoded file name 

        -- if input does not match commands -- it is a name --> return its IP from this function
    -- end loop

    return "127.0.0.1";

end Address_Book_CLI;

end AddressBook;
