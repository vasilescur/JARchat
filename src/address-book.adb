package body AddressBook is

function Address_Book_CLI (IP_Addr : out String) return String
is
    Input           : String(1..256);
    Input_Length    : Natural;
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

end Address_Book_CLI;

end AddressBook;