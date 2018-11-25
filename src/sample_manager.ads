-------------------------------------------------------------------------------
--                                                                           --
--                             Wee Noise Manager                             --
--                                                                           --
--                     Copyright (C) 2018 Fabien Chouteau                    --
--                                                                           --
--    Wee Noise Manager is free software: you can redistribute it and/or     --
--    modify it under the terms of the GNU General Public License as         --
--    published by the Free Software Foundation, either version 3 of the     --
--    License, or (at your option) any later version.                        --
--                                                                           --
--    Wee Noise Manager is distributed in the hope that it will be useful,   --
--    but WITHOUT ANY WARRANTY; without even the implied warranty of         --
--    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU       --
--    General Public License for more details.                               --
--                                                                           --
--    You should have received a copy of the GNU General Public License      --
--    along with Wee Noise Manager.                                          --
--    If not, see <http://www.gnu.org/licenses/>.                            --
--                                                                           --
-------------------------------------------------------------------------------

with Interfaces; use Interfaces;

package Sample_Manager is

   type Sample_Id is range 0 .. 99;

   subtype Sample_Name_Size is Integer range 0 .. 15;
   subtype Sample_Name is String (1 .. Sample_Name_Size'Last);

   type Sample_Block_Size is range 1 .. 512;
   type Sample_Block is array (Sample_Block_Size) of Unsigned_8;
   type Sample_Block_Id is range 1 .. 127;
   type Sample_Size is range 0 .. Sample_Block_Id'Last;

   function Name (Id : Sample_Id) return Sample_Name;

   procedure Set_Name (Id   : Sample_Id;
                       Name : Sample_Name);

   function Size (Id : Sample_Id) return Sample_Size;

   function Read_Block (Id    : Sample_Id;
                        Block : Sample_Block_Id;
                        Data  : out Sample_Block)
                        return Boolean;
   --  Return False if the block doesn't exist

private

   type Sample_Memory_Size is
     range 1 .. Integer (Sample_Size'Last) * Integer (Sample_Block_Size'Last);

   type Sample_Data is record
      Name : Sample_Name := (others => ASCII.NUL);
      Size : Sample_Size := 0;
      First_Block : Sample_Memory_Size := 1;
   end record;

   Sample_Memory : array (Sample_Memory_Size) of Unsigned_8;
   Samples : array (Sample_Id) of Sample_Data;

end Sample_Manager;
