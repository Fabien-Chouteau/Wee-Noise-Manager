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

with Audio;

package Sample_Manager is

   type Sample_Id is range 0 .. 99;

   subtype Sample_Name_Size is Integer range 0 .. 15;
   subtype Sample_Name is String (1 .. Sample_Name_Size'Last);

   subtype Sample_Block_Size is Natural range 1 .. 512;
   subtype Sample_Block is Audio.Mono_Buffer (Sample_Block_Size);
   type Sample_Size is range 0 .. 8_192;
   subtype Sample_Block_Id is Sample_Size range 1 .. Sample_Size'Last;

   function Name (Id : Sample_Id) return Sample_Name;

   procedure Set_Name (Id   : Sample_Id;
                       Name : Sample_Name);

   function Size (Id : Sample_Id) return Sample_Size;

   function Read_Block (Id    : Sample_Id;
                        Block : Sample_Block_Id;
                        Data  : out Sample_Block)
                        return Boolean;
   --  Return False if the block doesn't exist

   function Empty (Id : Sample_Id) return Boolean;

   function Available return Sample_Size;

   procedure Erase (Id : Sample_Id)
     with Post => Empty (Id)
                    and then
                  Available = Available'Old - Size (Id)'Old;

   -- Recording --

   procedure Start_Recording (Id : Sample_Id)
     with Pre => not Recording and then Empty (Id) and then Available > 0;

   procedure End_Recording
     with Pre  => Recording,
          Post => not Recording;

   function Recording return Boolean;
   --  Red Means Recording...

   procedure Push (Data : Sample_Block)
     with Pre  => Recording,
          Post => Available = Available'Old - 1;

end Sample_Manager;
