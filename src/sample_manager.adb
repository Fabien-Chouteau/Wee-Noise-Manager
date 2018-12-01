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

package body Sample_Manager is

   type Sample_Memory_Size is
     range 1 .. Integer (Sample_Size'Last) * Integer (Sample_Block_Size'Last);

   type Sample_Data is record
      Name : Sample_Name := (others => ASCII.NUL);
      Size : Sample_Size := 42;
      First_Block : Sample_Size := 0;
   end record;

   Sample_Memory : array (Sample_Memory_Size) of Unsigned_8;
   Samples : array (Sample_Id) of Sample_Data;

   Last_Used : Sample_Size := 0;

   Now_Recording : Boolean := False;
   Recording_In  : Sample_Id;

   ----------
   -- Name --
   ----------

   function Name (Id : Sample_Id) return Sample_Name
   is (Samples (Id).Name);

   --------------
   -- Set_Name --
   --------------

   procedure Set_Name (Id   : Sample_Id;
                       Name : Sample_Name)
   is
   begin
      Samples (Id).Name := Name;
   end Set_Name;

   ----------
   -- Size --
   ----------

   function Size (Id : Sample_Id) return Sample_Size
   is (Samples (Id).Size);

   ----------------
   -- Read_Block --
   ----------------

   function Read_Block (Id    : Sample_Id;
                        Block : Sample_Block_Id;
                        Data  : out Sample_Block)
                        return Boolean
   is
      Sample : Sample_Data renames Samples (Id);
      Index  : Sample_Memory_Size;
      Offset : Integer;
   begin
      if Sample.Size /= 0 and then Block <= Sample_Block_Id (Sample.Size) then

         Offset := (Integer (Block) - 1) * Integer (Sample_Block_Size'Last);
         Index := Sample_Memory_Size (Integer (Sample.First_Block) + Offset);

         for Elt of Data loop
            Elt := Sample_Memory (Index);
            Index := Index + 1;
         end loop;

         return True;
      else
         return False;
      end if;
   end Read_Block;

   -----------
   -- Empty --
   -----------

   function Empty (Id : Sample_Id) return Boolean
   is (Size (Id) = 0);

   ---------------
   -- Available --
   ---------------

   function Available return Sample_Size
   is (Sample_Size'Last - Last_Used);

   -----------
   -- Erase --
   -----------

   procedure Erase (Id : Sample_Id) is
   begin
      raise Program_Error;
   end Erase;

   ---------------------
   -- Start_Recording --
   ---------------------

   procedure Start_Recording (Id : Sample_Id) is
   begin
      Now_Recording := True;
      Recording_In := Id;

      Samples (Id).First_Block := Last_Used + 1;
   end Start_Recording;

   -------------------
   -- End_Recording --
   -------------------

   procedure End_Recording is
   begin
      Now_Recording := False;
   end End_Recording;

   ---------------
   -- Recording --
   ---------------

   function Recording return Boolean
   is (Now_Recording);

   ----------
   -- Push --
   ----------

   procedure Push (Data : Sample_Block) is
      Index  : Sample_Memory_Size;
      Offset : Integer;
   begin
      Last_Used := Last_Used + 1;

      --  Copy data at Last_Used
      Offset := (Integer (Last_Used) - 1) * Integer (Sample_Block_Size'Last);
      Index := Sample_Memory_Size (1 + Offset);

      for Elt of Data loop
         Sample_Memory (Index) := Elt;
         Index := Index + 1;
      end loop;

      Samples (Recording_In).Size := Samples (Recording_In).Size + 1;
   end Push;

end Sample_Manager;
