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

with Gtk.Widget;
with Gtk.Dialog;                use Gtk.Dialog;
with Gtk.Message_Dialog;        use Gtk.Message_Dialog;
with Gtk.File_Chooser;          use Gtk.File_Chooser;
with Gtk.File_Chooser_Dialog;   use Gtk.File_Chooser_Dialog;

with GUI.Sample_Edit_Dialog;
with Sample_Manager;            use Sample_Manager;
with Audio_Interface;           use Audio_Interface;

with GNAT.Directory_Operations;
with Ada.Text_IO;               use Ada.Text_IO;
with Ada.Streams.Stream_IO;     use Ada.Streams.Stream_IO;
with Ada.Strings.Fixed;         use Ada.Strings.Fixed;

package body GUI.Sample_Actions is

   package GDO renames GNAT.Directory_Operations;

   function Erase_Or_Cancel (Id : Sample_Id) return Gtk_Response_Type;
   function Create_Sample_Name (Filename : String) return Sample_Name;

   ---------------------
   -- Erase_Or_Cancel --
   ---------------------

   function Erase_Or_Cancel (Id : Sample_Id) return Gtk_Response_Type
   is
      Confirm : Gtk.Message_Dialog.Gtk_Message_Dialog;
      Ret : Gtk_Response_Type;
   begin
      Gtk.Message_Dialog.Gtk_New
        (Confirm, null, 0,
         Message_Question, Buttons_Ok_Cancel,
         "Do you want to erase the current sample?");

      Ret := Confirm.Run;
      Confirm.Destroy;

      if Ret = Gtk_Response_OK then
         Sample_Manager.Erase (Id);
      end if;

      return Ret;
   end Erase_Or_Cancel;

   ------------------------
   -- Create_Sample_Name --
   ------------------------

   function Create_Sample_Name (Filename : String) return Sample_Name is
      Ext       : constant String := GDO.File_Extension (Filename);
      Base_Name : constant String := GDO.Base_Name (Filename, Ext);
   begin
      return Head (Base_Name,
                   Natural (Sample_Manager.Sample_Name_Size'Last),
                   ASCII.NUL);
   end Create_Sample_Name;

   -----------------
   -- Load_Sample --
   -----------------

   procedure Load_Sample (Id : Sample_Manager.Sample_Id)
   is

      Diag : Gtk_File_Chooser_Dialog;

      Unused : Gtk.Widget.Gtk_Widget;
   begin

      if not Empty (Id)
        and then
          Erase_Or_Cancel (Id) = Gtk_Response_Cancel
      then
         return;
      end if;

      Gtk_New (Diag, "Load sample", null, Action_Open);

      Unused := Diag.Add_Button ("Load", Gtk_Response_OK);
      Unused := Diag.Add_Button ("Cancel", Gtk_Response_Cancel);

      if Diag.Run = Gtk.Dialog.Gtk_Response_OK then
         declare
            Filename : constant String := Diag.Get_Filename;
            Input    : Ada.Streams.Stream_IO.File_Type;
            Buffer   : Sample_Block;
         begin

            Open (Input, In_File, Filename);

            Sample_Manager.Start_Recording (Id);

            while Sample_Manager.Available > 0 loop
               Buffer := Sample_Block'Input (Stream (Input));
               Sample_Manager.Push (Buffer);
            end loop;

            Close (Input);

         exception
            when Ada.Streams.Stream_IO.End_Error =>
               if Is_Open (Input) then
                  Close (Input);
               end if;
         end;

         if Sample_Manager.Recording then
            Sample_Manager.End_Recording;

            Sample_Manager.Set_Name
              (Id, Create_Sample_Name (Diag.Get_Filename));
         end if;
      end if;

      Diag.Destroy;
   end Load_Sample;

   ------------------
   -- Play_Preview --
   ------------------

   procedure Play_Preview (Id : Sample_Id) is
      Block    : Sample_Block;
      BlocK_Id : Sample_Block_Id := Sample_Block_Id'First;
   begin
      Ada.Text_IO.Put_Line ("Sample preview:" & Id'Img);
      Audio_Interface.Sample_Preview_Flush;
      while Sample_Manager.Read_Block (Id, BlocK_Id, Block) loop
         Audio_Interface.Push_Sample_Preview (Block);
         BlocK_Id := BlocK_Id + 1;
      end loop;
      Ada.Text_IO.Put_Line ("End sample preview");
   end Play_Preview;

   ----------
   -- Edit --
   ----------

   procedure Edit (Id            : Sample_Manager.Sample_Id;
                   Parent_Window : Gtk.Window.Gtk_Window)
   is
      Diag : GUI.Sample_Edit_Dialog.Widget;
   begin
      GUI.Sample_Edit_Dialog.Gtk_New (Diag, Parent_Window, Id);

      if Diag.Run = Gtk.Dialog.Gtk_Response_Apply then
         Ada.Text_IO.Put_Line ("Edit: erase");
         Sample_Manager.Erase (Id);

         Ada.Text_IO.Put_Line ("Edit: start recording");
         Sample_Manager.Start_Recording (Id);

         for Block of Diag.Modified_Sample.all loop
            Sample_Manager.Push (Block);
         end loop;
         Ada.Text_IO.Put_Line ("Edit: end recording");
         Sample_Manager.End_Recording;
      end if;

      Diag.Destroy;

   end Edit;

end GUI.Sample_Actions;
