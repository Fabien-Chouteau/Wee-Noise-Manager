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
with Gtk.Dialog;              use Gtk.Dialog;
with Gtk.Message_Dialog;      use Gtk.Message_Dialog;
with Gtk.File_Chooser;        use Gtk.File_Chooser;
with Gtk.File_Chooser_Dialog; use Gtk.File_Chooser_Dialog;

with GUI.Sample_Edit_Dialog;
with Sample_Manager;          use Sample_Manager;
with Audio_Interface;         use Audio_Interface;

with Ada.Text_IO;             use Ada.Text_IO;
with Ada.Streams.Stream_IO;   use Ada.Streams.Stream_IO;

package body GUI.Sample_Actions is

   function Erase_Or_Cancel (Id : Sample_Id) return Gtk_Response_Type;

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
         --  Erase the sample
         Ada.Text_IO.Put_Line ("Erase Sample " & Id'Img);
         --  Erase (Id);
      end if;

      return Ret;
   end Erase_Or_Cancel;

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

      Ada.Text_IO.Put_Line ("Load sample" & Id'Img);

      Gtk_New (Diag, "Load sample", null, Action_Open);

      Unused := Diag.Add_Button ("Load", Gtk_Response_OK);
      Unused := Diag.Add_Button ("Cancel", Gtk_Response_Cancel);

      if Diag.Run = Gtk.Dialog.Gtk_Response_OK then
         Ada.Text_IO.Put_Line ("File open -> " & Diag.Get_Filename);
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

            Sample_Manager.End_Recording;
            Close (Input);

         exception
            when Ada.Streams.Stream_IO.End_Error =>
               Sample_Manager.End_Recording;
               if Is_Open (Input) then
                  Close (Input);
               end if;
         end;
         Ada.Text_IO.Put_Line ("Sample Size (" & Id'Img & ") = " &
                                 Sample_Manager.Size (Id)'Img);
      else
         Ada.Text_IO.Put_Line ("File open -> Canceled");
      end if;

      Diag.Destroy;
   end Load_Sample;

   ------------------
   -- Play_Preview --
   ------------------

   procedure Play_Preview (Id : Sample_Id) is
      Block    : Sample_Block;
      BlocK_Id : Sample_Block_Id := Sample_Block_Id'First;
      Stereo   : Stereo_Buffer (Block'Range);
   begin
      Ada.Text_IO.Put_Line ("Sample preview:" & Id'Img);
      while Sample_Manager.Read_Block (Id, BlocK_Id, Block) loop

         for Index in Block'Range loop
            Stereo (Index) := (Block (Index), Block (Index));
         end loop;

         Audio_Interface.Send (Stereo);
         BlocK_Id := BlocK_Id + 1;
      end loop;

      Stereo := (others => (0, 0));
      Audio_Interface.Send (Stereo);

      Ada.Text_IO.Put_Line ("End sample preview");
   end Play_Preview;

   ----------
   -- Edit --
   ----------

   procedure Edit (Id : Sample_Manager.Sample_Id) is
      Diag : GUI.Sample_Edit_Dialog.Widget;
   begin
      GUI.Sample_Edit_Dialog.Gtk_New (Diag, Id);

      if Diag.Run = Gtk.Dialog.Gtk_Response_Apply then
         raise Program_Error with "Not implemented";
      end if;

      Diag.Destroy;

   end Edit;

end GUI.Sample_Actions;
