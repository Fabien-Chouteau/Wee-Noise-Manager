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

with Gtk.Dialog;

with Sample_Manager; use Sample_Manager;

private with Gtk.Event_Box;
private with Gtk.Scale;

package GUI.Sample_Edit_Dialog is

   package Parent_Package renames Gtk.Dialog;
   subtype Parent_Record is Parent_Package.Gtk_Dialog_Record;
   subtype Parent is Parent_Package.Gtk_Dialog;

   type Widget_Record is new Parent_Record
   with private;

   type Widget is access all Widget_Record'Class;

   procedure Gtk_New (Self  : out Widget;
                      Id    : Sample_Id);

   procedure Initialize (Self : not null access Widget_Record'Class;
                         Id   : Sample_Id);

   overriding
   procedure Destroy (Self : not null access Widget_Record);

private

   type Sample_Data is array (Sample_Size range <>) of Sample_Block;
   type Sample_Data_Ptr is access all Sample_Data;

   type Widget_Record is new Parent_Record
   with record
      Track : Sample_Id;

      Draw   : Gtk.Event_Box.Gtk_Event_Box;
      Gain   : Gtk.Scale.Gtk_Scale;

      Before : Sample_Data_Ptr := null;
      After  : Sample_Data_Ptr := null;
   end record;

end GUI.Sample_Edit_Dialog;
