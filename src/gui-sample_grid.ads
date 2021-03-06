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

with Gtk.Scrolled_Window;

with Sample_Manager;

private with Gtk.Grid;
private with GUI.Sample_Grid_Cell;

package GUI.Sample_Grid is

   subtype Sample_Id is Sample_Manager.Sample_Id;

   subtype Parent_Record is Gtk.Scrolled_Window.Gtk_Scrolled_Window_Record;
   subtype Parent is Gtk.Scrolled_Window.Gtk_Scrolled_Window;

   type Widget_Record is new Parent_Record with private;
   type Widget is access all Widget_Record'Class;

   procedure Gtk_New (Self : out Widget);

   procedure Initialize (Self : not null access Widget_Record'Class);

private

   type Grid_Array is array (Sample_Id) of GUI.Sample_Grid_Cell.Widget;

   type Widget_Record is new Parent_Record with record
      Cells : Grid_Array := (others => null);
      Cells_Container : Gtk.Grid.Gtk_Grid;
   end record;

end GUI.Sample_Grid;
