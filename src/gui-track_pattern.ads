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

with Gtk.Grid;

with Project_Manager; use Project_Manager;

private with GUI.Step_Enabled;
private with GUI.Step_Condition;

package GUI.Track_Pattern is

   subtype Parent_Record is Gtk.Grid.Gtk_Grid_Record;
   subtype Parent is Gtk.Grid.Gtk_Grid;

   type Widget_Record is new Parent_Record and Updatable_Record with private;
   type Widget is access all Widget_Record'Class;

   procedure Gtk_New (Self : out Widget;
                      Track : Track_Id);

   procedure Initialize (Self  : not null access Widget_Record'Class;
                         Track : Track_Id);

   overriding
   procedure Update (Self : in out Widget_Record);

private

   type Condition_Array is array (Step_Id) of GUI.Step_Condition.Widget;
   type Enabled_Array is array (Step_Id) of GUI.Step_Enabled.Widget;

   type Widget_Record is new Parent_Record and Updatable_Record with record
      Track : Track_Id;

      Conditions : Condition_Array;
      Enabled    : Enabled_Array;
   end record;

end GUI.Track_Pattern;
