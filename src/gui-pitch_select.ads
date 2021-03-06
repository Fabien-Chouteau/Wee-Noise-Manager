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

with Gtk.Combo_Box_Text;

with Project_Manager; use Project_Manager;

package GUI.Pitch_Select is

   package Parent_Package renames Gtk.Combo_Box_Text;
   subtype Parent_Record is Parent_Package.Gtk_Combo_Box_Text_Record;
   subtype Parent is Parent_Package.Gtk_Combo_Box_Text;

   type Widget_Record is new Parent_Record and Updatable_Record with private;
   type Widget is access all Widget_Record'Class;

   procedure Gtk_New (Self : out Widget;
                      Track : Track_Id;
                      Step  : Step_Id;
                      Param : Param_Id);

   procedure Initialize (Self  : not null access Widget_Record'Class;
                         Track : Track_Id;
                         Step  : Step_Id;
                         Param : Param_Id);

   function Value (Self : Widget_Record) return Param_Value;

   overriding
   procedure Update (Self : in out Widget_Record);

private

   type Widget_Record is new Parent_Record and Updatable_Record with record
      Track : Track_Id;
      Step  : Step_Id;
      Param : Param_Id;
      Val   : Param_Value := 0;
   end record;

end GUI.Pitch_Select;
