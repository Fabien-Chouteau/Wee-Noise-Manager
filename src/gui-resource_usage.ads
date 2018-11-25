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

with Gtk.Box; use Gtk.Box;

private with Gtk.Event_Box;

generic
   type Element_Id is (<>);
package GUI.Resource_Usage is

   subtype Parent_Record is Gtk.Box.Gtk_Box_Record;
   subtype Parent is Gtk.Box.Gtk_Box;

   type Widget_Record is new Parent_Record with private;
   type Widget is access all Widget_Record'Class;

   procedure Gtk_New (Self : out Widget);

   procedure Initialize (Self : not null access Widget_Record'Class);

   procedure Set_Usage (Self  : in out Widget_Record;
                        Id    : Element_Id;
                        Usage : Float);

   type Selected_Callback is access procedure (Id       : Element_Id;
                                               Selected : Boolean);

   procedure Set_Selected_Callback (Self     : in out Widget_Record;
                                    Callback : Selected_Callback);

   procedure Set_Selected (Self     : in out Widget_Record;
                           Id       : Element_Id;
                           Selected : Boolean);

private

   type Event_Box_Array is array (Element_Id) of Gtk.Event_Box.Gtk_Event_Box;
   type Usage_Array is array (Element_Id) of Float;

   type Widget_Record is new Parent_Record with record
      Cells_Box : Gtk.Box.Gtk_Box;
      BG        : Gtk.Event_Box.Gtk_Event_Box;
      Cells : Event_Box_Array := (others => null);
      Usage : Usage_Array := (others => 0.0);

      Callback : Selected_Callback := null;
   end record;

end GUI.Resource_Usage;
