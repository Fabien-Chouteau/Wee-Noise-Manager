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

with Gtk.Combo_Box_Text; use Gtk.Combo_Box_Text;
with Gtk.Combo_Box;      use Gtk.Combo_Box;
with Glib;               use Glib;
with Gtk.Widget;         use Gtk.Widget;
with Engine_Manager;     use Engine_Manager;

package body GUI.Track_Engine_Select is

   procedure On_Change (W : access Gtk_Combo_Box_Record'Class);

   ---------------
   -- On_Change --
   ---------------

   procedure On_Change (W : access Gtk_Combo_Box_Record'Class) is
      Self   : constant Widget := Widget (W);
      Config : constant Gtk.Widget.Gtk_Widget := Self.Get_Parent;
      Track  : constant Reconfigurable := Reconfigurable (Config.Get_Parent);
   begin
      if not Self.In_Reconfig and then Self.Get_Active /= -1 then
         Change_Engine (Self.Track,
                        Track_Engine_Kind'Val (Self.Get_Active));
         if Track /= null then
            Track.Reconfigure;
         end if;
      end if;
   end On_Change;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New (Self : out Widget;
                      Track : Track_Id)
   is
   begin
      Self := new Widget_Record;
      Track_Engine_Select.Initialize (Self, Track);
   end Gtk_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (Self  : not null access Widget_Record'Class;
                         Track : Track_Id)
   is
   begin
      Gtk.Combo_Box_Text.Initialize (Parent (Self));

      Self.On_Changed (On_Change'Access);

      Self.Track := Track;

      for Engine in Track_Engine_Kind loop
         Self.Insert_Text (Gint (Track_Engine_Kind'Pos (Engine)),
                           Img (Engine));
      end loop;
   end Initialize;

   ------------
   -- Update --
   ------------

   overriding
   procedure Update (Self : in out Widget_Record) is
   begin
      null;
   end Update;

   -----------------
   -- Reconfigure --
   -----------------

   overriding
   procedure Reconfigure (Self : in out Widget_Record) is
      Engine : constant Track_Engine_Kind := Current_Engine (Self.Track);
   begin
      Self.In_Reconfig := True;
      Self.Set_Active (Gint (Track_Engine_Kind'Pos (Engine)));
      Self.In_Reconfig := False;
   end Reconfigure;

end GUI.Track_Engine_Select;
