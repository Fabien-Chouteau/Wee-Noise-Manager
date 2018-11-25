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
with Glib;          use Glib;
with Gtk.Widget;    use Gtk.Widget;

package body GUI.Track_Sub_Select is

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New (Self : out Widget;
                      Track : Track_Id)
   is
   begin
      Self := new Widget_Record;
      Track_Sub_Select.Initialize (Self, Track);
   end Gtk_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (Self  : not null access Widget_Record'Class;
                         Track : Track_Id)
   is
   begin
      Initialize (Parent (Self));

      Self.Track := Track;

      for Sub in Track_Sub_Engine_Id loop
         Self.Insert_Text (Gint (Track_Sub_Engine_Id'Pos (Sub)),
                           Sub'Img);
      end loop;

      Self.Set_Active
        (Gint (Track_Sub_Engine_Id'Pos (Track_Sub_Engine_Id'First)));
   end Initialize;

   ------------
   -- Update --
   ------------

   overriding
   procedure Update (Self : in out Widget_Record) is
   begin
      null;
   end Update;

end GUI.Track_Sub_Select;
