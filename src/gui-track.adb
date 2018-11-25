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

with Gtk.Box;       use Gtk.Box;
with Glib;          use Glib;
with Gtk.Widget;    use Gtk.Widget;

with Gtk.Enums;

package body GUI.Track is

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New (Self  : out Widget;
                      Track : Track_Id)
   is
   begin
      Self := new Widget_Record;
      GUI.Track.Initialize (Self, Track);
   end Gtk_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (Self  : not null access Widget_Record'Class;
                         Track : Track_Id)
   is
   begin
      Initialize (Parent (Self),
                  Gtk.Enums.Orientation_Horizontal,
                  Spacing => 0);

      Self.Track := Track;

      Self.Set_Hexpand (True);
      Self.Set_Vexpand (True);

      GUI.Track_Config.Gtk_New (Self.Config, Track);
      Self.Add (Self.Config);

      GUI.Track_Sequence.Gtk_New (Self.Sequence, Track);
      Self.Add (Self.Sequence);

   end Initialize;

   ------------
   -- Update --
   ------------

   overriding
   procedure Update (Self : in out Widget_Record) is
   begin
      Self.Config.Update;
      Self.Sequence.Update;
   end Update;

end GUI.Track;
