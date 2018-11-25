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

with Gtk.Box;    use Gtk.Box;
with Glib;       use Glib;

with Gtk.Enums;

package body GUI.Projects is

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New (Self  : out Widget)
   is
   begin
      Self := new Widget_Record;
      GUI.Projects.Initialize (Self);
   end Gtk_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (Self  : not null access Widget_Record'Class)
   is
   begin
      Initialize (Parent (Self),
                  Gtk.Enums.Orientation_Vertical,
                  Spacing => 0);

      Self.Set_Hexpand (True);
      Self.Set_Vexpand (True);

      for Track in Track_Id loop
         GUI.Track.Gtk_New (Self.Tracks (Track), Track);
         Self.Add (Self.Tracks (Track));
      end loop;
   end Initialize;

   ------------
   -- Update --
   ------------

   overriding
   procedure Update (Self : in out Widget_Record) is
   begin
      for T of Self.Tracks loop
         T.Update;
      end loop;
   end Update;

end GUI.Projects;
